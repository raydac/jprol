package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.jsr223.JProlScriptEngineUtils.java2term;
import static javax.script.ScriptContext.ENGINE_SCOPE;

import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import java.io.StringReader;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

/**
 * {@link javax.script.CompiledScript} for JProl: at compile time, non-query phrases are consulted into the
 * {@linkplain JProlContext context} of the compiling thread; the first {@code ?- Goal.} is stored for repeated
 * {@linkplain #eval(ScriptContext) evaluation}. Closing this object does <b>not</b> dispose that shared {@link JProlContext}
 * (the owning {@link JProlScriptEngine} remains responsible).
 *
 * @since 3.0.0
 */
public class JProlCompiledScript extends CompiledScript
    implements Disposable, CanGC, Invocable, AutoCloseable, JProlScriptEngineProvider {

  private final JProlScriptEngine engine;
  private final Term query;
  private final String scriptSources;
  private final JProlContext compiledProlContext;

  private final AtomicBoolean disposed = new AtomicBoolean();

  JProlCompiledScript(
      final JProlScriptEngine engine,
      final String script) throws ScriptException {
    this.engine = engine;
    this.scriptSources = script;

    try {
      this.compiledProlContext =
          JProlScriptEngineUtils.asJProlContext(engine.getContext()).findOrMakeJProlContext();

      final List<Term> parsed =
          JProlScriptEngine.parseWholeScript(script, this.compiledProlContext);
      this.query = parsed.stream().map(JProlScriptEngine.QUERY_PREDICATE_FILTER)
          .filter(Objects::nonNull)
          .findFirst()
          .orElse(null);

      final String scriptWithoutQuery =
          JProlScriptEngine.joinSources(parsed, JProlScriptEngine.NOT_QUERY_PREDICATE_FILTER,
              Integer.MAX_VALUE,
              Map.of());

      this.compiledProlContext.consult(new StringReader(scriptWithoutQuery));
    } catch (PrologParserException e) {
      if (e.hasValidPosition()) {
        throw new ScriptException(
            "Error parsing script (" + e.getMessage() + ") " + e.getLine() + ':' + e.getPos() +
                " : " + script);
      } else {
        throw new ScriptException("Error parsing script (" + e.getMessage() + "): " + script);
      }
    } catch (Exception e) {
      throw new ScriptException("Error compiling Prolog script: " + e.getMessage());
    }
  }

  private void assertNotDisposed() {
    if (this.disposed.get()) {
      throw new IllegalStateException("Closed context");
    }
  }

  /**
   * {@link JProlContext} used at compile time (the same instance the {@linkplain #getEngine() engine} used on that thread).
   */
  public JProlContext getCompiledContext() {
    this.assertNotDisposed();
    return this.compiledProlContext;
  }

  /**
   * Query found in pre-compiled source, or {@code null} if the script had no {@code ?-} phrase.
   *
   * @return first script query or {@code null}
   */
  public Term getQuery() {
    this.assertNotDisposed();
    return this.query;
  }

  /**
   * Get script sources for the compilation.
   *
   * @return sources of the script as String
   */
  public String getScriptSources() {
    this.assertNotDisposed();
    return this.scriptSources;
  }

  /**
   * Proves the compiled query once using {@code context} for variable bindings and {@link ScriptContext#ENGINE_SCOPE}
   * for output bindings.
   *
   * @param context script context (must be a {@link JProlScriptEngineContext} for binding resolution)
   * @return {@link Boolean#TRUE} if the goal succeeds, {@link Boolean#FALSE} otherwise
   * @throws ScriptException if evaluation fails
   */
  @Override
  public Object eval(final ScriptContext context) throws ScriptException {
    this.assertNotDisposed();

    if (this.query == null) {
      throw new IllegalStateException(
          "There is not any pre-compiled query because it was not provided in source script");
    }

    try {
      final Map<String, Term> bindings = JProlScriptEngine.extractVarsFromBindings(context, null);
      Term preparedQuery = this.query.makeClone();
      if (!bindings.isEmpty()) {
        for (final Map.Entry<String, Term> t : bindings.entrySet()) {
          preparedQuery = preparedQuery.replaceVar(t.getKey(), t.getValue());
        }
      }
      return this.engine.proveQueryOnce(preparedQuery, this.compiledProlContext,
          context.getBindings(ENGINE_SCOPE));
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ScriptEngine getEngine() {
    this.assertNotDisposed();
    return this.engine;
  }

  /**
   * Proves {@code name(Args...)} on the {@linkplain #getCompiledContext() compiled} {@link JProlContext};
   * {@code thisObject} must be a {@link JProlScriptEngineProvider}.
   */
  @Override
  public Object invokeMethod(final Object thisObject, final String name, final Object... args) {
    this.assertNotDisposed();
    if (thisObject instanceof JProlScriptEngineProvider) {
      final JProlScriptEngine thisEngine =
          ((JProlScriptEngineProvider) thisObject).getJProlScriptEngine();
      final Term[] terms = new Term[args.length];
      for (int i = 0; i < args.length; i++) {
        terms[i] = java2term(args[i]);
      }
      final Term term = Terms.newStruct(name, terms, SourcePosition.UNKNOWN);
      return thisEngine.proveQueryOnce(term, this.compiledProlContext,
          thisEngine.getBindings(ENGINE_SCOPE));
    } else {
      throw new IllegalArgumentException(
          "Expected " + JProlScriptEngineProvider.class.getCanonicalName() + " instance");
    }
  }

  /**
   * Delegates to {@link #invokeMethod(Object, String, Object...)} with {@code this} as {@code thisObject}.
   */
  @Override
  public Object invokeFunction(final String name, final Object... args) {
    return this.invokeMethod(this, name, args);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public JProlScriptEngine getJProlScriptEngine() {
    return this.engine;
  }

  /**
   * {@inheritDoc}
   * <p>For compiled scripts, supported types from {@link #getCompiledContext()} include {@link JProlContext},
   * {@link ParserContext}, and {@link KnowledgeBase}; {@link JProlScriptEngine} is resolved from the provider.
   */
  @Override
  public <T> T getInterface(final Class<T> targetClass) {
    return this.getInterface(this.engine, targetClass);
  }

  /**
   * {@inheritDoc}
   * <p>Same supported types as {@link #getInterface(Class)}; {@code thisObject} must be a {@link JProlScriptEngineProvider}.
   */
  @SuppressWarnings("unchecked")
  @Override
  public <T> T getInterface(final Object thisObject, final Class<T> targetClass) {
    this.assertNotDisposed();

    if (thisObject instanceof JProlScriptEngineProvider) {
      if (targetClass == null) {
        throw new NullPointerException("Class must not be null");
      }

      final JProlScriptEngine engine =
          ((JProlScriptEngineProvider) thisObject).getJProlScriptEngine();

      final T result;
      if (targetClass.isAssignableFrom(JProlScriptEngine.class)) {
        result = (T) engine;
      } else {
        final JProlContext prolContext = this.compiledProlContext;
        if (targetClass.isAssignableFrom(JProlContext.class)) {
          result = (T) prolContext;
        } else if (targetClass.isAssignableFrom(ParserContext.class)) {
          result = (T) prolContext.makeParserContext();
        } else if (targetClass.isAssignableFrom(KnowledgeBase.class)) {
          result = (T) prolContext.getKnowledgeBase();
        } else {
          result = null;
        }
      }
      return result;
    } else {
      throw new IllegalArgumentException(
          "Expected " + JProlScriptEngineProvider.class.getCanonicalName() + " instance");
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isDisposed() {
    return this.disposed.get();
  }

  /**
   * Marks this compiled handle as closed. Does not dispose the underlying {@link JProlContext}
   * (it is shared with the creating {@link JProlScriptEngine} for the compiling thread).
   */
  @Override
  public void dispose() {
    this.disposed.compareAndSet(false, true);
  }

  /**
   * {@inheritDoc}
   * <p>Marks this handle closed only; does not dispose the shared {@link JProlContext}.
   */
  @Override
  public void close() {
    this.dispose();
  }

  /**
   * {@inheritDoc}
   * <p>Forwards to the parent {@link JProlScriptEngine#gc()}.
   */
  @Override
  public void gc() {
    if (!this.disposed.get()) {
      this.engine.gc();
    }
  }

  @Deprecated
  protected void finalize() {
    this.dispose();
  }
}
