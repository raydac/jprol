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
 * Allows to compile a script for further usage. Allows to decrease overheads and increase call speed.
 * As precompiled query it keeps only the first found script query marked '?-'.
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

  public JProlContext getCompiledContext() {
    this.assertNotDisposed();
    return this.compiledProlContext;
  }

  /**
   * Query found in pre-compiled source, can be null np query.
   *
   * @return first script query or null
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
   * Prove compiled query once.
   *
   * @param context target context
   * @return {@link Boolean#TRUE} if proved, {@link Boolean#FALSE} otherwise
   * @throws ScriptException if any internal error
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
      Object result =
          this.engine.proveQueryOnce(preparedQuery, this.compiledProlContext, context.getBindings(
              ENGINE_SCOPE));
      return result == null ? Boolean.FALSE : Boolean.TRUE;
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }

  @Override
  public ScriptEngine getEngine() {
    this.assertNotDisposed();
    return this.engine;
  }

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

  @Override
  public Object invokeFunction(final String name, final Object... args) {
    return this.invokeMethod(this, name, args);
  }

  @Override
  public JProlScriptEngine getJProlScriptEngine() {
    return this.engine;
  }

  @Override
  public <T> T getInterface(final Class<T> targetClass) {
    return this.getInterface(this.engine, targetClass);
  }

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
          result = (T) prolContext.getParserContext();
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

  @Override
  public boolean isDisposed() {
    return this.disposed.get();
  }

  @Override
  public void dispose() {
    if (this.disposed.compareAndSet(false, true)) {
      this.compiledProlContext.dispose();
    }
  }

  @Override
  public void close() {
    this.dispose();
  }

  @Override
  public void gc() {
    this.assertNotDisposed();
    if (!this.engine.isDisposed()) {
      this.engine.gc();
    }
  }

  @Deprecated
  protected void finalize() {
    this.dispose();
  }
}
