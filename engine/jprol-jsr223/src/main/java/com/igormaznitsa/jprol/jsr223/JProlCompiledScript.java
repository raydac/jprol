package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.jsr223.JProlScriptEngine.java2term;
import static java.lang.System.identityHashCode;

import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import java.io.StringReader;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

public class JProlCompiledScript extends CompiledScript
    implements Invocable, AutoCloseable, JProlScriptEngineProvider {
  private final JProlScriptEngine engine;
  private final List<Term> queryList;
  private final String scriptSources;
  private final JProlContext compiledContext;

  private final AtomicBoolean closed = new AtomicBoolean();

  JProlCompiledScript(
      final JProlScriptEngine engine,
      String script,
      final List<? extends AbstractJProlLibrary> libraries) throws ScriptException {
    this.engine = engine;
    this.scriptSources = script;
    final List<? extends AbstractJProlLibrary> libraries1 = List.copyOf(libraries);

    try {
      final List<Term> parsed =
          JProlScriptEngine.parseWholeScript(script, engine.getPrologContext());
      this.queryList = parsed.stream().map(JProlScriptEngine.QUERY_PREDICATE_FILTER)
          .filter(Objects::nonNull)
          .collect(Collectors.toList());

      script = JProlScriptEngine.joinSources(parsed, JProlScriptEngine.NOT_QUERY_PREDICATE_FILTER,
          Integer.MAX_VALUE,
          Map.of());

      this.compiledContext = new JProlContext(
          "compiled-context-" + identityHashCode(this),
          Stream.concat(JProlScriptEngine.BOOTSTRAP_LIBRARIES.stream(), libraries1.stream())
              .toArray(
                  AbstractJProlLibrary[]::new)
      );
      this.compiledContext.addIoResourceProvider(JProlScriptEngine.CONSOLE_IO_PROVIDER);
      this.compiledContext.consult(new StringReader(script));
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

  private void assertNotClosed() {
    if (this.closed.get()) {
      throw new IllegalStateException("Closed context");
    }
  }

  public JProlContext getCompiledContext() {
    this.assertNotClosed();
    return this.compiledContext;
  }

  public List<Term> getQueryList() {
    this.assertNotClosed();
    return this.queryList;
  }

  public String getScriptSources() {
    this.assertNotClosed();
    return this.scriptSources;
  }

  @Override
  public Object eval(final ScriptContext context) throws ScriptException {
    this.assertNotClosed();
    final JProlContext oldContext = this.engine.getPrologContext();
    try {
      this.engine.setPrologContext(this.compiledContext);
      this.engine.applyContextFlags(context);

      Object lastResult = null;

      final Map<String, Term> bindings = JProlScriptEngine.extractVarsFromBindings(context);
      for (final Term q : this.queryList) {
        Term preparedQuery = q.makeClone();
        if (!bindings.isEmpty()) {
          for (final Map.Entry<String, Term> t : bindings.entrySet()) {
            preparedQuery = preparedQuery.replaceVar(t.getKey(), t.getValue());
          }
        }
        lastResult = this.engine.executeQuery(preparedQuery, context);
      }
      return lastResult == null ? Boolean.FALSE : lastResult;
    } catch (Exception e) {
      throw new ScriptException("Error evaluating compiled script: " + e.getMessage());
    } finally {
      this.engine.setPrologContext(oldContext);
    }
  }

  @Override
  public ScriptEngine getEngine() {
    this.assertNotClosed();
    return this.engine;
  }

  @Override
  public Object invokeMethod(final Object thisObject, final String name, final Object... args)
      throws ScriptException, NoSuchMethodException {
    this.assertNotClosed();
    if (thisObject instanceof JProlScriptEngineProvider) {

      final JProlScriptEngine thisEngine =
          ((JProlScriptEngineProvider) thisObject).getJProlScriptEngine();
      final JProlContext oldContext = thisEngine.getPrologContext();
      thisEngine.setPrologContext(this.compiledContext);
      try {
        final Term[] terms = new Term[args.length];
        for (int i = 0; i < args.length; i++) {
          terms[i] = java2term(args[i]);
        }
        final Term term = Terms.newStruct(name, terms, SourcePosition.UNKNOWN);
        return thisEngine.executeQuery(term, thisEngine.getContext());
      } finally {
        thisEngine.setPrologContext(oldContext);
      }
    } else {
      throw new IllegalArgumentException(
          "Expected " + JProlScriptEngineProvider.class.getCanonicalName() + " instance");
    }
  }

  @Override
  public Object invokeFunction(final String name, final Object... args)
      throws ScriptException, NoSuchMethodException {
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
    this.assertNotClosed();

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
        final JProlContext prolContext = engine.getPrologContext();
        if (prolContext != null && targetClass.isAssignableFrom(JProlContext.class)) {
          result = (T) prolContext;
        } else if (prolContext != null && targetClass.isAssignableFrom(ParserContext.class)) {
          result = (T) prolContext.getParserContext();
        } else if (prolContext != null && targetClass.isAssignableFrom(KnowledgeBase.class)) {
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
  public void close() throws Exception {
    if (this.closed.compareAndSet(false, true)) {
      this.compiledContext.dispose();
      this.engine.close();
    }
  }
}
