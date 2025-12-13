package com.igormaznitsa.jprol.jsr223;

import static java.lang.System.identityHashCode;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import java.io.StringReader;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

public class JProlCompiledScript extends CompiledScript {
  private final JProlScriptEngine engine;
  private final List<Term> queryList;
  private final String scriptSources;
  private final JProlContext compiledContext;

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

  public JProlContext getCompiledContext() {
    return this.compiledContext;
  }

  public List<Term> getQueryList() {
    return this.queryList;
  }

  public String getScriptSources() {
    return this.scriptSources;
  }

  @Override
  public Object eval(final ScriptContext context) throws ScriptException {
    final JProlContext oldContext = engine.getPrologContext();
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
      engine.setPrologContext(oldContext);
    }
  }

  @Override
  public ScriptEngine getEngine() {
    return this.engine;
  }
}
