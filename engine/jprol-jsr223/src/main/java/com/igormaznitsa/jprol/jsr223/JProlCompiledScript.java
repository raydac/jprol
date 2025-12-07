package com.igormaznitsa.jprol.jsr223;

import static java.lang.System.identityHashCode;

import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import java.io.StringReader;
import java.util.List;
import java.util.stream.Stream;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;

class JProlCompiledScript extends CompiledScript {
  private final JProlScriptEngine engine;
  private final String script;
  private final JProlContext compiledContext;

  JProlCompiledScript(
      final JProlScriptEngine engine,
      final String script,
      final List<? extends AbstractJProlLibrary> libraries) throws ScriptException {
    this.engine = engine;
    this.script = script;
    final List<? extends AbstractJProlLibrary> libraries1 = List.copyOf(libraries);

    try {
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
            "Error parsing query: " + e.getMessage() + " " + e.getLine() + ':' + e.getPos());
      } else {
        throw new ScriptException("Error parsing query: " + e.getMessage());
      }
    } catch (Exception e) {
      throw new ScriptException("Error compiling Prolog script: " + e.getMessage());
    }
  }

  @Override
  public Object eval(final ScriptContext context) throws ScriptException {
    final JProlContext oldContext = engine.getPrologContext();
    try {
      this.engine.setPrologContext(this.compiledContext);
      this.engine.applyContextFlags(context);

      String[] lines = script.split("\n");
      Object lastResult = null;

      for (String line : lines) {
        line = line.trim();
        if (line.startsWith("?-")) {
          String query = line.substring(2).trim();
          if (query.endsWith(".")) {
            query = query.substring(0, query.length() - 1).trim();
          }
          lastResult = engine.executeQuery(query, context);
        }
      }

      return lastResult != null ? lastResult : Boolean.TRUE;

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
