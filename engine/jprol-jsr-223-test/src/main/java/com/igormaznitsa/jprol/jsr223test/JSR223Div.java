package com.igormaznitsa.jprol.jsr223test;

import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.jsr223.JProlScriptEngine;
import com.igormaznitsa.jprol.jsr223.JProlScriptEngineUtils;
import java.util.Objects;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class JSR223Div implements AutoCloseable {

  private static final ScriptEngineManager MANAGER = new ScriptEngineManager();

  private final JProlScriptEngine prolScripEngine;

  public JSR223Div() throws ScriptException {
    this.prolScripEngine = (JProlScriptEngine) Objects.requireNonNull(
        MANAGER.getEngineByMimeType("application/jprol"));
    this.prolScripEngine.consult("divide(A,B,C) :- C is A div B.");
  }

  public long div(final long a, final long b) throws ScriptException {
    final TermVar result = Terms.newVar("Result");
    if ((boolean) this.prolScripEngine.invokeFunction("divide", a, b, result)) {
      return (long) JProlScriptEngineUtils.term2java(result);
    } else {
      throw new IllegalStateException("No result");
    }
  }

  @Override
  public void close() {
    this.prolScripEngine.close();
  }
}
