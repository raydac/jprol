package com.igormaznitsa.jprol.jsr223test;

import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.jsr223.JProlScriptEngine;
import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import javax.script.SimpleBindings;

public class JSR223Multiply {

  private final ScriptEngine engine;

  public JSR223Multiply() throws ScriptException {
    this.engine = requireNonNull(new ScriptEngineManager().getEngineByName("jprol"));
    this.engine.eval("mul(A,B,Y) :- Y is A * B.");
  }

  public long mul(final long a, final long b) throws ScriptException {
    final Bindings bindings = new SimpleBindings();
    bindings.put("X", a);
    bindings.put("Y", b);
    requireNonNull(this.engine.eval("?-mul(X,Y,Z).", bindings));
    return requireNonNull((Long) bindings.get("Z"));
  }

  public int internalSize() {
    return ((JProlScriptEngine) engine).size();
  }
}
