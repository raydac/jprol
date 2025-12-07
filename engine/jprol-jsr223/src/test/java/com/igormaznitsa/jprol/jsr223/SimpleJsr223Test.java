package com.igormaznitsa.jprol.jsr223;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import org.junit.jupiter.api.Test;

public class SimpleJsr223Test {

  private ScriptEngine findScriptEngine() {
    var manager = new ScriptEngineManager();
    var engine = manager.getEngineByName("jprol");
    assertNotNull(engine);
    return engine;
  }

  @Test
  public void testSimpleHelloWorld() throws Exception {
    assertTrue((Boolean) findScriptEngine().eval("?-writeln('Hello world')."));
  }

  @Test
  public void testBindings() throws Exception {
    var engine = findScriptEngine();
    var bindings = engine.getBindings(ScriptContext.ENGINE_SCOPE);
    bindings.put("X", 3);
    assertTrue((Boolean) engine.eval(
        "cube(X,Y):-Y is X * X * X. ?-cube(X,Result). ?- this_line_must_be_ignored('lalala')."));
    assertEquals(27L, bindings.get("Result"));
  }
}
