package com.igormaznitsa.jprol.jsr223test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.SimpleBindings;
import org.junit.jupiter.api.Test;

public class JSR223MinimalisticTest {

  @Test
  void testMul() throws Exception {
    ScriptEngine engine = new ScriptEngineManager().getEngineByName("jprol");
    engine.eval("mul(A,B,Y) :- Y is A * B.");
    Bindings bindings = new SimpleBindings();
    bindings.put("X", 5);
    bindings.put("Y", 6);
    assertTrue(
        (boolean) engine.eval("?-mul(X,Y,Z).", bindings)); // returns TRUE if proven successfully
    assertEquals(30L, bindings.get("Z"));
  }
}
