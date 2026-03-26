package com.igormaznitsa.jprol.jsr223test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.jsr223.JProlScriptEngine;
import java.util.List;
import java.util.Map;
import javax.script.Bindings;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.SimpleBindings;
import org.junit.jupiter.api.Test;

public class JSR223MinimalisticTest {

  @Test
  void testMul_ScriptEngine() throws Exception {
    ScriptEngine engine = new ScriptEngineManager().getEngineByName("jprol");
    engine.eval("mul(A,B,Y) :- Y is A * B.");
    Bindings bindings = new SimpleBindings();
    bindings.put("X", 5);
    bindings.put("Y", 6);
    assertTrue(
        (boolean) engine.eval("?-mul(X,Y,Z).", bindings)); // returns TRUE if proven successfully
    assertEquals(30L, bindings.get("Z"));
  }

  @Test
  void testMul_JProlScriptEngine() throws Exception {
    JProlScriptEngine engine =
        (JProlScriptEngine) new ScriptEngineManager().getEngineByMimeType("application/jprol");
    engine.consult("mul(A,B,Y) :- Y is A * B.");
    List<Map<String, Object>> results = engine.proveAll("mul(5,6,Z).");
    assertEquals(1, results.size());
    assertEquals(30L, (Long) results.get(0).get("Z"));
  }
}
