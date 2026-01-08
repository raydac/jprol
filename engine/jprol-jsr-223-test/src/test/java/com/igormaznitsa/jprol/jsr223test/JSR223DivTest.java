package com.igormaznitsa.jprol.jsr223test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import javax.script.ScriptException;
import org.junit.jupiter.api.Test;

class JSR223DivTest {

  @Test
  void testDiv() throws Exception {
    try (final JSR223Div divider = new JSR223Div()) {
      assertEquals(32L, divider.div(128, 4));
      assertEquals(-10L, divider.div(31, -3));
      assertThrows(ScriptException.class, () -> divider.div(128, 0));
    }
  }
}