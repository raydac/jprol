/*
 * Copyright 2019 Igor Maznitsa.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prol.script;

import com.igormaznitsa.prol.data.TermStruct;
import org.junit.Test;

import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import java.util.List;

import static org.junit.Assert.*;

public class JProlScriptEngineTest {

  private static final ScriptEngineFactory JPROL_ENGINE_FACTORY = new JProlScriptEngineFactory();

  @Test
  public void testEval() throws Exception {
    final ScriptEngine engine = JPROL_ENGINE_FACTORY.getScriptEngine();
    assertNull(engine.eval("X is 5 * 10, X = 10."));
    assertNull(engine.eval("X is 5 * 10, X = 10."));
    assertNotNull(engine.eval("X is 5 * 10, X = 50.", engine.getContext()));
    assertNotNull(engine.eval("X is 5 * 10, X = 50.", engine.getContext()));
  }

  @Test
  public void testCompiled() throws Exception {
    final ScriptEngine engine = JPROL_ENGINE_FACTORY.getScriptEngine();

    final CompiledScript success = ((Compilable) engine).compile("X is 5 * 10, X = 50.");
    for (int i = 0; i < 10; i++) {
      assertNotNull(success.eval());
    }

    final CompiledScript failed = ((Compilable) engine).compile("X is 5 * 10, X = 10.");
    for (int i = 0; i < 10; i++) {
      assertNull(failed.eval());
    }
  }

  @Test
  public void testBindings() throws Exception {
    final ScriptEngine engine = JPROL_ENGINE_FACTORY.getScriptEngine();
    engine.put("X", "world");
    assertNotNull(engine.eval("write(X)."));
  }

  @Test
  public void testBindAndGet() throws Exception {
    final ScriptEngine engine = JPROL_ENGINE_FACTORY.getScriptEngine();
    engine.put("X", 5);
    assertNotNull(engine.eval("Y is X + 10."));
    assertEquals(15, engine.get("Y"));
  }

  @Test
  public void testAssert() throws Exception {
    final ScriptEngine engine = JPROL_ENGINE_FACTORY.getScriptEngine();
    final JProlScriptContext context = (JProlScriptContext)engine.getContext();
    assertTrue(context.findClauses("some/1").isEmpty());
    assertNull(engine.eval("(Y=1;Y=2;Y=3),assertz(some(Y)),fail."));

    final List<TermStruct> found = context.findClauses("some/1");
    assertEquals(3, found.size());
    assertEquals("some(1)", found.get(0).toSourceString());
    assertEquals("some(2)", found.get(1).toSourceString());
    assertEquals("some(3)", found.get(2).toSourceString());
    context.abolish("some/1");
    assertTrue(context.findClauses("some/1").isEmpty());
  }

}
