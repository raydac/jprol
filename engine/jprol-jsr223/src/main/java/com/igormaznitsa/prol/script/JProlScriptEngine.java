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

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.exceptions.ParserException;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.parser.ProlReader;

import javax.script.*;
import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;

final class JProlScriptEngine extends AbstractScriptEngine implements Compilable, Invocable {

  private final JProlScriptEngineFactory factory;

  public JProlScriptEngine(final JProlScriptEngineFactory factory) {
    super();
    this.factory = factory;
    this.context = new JProlScriptContext("prol-script-context");
  }

  static void fillGoalByBindings(final JProlBindings bindings, final Map<String, Term> map) {
    if (bindings != null) {
      map.putAll(bindings.getTermMap());
    }
  }

  private Object solveGoal(final ProlReader reader, final JProlScriptContext context) throws ScriptException {
    try {
      final Map<String, Term> varValues = new HashMap<>();

      fillGoalByBindings(context.getJProlBindings(ScriptContext.GLOBAL_SCOPE), varValues);
      fillGoalByBindings(context.getJProlBindings(ScriptContext.ENGINE_SCOPE), varValues);

      final ChoicePoint goal = new ChoicePoint(reader, context.getProlContext(), varValues.isEmpty() ? null : varValues);
      final Object result = goal.next();

      if (result != null) {
        context.getJProlBindings(ScriptContext.ENGINE_SCOPE).fillByValues(goal.findAllGroundedVars());
      }

      return result;
    } catch (ParserException ex) {
      throw new ScriptException(ex.getMessage(), "script", ex.getLine(), ex.getPos());
    } catch (IOException ex) {
      throw new ScriptException(ex);
    }
  }

  @Override
  public Object eval(final String script, final ScriptContext context) throws ScriptException {
    return solveGoal(new ProlReader(script), (JProlScriptContext) context);
  }

  @Override
  public Object eval(final Reader reader, final ScriptContext context) throws ScriptException {
    return solveGoal(new ProlReader(reader), (JProlScriptContext) context);
  }

  @Override
  public Bindings createBindings() {
    return new SimpleBindings();
  }

  @Override
  public ScriptEngineFactory getFactory() {
    return this.factory;
  }

  @Override
  public CompiledScript compile(final String script) throws ScriptException {
    return new JProlCompiledScript(script, this);
  }

  @Override
  public CompiledScript compile(final Reader script) throws ScriptException {
    return new JProlCompiledScript(script, this);
  }

  @Override
  public Object invokeMethod(Object thiz, String name, Object... args) throws ScriptException, NoSuchMethodException {
    return null;
  }

  @Override
  public Object invokeFunction(String name, Object... args) throws ScriptException, NoSuchMethodException {
    return null;
  }

  @Override
  public <T> T getInterface(Class<T> clasz) {
    return null;
  }

  @Override
  public <T> T getInterface(Object thiz, Class<T> clasz) {
    return null;
  }
}
