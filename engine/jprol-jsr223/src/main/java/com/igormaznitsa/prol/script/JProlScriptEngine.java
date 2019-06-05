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

import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlReader;
import java.io.IOException;
import java.io.Reader;
import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;

class JProlScriptEngine extends AbstractScriptEngine {

  private final JProlScriptEngineFactory factory;

  private static final class IoActionProviderImpl {

    private final ScriptContext context;

    private IoActionProviderImpl(final ScriptContext context) {
      this.context = context;
    }
  }

  public JProlScriptEngine(final JProlScriptEngineFactory factory) {
    super();
    this.factory = factory;
    this.context = new JProlScriptContext("test");
  }

  private Boolean solveGoal(final ProlReader reader, final ScriptContext context) throws ScriptException {
    try {
      final ProlContext prolContext = new ProlContext("jprol_script_engine");
      final Goal goal = new Goal(reader, prolContext);
      return goal.solve() != null;
    } catch (IOException ex) {
      throw new ScriptException(ex);
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
      return null;
    }
  }

  @Override
  public ScriptContext getContext() {
    return this.context;
  }

  @Override
  public Object eval(final String script, final ScriptContext context) throws ScriptException {
    return solveGoal(new ProlReader(script), context);
  }

  @Override
  public Object eval(final Reader reader, final ScriptContext context) throws ScriptException {
    return solveGoal(new ProlReader(reader), context);
  }

  @Override
  public Bindings createBindings() {
    return new JProlBindings();
  }

  @Override
  public ScriptEngineFactory getFactory() {
    return this.factory;
  }

}
