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

import com.igormaznitsa.prol.data.Terms;
import com.igormaznitsa.prol.logic.ProlContext;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public final class JProlScriptEngineFactory implements ScriptEngineFactory {

  @Override
  public String getEngineName() {
    return ProlContext.ENGINE_NAME;
  }

  @Override
  public String getEngineVersion() {
    return ProlContext.ENGINE_VERSION;
  }

  @Override
  public List<String> getExtensions() {
    return Collections.singletonList("jprol");
  }

  @Override
  public List<String> getMimeTypes() {
    return Collections.singletonList("text/x-prolog");
  }

  @Override
  public List<String> getNames() {
    return Collections.singletonList("jprol");
  }

  @Override
  public String getLanguageName() {
    return "Edinburgh Prolog";
  }

  @Override
  public String getLanguageVersion() {
    return "0.0.1";
  }

  @Override
  public Object getParameter(final String key) {
    switch (key) {
      case ScriptEngine.ENGINE:
        return this.getEngineName();
      case ScriptEngine.ENGINE_VERSION:
        return this.getEngineVersion();
      case ScriptEngine.NAME:
        return this.getNames();
      case ScriptEngine.LANGUAGE:
        return this.getLanguageName();
      case ScriptEngine.LANGUAGE_VERSION:
        return this.getLanguageVersion();
      default:
        return null;
    }
  }

  @Override
  public String getMethodCallSyntax(final String obj, final String method, final String... args) {
    final StringBuilder result = new StringBuilder(obj);
    result.append(":").append(method).append('(');
    String delimiter = "";
    for (final String a : args) {
      result.append(delimiter).append(a);
      delimiter = ",";
    }
    result.append(')');
    return result.toString();
  }

  @Override
  public String getOutputStatement(final String toDisplay) {
    return String.format("write(%s)", Terms.newAtom(toDisplay).toSrcString());
  }

  @Override
  public String getProgram(final String... statements) {
    return Arrays.stream(statements).collect(Collectors.joining(" , ", "", " ."));
  }

  @Override
  public ScriptEngine getScriptEngine() {
    return new JProlScriptEngine(this);
  }
}
