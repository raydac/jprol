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

import com.igormaznitsa.prol.logic.ProlContext;
import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;
import java.util.List;
import javax.script.Bindings;
import javax.script.ScriptContext;

public final class JProlScriptContext implements ScriptContext {

  private final ProlContext context;
  private static JProlBindings globalMap = null;
  private JProlBindings engineMap = new JProlBindings(this);

  private static final List<Integer> ALLOWED_SCOPES = Arrays.asList(ScriptContext.ENGINE_SCOPE, ScriptContext.GLOBAL_SCOPE);

  JProlScriptContext(final String name) {
    this.context = new ProlContext(name);
  }

  public ProlContext getProlContext() {
    return this.context;
  }

  @Override
  public void setBindings(final Bindings bindings, final int scope) {
    if (bindings == null) {
      throw new NullPointerException("null bindings in ENGINE scope");
    }
    switch (scope) {
      case ScriptContext.ENGINE_SCOPE:
        engineMap = (JProlBindings) bindings;
        break;
      case ScriptContext.GLOBAL_SCOPE:
        globalMap = (JProlBindings) bindings;
        break;
      default:
        throw new IllegalArgumentException("invalid scope");
    }
  }

  JProlBindings getJprolBindings(final int scope) {
    switch (scope) {
      case ScriptContext.ENGINE_SCOPE:
        return engineMap;
      case ScriptContext.GLOBAL_SCOPE:
        return globalMap;
      default:
        throw new IllegalArgumentException("invalid scope");
    }
  }

  @Override
  public Bindings getBindings(final int scope) {
    return this.getJprolBindings(scope);
  }

  @Override
  public void setAttribute(final String name, final Object value, final int scope) {
    this.getJprolBindings(scope).put(name, value);
  }

  @Override
  public Object getAttribute(final String name, final int scope) {
    return this.getJprolBindings(scope).get(name);
  }

  @Override
  public Object removeAttribute(final String name, final int scope) {
    return this.getJprolBindings(scope).remove(name);
  }

  @Override
  public Object getAttribute(String name) {
    return ALLOWED_SCOPES.stream().map(x -> this.getAttribute(name, x)).filter(x -> x != null).findFirst().orElse(null);
  }

  @Override
  public int getAttributesScope(final String name) {
    return ALLOWED_SCOPES.stream().filter(x -> this.getJprolBindings(x).containsKey(name)).findFirst().orElse(-1);
  }

  @Override
  public Writer getWriter() {
    return null;
  }

  @Override
  public Writer getErrorWriter() {
    return null;
  }

  @Override
  public void setWriter(Writer writer) {

  }

  @Override
  public void setErrorWriter(Writer writer) {

  }

  @Override
  public Reader getReader() {
    return null;
  }

  @Override
  public void setReader(Reader reader) {

  }

  @Override
  public List<Integer> getScopes() {
    return ALLOWED_SCOPES;
  }

}
