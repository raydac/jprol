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

import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.libraries.AbstractProlLibrary;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.parser.ProlReader;

import javax.script.Bindings;
import javax.script.ScriptContext;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;
import java.util.List;

public final class JProlScriptContext implements ScriptContext {

  private static final List<Integer> ALLOWED_SCOPES = Arrays.asList(ScriptContext.ENGINE_SCOPE, ScriptContext.GLOBAL_SCOPE);
  private static JProlBindings globalMap = null;
  private final ProlContext context;
  private JProlBindings engineMap = new JProlBindings(this);

  JProlScriptContext(final String name) {
    this.context = new ProlContext(name);
  }

  ProlContext getProlContext() {
    return this.context;
  }

  private void consult(final ProlReader reader) throws IOException {
    new ProlConsult(reader, this.context).consult();
  }

  public void addOperators(final Operator... operators) {
    for (final Operator op : operators) {
      this.context.getKnowledgeBase().addOperator(op);
    }
  }

  public void abolish(final String signature) {
    this.context.getKnowledgeBase().abolish(signature);
  }

  public List<TermStruct> findClauses(final String signature) {
    return this.context.getKnowledgeBase().findAllForSignature(signature);
  }

  public void addLibraries(final AbstractProlLibrary... libraries) throws IOException {
    for (final AbstractProlLibrary lib : libraries) {
      this.context.addLibrary(lib);
    }
  }

  public KnowledgeBase getKnowledgeBase() {
    return this.context.getKnowledgeBase();
  }

  public void consult(final String text) {
    try {
      this.consult(new ProlReader(text));
    } catch (IOException ex) {
      throw new RuntimeException("Unexpected IO error", ex);
    }
  }

  public void consult(final Reader reader) throws IOException {
    this.consult(new ProlReader(reader));
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

  JProlBindings getJProlBindings(final int scope) {
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
    return this.getJProlBindings(scope);
  }

  @Override
  public void setAttribute(final String name, final Object value, final int scope) {
    this.getJProlBindings(scope).put(name, value);
  }

  @Override
  public Object getAttribute(final String name, final int scope) {
    return this.getJProlBindings(scope).get(name);
  }

  @Override
  public Object removeAttribute(final String name, final int scope) {
    return this.getJProlBindings(scope).remove(name);
  }

  @Override
  public Object getAttribute(String name) {
    return ALLOWED_SCOPES.stream().map(x -> this.getAttribute(name, x)).filter(x -> x != null).findFirst().orElse(null);
  }

  @Override
  public int getAttributesScope(final String name) {
    return ALLOWED_SCOPES.stream().filter(x -> this.getJProlBindings(x).containsKey(name)).findFirst().orElse(-1);
  }

  @Override
  public Writer getWriter() {
    return null;
  }

  @Override
  public void setWriter(Writer writer) {

  }

  @Override
  public Writer getErrorWriter() {
    return null;
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
