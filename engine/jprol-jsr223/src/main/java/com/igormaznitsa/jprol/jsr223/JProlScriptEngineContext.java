package com.igormaznitsa.jprol.jsr223;

import static java.lang.ThreadLocal.withInitial;
import static java.util.Objects.requireNonNull;

import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.SimpleBindings;

public class JProlScriptEngineContext implements ScriptContext {

  private static final List<Integer> SCOPES = List.of(GLOBAL_SCOPE, ENGINE_SCOPE);
  private final Bindings globalBindings = new SimpleBindings(new ConcurrentHashMap<>());
  private final ThreadLocal<Bindings> engineBindings = withInitial(SimpleBindings::new);
  private final AtomicReference<Writer> writer = new AtomicReference<>();
  private final AtomicReference<Reader> reader =
      new AtomicReference<>();
  private final AtomicReference<Writer> writerErr =
      new AtomicReference<>();

  JProlScriptEngineContext() {
    this(new InputStreamReader(System.in), new PrintWriter(System.out),
        new PrintWriter(System.err));
  }

  JProlScriptEngineContext(
      final Reader reader,
      final Writer outWriter,
      final Writer errWriter
  ) {
    this.writer.set(outWriter);
    this.writerErr.set(errWriter);
    this.reader.set(reader);
  }

  private static void checkName(final String name) {
    requireNonNull(name);
    if (name.isEmpty()) {
      throw new IllegalArgumentException("name cannot be empty");
    }
  }

  @Override
  public void setBindings(final Bindings bindings, final int scope) {
    switch (scope) {
      case ENGINE_SCOPE: {
        this.engineBindings.set(bindings);
      }
      break;
      case GLOBAL_SCOPE: {
        final Set<String> currentKeys = Set.copyOf(this.globalBindings.keySet());
        currentKeys.forEach(x -> {
          if (!bindings.containsKey(x)) {
            {
              this.globalBindings.remove(x);
            }
          }
        });
        this.globalBindings.putAll(bindings);
      }
      break;
      default:
        throw new IllegalArgumentException("Invalid scope value.");
    }
  }

  @Override
  public Bindings getBindings(final int scope) {
    switch (scope) {
      case ENGINE_SCOPE:
        return this.engineBindings.get();
      case GLOBAL_SCOPE:
        return this.globalBindings;
      default:
        throw new IllegalArgumentException("Invalid scope value.");
    }
  }

  @Override
  public void setAttribute(String name, Object value, int scope) {
    checkName(name);
    this.getBindings(scope).put(name, value);
  }

  @Override
  public Object getAttribute(String name, int scope) {
    checkName(name);
    return this.getBindings(scope).get(name);
  }

  @Override
  public Object removeAttribute(String name, int scope) {
    checkName(name);
    return this.getBindings(scope).remove(name);
  }

  @Override
  public Object getAttribute(final String name) {
    checkName(name);
    final Bindings engine = this.getBindings(ENGINE_SCOPE);
    final Bindings global = this.getBindings(GLOBAL_SCOPE);
    if (engine.containsKey(name)) {
      return engine.get(name);
    }
    return global.get(name);
  }

  @Override
  public int getAttributesScope(String name) {
    checkName(name);
    final Bindings engine = this.getBindings(ENGINE_SCOPE);
    final Bindings global = this.getBindings(GLOBAL_SCOPE);
    if (engine.containsKey(name)) {
      return ENGINE_SCOPE;
    }
    if (global.containsKey(name)) {
      return GLOBAL_SCOPE;
    }
    return -1;
  }

  @Override
  public Writer getWriter() {
    return this.writer.get();
  }

  @Override
  public void setWriter(Writer writer) {
    this.writer.set(writer);
  }

  @Override
  public Writer getErrorWriter() {
    return this.writerErr.get();
  }

  @Override
  public void setErrorWriter(Writer writer) {
    this.writerErr.set(writer);
  }

  @Override
  public Reader getReader() {
    return this.reader.get();
  }

  @Override
  public void setReader(Reader reader) {
    this.reader.set(reader);
  }

  @Override
  public List<Integer> getScopes() {
    return SCOPES;
  }
}
