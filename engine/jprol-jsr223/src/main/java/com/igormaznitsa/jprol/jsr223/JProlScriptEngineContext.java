package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.jsr223.JProlScriptEngine.CONSOLE_IO_PROVIDER;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngine.JPROL_CONTEXT_FLAGS;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngine.JPROL_GLOBAL_CRITICAL_PREDICATE_ALLOW;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngine.JPROL_GLOBAL_EXECUTOR_SERVICE;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngine.JPROL_GLOBAL_KNOWLEDGE_BASE;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngine.JPROL_LIBRARIES;
import static com.igormaznitsa.jprol.jsr223.JProlScriptEngineUtils.java2term;
import static java.lang.System.identityHashCode;
import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.kbase.inmemory.ConcurrentInMemoryKnowledgeBase;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.io.File;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.SimpleBindings;

public class JProlScriptEngineContext implements ScriptContext {

  static final List<AbstractJProlLibrary> BOOTSTRAP_LIBRARIES =
      List.of(new JProlCoreLibrary(), new JProlJsr223BootstrapLibrary());
  private static final List<Integer> SCOPES = List.of(GLOBAL_SCOPE, ENGINE_SCOPE);
  private final AtomicReference<Writer> writer = new AtomicReference<>();
  private final AtomicReference<Reader> reader =
      new AtomicReference<>();
  private final AtomicReference<Writer> writerErr =
      new AtomicReference<>();
  private final AtomicBoolean disposed = new AtomicBoolean();
  private final JProlScriptEngineFactory factory;

  private final ClearableThreadLocal<Bindings> engineBindings =
      new ClearableThreadLocal<>(() -> new SimpleBindings(new ConcurrentHashMap<>()));
  private final ClearableThreadLocal<JProlContext> jprolContext =
      new ClearableThreadLocal<>(() -> this.newInitedJProlContext(null));

  private final AtomicReference<KnowledgeBase> prolKnowledgeBase = new AtomicReference<>();

  JProlScriptEngineContext(final JProlScriptEngineFactory factory) {
    this(factory, new InputStreamReader(System.in), new PrintWriter(System.out),
        new PrintWriter(System.err));
  }

  JProlScriptEngineContext(
      final JProlScriptEngineFactory factory,
      final Reader reader,
      final Writer outWriter,
      final Writer errWriter
  ) {
    this.factory = factory;
    this.writer.set(outWriter);
    this.writerErr.set(errWriter);
    this.reader.set(reader);

    this.prolKnowledgeBase.set(new ConcurrentInMemoryKnowledgeBase(
        "jprol-script-engine-context-" + System.identityHashCode(this)));
  }

  private static void checkName(final String name) {
    requireNonNull(name);
    if (name.isEmpty()) {
      throw new IllegalArgumentException("name cannot be empty");
    }
  }

  public JProlContext findOrMakeJProlContext() {
    this.assertNotClosed();
    return this.jprolContext.get();
  }

  public JProlContext findJProlContext() {
    this.assertNotClosed();
    return this.jprolContext.find();
  }

  /**
   * Remove all thread local internal states.
   *
   * @param disposeProlContext if true then existing JProl context will be disposed
   */
  public void clear(final boolean disposeProlContext) {
    this.assertNotClosed();

    final JProlContext context = this.jprolContext.remove();
    if (context != null && disposeProlContext) {
      context.dispose();
    }
    final Bindings currentBindings = this.engineBindings.remove();
    if (currentBindings != null) {
      currentBindings.clear();
    }
  }

  public KnowledgeBase getKnowledgeBase() {
    this.assertNotClosed();
    KnowledgeBase result =
        (KnowledgeBase) this.factory.getGlobalBindings().get(JPROL_GLOBAL_KNOWLEDGE_BASE);
    if (result == null) {
      result = this.prolKnowledgeBase.get();
    }
    return result;
  }

  @SuppressWarnings("unchecked")
  private JProlContext newInitedJProlContext(final KnowledgeBase forceKnowledgeBase) {
    final List<AbstractJProlLibrary> libraries =
        (List<AbstractJProlLibrary>) this.getAttribute(JPROL_LIBRARIES);
    final List<AbstractJProlLibrary> prolLibraries = new ArrayList<>(BOOTSTRAP_LIBRARIES);
    if (libraries != null) {
      prolLibraries.addAll(libraries);
    }

    final ExecutorService executorService =
        (ExecutorService) this.factory.getGlobalBindings().get(JPROL_GLOBAL_EXECUTOR_SERVICE);

    final JProlCriticalPredicateAllow foundPredicateAllow =
        (JProlCriticalPredicateAllow) this.factory.getGlobalBindings()
            .get(JPROL_GLOBAL_CRITICAL_PREDICATE_ALLOW);
    final JProlCriticalPredicateAllow predicateAllow =
        foundPredicateAllow == null ? (a, b, c) -> true : foundPredicateAllow;

    final Function<String, KnowledgeBase> knowledgeBaseSupplier =
        s -> Objects.requireNonNullElseGet(forceKnowledgeBase, this::getKnowledgeBase);

    final JProlContext result = new JProlContext(
        "jprol-jsr223-" + identityHashCode(this),
        new File(System.getProperty("user.home")),
        knowledgeBaseSupplier,
        executorService == null ? ForkJoinPool.commonPool() : executorService,
        prolLibraries.toArray(AbstractJProlLibrary[]::new)
    ) {
      @Override
      public boolean isCriticalPredicateAllowed(
          Class<? extends AbstractJProlLibrary> sourceLibrary,
          JProlChoicePoint choicePoint, String predicateIndicator) {
        return predicateAllow.isCriticalPredicateAllowed(sourceLibrary, choicePoint,
            predicateIndicator);
      }
    };
    result.addIoResourceProvider(CONSOLE_IO_PROVIDER);

    Object flagAttributes = this.getAttribute(JPROL_CONTEXT_FLAGS);
    if (flagAttributes != null) {
      if (flagAttributes instanceof Map) {
        final StringBuilder buffer = new StringBuilder();
        final Map<String, Object> flags = (Map<String, Object>) flagAttributes;
        for (Map.Entry<String, Object> entry : flags.entrySet()) {
          buffer.append(":- set_prolog_flag('").append(ProlUtils.escapeSrc(entry.getKey()))
              .append("', ").append(java2term(entry.getValue()).toSrcString())
              .append("). ");
        }
        result.consult(new StringReader(buffer.toString()));
      } else {
        throw new IllegalArgumentException("Expected Map<String,Object> for flags but found " +
            flagAttributes.getClass().getCanonicalName());
      }
    }

    return result;
  }

  private void assertNotClosed() {
    if (this.disposed.get()) {
      throw new IllegalStateException("Already closed context");
    }
  }

  @Override
  public void setBindings(final Bindings bindings, final int scope) {
    this.assertNotClosed();
    switch (scope) {
      case ENGINE_SCOPE: {
        this.engineBindings.set(bindings);
      }
      break;
      case GLOBAL_SCOPE: {
        final Set<String> currentKeys = Set.copyOf(this.factory.getGlobalBindings().keySet());
        currentKeys.forEach(x -> {
          if (!bindings.containsKey(x)) {
            {
              this.factory.getGlobalBindings().remove(x);
            }
          }
        });
        this.factory.getGlobalBindings().putAll(bindings);
      }
      break;
      default:
        throw new IllegalArgumentException("Invalid scope value.");
    }
  }

  @Override
  public Bindings getBindings(final int scope) {
    this.assertNotClosed();
    switch (scope) {
      case ENGINE_SCOPE:
        return this.engineBindings.get();
      case GLOBAL_SCOPE:
        return this.factory.getGlobalBindings();
      default:
        throw new IllegalArgumentException("Invalid scope value.");
    }
  }

  @Override
  public void setAttribute(final String name, final Object value, final int scope) {
    checkName(name);
    this.getBindings(scope).put(name, value);
  }

  @Override
  public Object getAttribute(final String name, final int scope) {
    checkName(name);
    return this.getBindings(scope).get(name);
  }

  @Override
  public Object removeAttribute(final String name, final int scope) {
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
    this.assertNotClosed();
    return this.writer.get();
  }

  @Override
  public void setWriter(final Writer writer) {
    this.assertNotClosed();
    this.writer.set(writer);
  }

  @Override
  public Writer getErrorWriter() {
    this.assertNotClosed();
    return this.writerErr.get();
  }

  @Override
  public void setErrorWriter(final Writer writer) {
    this.assertNotClosed();
    this.writerErr.set(writer);
  }

  @Override
  public Reader getReader() {
    this.assertNotClosed();
    return this.reader.get();
  }

  @Override
  public void setReader(final Reader reader) {
    this.assertNotClosed();
    this.reader.set(reader);
  }

  @Override
  public List<Integer> getScopes() {
    return SCOPES;
  }

  public void dispose() {
    if (this.disposed.compareAndSet(false, true)) {
      this.engineBindings.removeAll(Map::clear);
      this.jprolContext.removeAll(JProlContext::dispose);
      this.writer.set(null);
      this.writerErr.set(null);
      this.reader.set(null);
      this.prolKnowledgeBase.set(null);
    }
  }

  public void reloadLibraries(final boolean disposeOldContext) {
    this.assertNotClosed();
    final JProlContext current = this.jprolContext.remove();
    if (current == null) {
      this.findOrMakeJProlContext();
    } else {
      try {
        if (this.jprolContext.set(this.newInitedJProlContext(this.getKnowledgeBase())) != null) {
          throw new IllegalStateException("Unexpectedly found created JProl engine instance");
        }
      } finally {
        if (disposeOldContext) {
          current.dispose();
        }
      }
    }
  }

  /**
   * Returns total number of context and binding options kept for different thread in internal stores.
   *
   * @return number of instances in internal stores
   */
  public int size() {
    this.assertNotClosed();
    return this.jprolContext.size() + this.engineBindings.size();
  }
}
