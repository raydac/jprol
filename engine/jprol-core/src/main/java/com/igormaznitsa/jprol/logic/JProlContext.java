/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
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

package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.SourcePosition.UNKNOWN;
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.Terms.FALSE;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.logic.PredicateInvoker.NULL_INVOKER;
import static java.lang.Boolean.parseBoolean;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toList;

import com.igormaznitsa.jprol.annotations.JProlConsultFile;
import com.igormaznitsa.jprol.annotations.JProlConsultResource;
import com.igormaznitsa.jprol.annotations.JProlConsultText;
import com.igormaznitsa.jprol.annotations.JProlConsultUrl;
import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermOperatorContainer;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlAbortExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlAbstractCatchableException;
import com.igormaznitsa.jprol.exceptions.ProlChoicePointInterruptedException;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlForkExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlInterruptException;
import com.igormaznitsa.jprol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.exceptions.RuntimeIOException;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.kbase.inmemory.ConcurrentInMemoryKnowledgeBase;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.libs.JProlBootstrapLibrary;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import com.igormaznitsa.jprol.logic.triggers.JProlContextTrigger;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggerType;
import com.igormaznitsa.jprol.logic.triggers.TriggerEvent;
import com.igormaznitsa.jprol.trace.JProlContextListener;
import com.igormaznitsa.jprol.trace.TraceEvent;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.ProlUtils;
import com.igormaznitsa.jprol.utils.lazy.LazyExecutorService;
import com.igormaznitsa.jprol.utils.lazy.LazyMap;
import com.igormaznitsa.jprol.utils.lazy.LazySet;
import com.igormaznitsa.jprol.utils.lazy.LazySynchronizedMap;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class JProlContext implements AutoCloseable {

  protected static final String QUERY = "?-";
  private static final AtomicLong TASK_COUNTER = new AtomicLong();
  private static final JProlBootstrapLibrary BOOTSTRAP_LIBRARY = new JProlBootstrapLibrary();
  private final String contextId;
  private final Map<String, Map<JProlTriggerType, List<JProlContextTrigger>>> triggers =
      new LazySynchronizedMap<>();
  private final Map<String, JProlNamedLock> namedLocks;
  private final Map<Long, CompletableFuture<Term>> startedAsyncTasks =
      new LazySynchronizedMap<>();
  private final List<AbstractJProlLibrary> libraries = new CopyOnWriteArrayList<>();
  private final AtomicBoolean disposed = new AtomicBoolean(false);
  private final KnowledgeBase knowledgeBase;
  private final LazyExecutorService asyncTaskExecutorService;
  private final List<JProlContextListener> contextListeners = new CopyOnWriteArrayList<>();
  private final Map<JProlSystemFlag, Term> systemFlags = new ConcurrentHashMap<>();
  private final Set<String> dynamicSignatures = new ConcurrentSkipListSet<>();
  private final ReentrantLock asyncLocker = new ReentrantLock();
  private final Condition asyncCounterCondition = asyncLocker.newCondition();
  private final Map<Term, Object> payloads;
  private final KeyValueTermStore globalVariablesStore;
  private final List<IoResourceProvider> ioProviders = new CopyOnWriteArrayList<>();
  private final File currentFolder;
  private final JProlContext parentContext;
  private boolean verify;
  private boolean trace;
  private boolean shareKnowledgeBaseWithAsyncTasks;
  private UndefinedPredicateBehavior undefinedPredicateBehaviour;

  /**
   * Create instance.
   *
   * @param name          name of the context, must not be null
   * @param currentFolder current file folder for IO  operations
   * @param libs          libraries to be added into context
   */
  public JProlContext(
      final String name,
      final File currentFolder,
      final AbstractJProlLibrary... libs
  ) {
    this(name, currentFolder, ConcurrentInMemoryKnowledgeBase::new, null, libs);
  }

  /**
   * Create instance.
   *
   * @param name                 name of the context, must not be null
   * @param currentFolder        current file folder for IO  operations
   * @param globalVariablesStore global variable store, it is some key value store shared between root and child contexts and allows keep global values, but can't share variables between async tasks. Can be null.
   * @param libs                 libraries to be added into context
   */
  public JProlContext(
      final String name,
      final File currentFolder,
      final KeyValueTermStore globalVariablesStore,
      final AbstractJProlLibrary... libs
  ) {
    this(name, currentFolder, ConcurrentInMemoryKnowledgeBase::new, globalVariablesStore, libs);
  }

  /**
   * Create instance.
   *
   * @param name                  name of the context, must not be null
   * @param currentFolder         current file folder for IO  operations
   * @param knowledgeBaseSupplier function gets context name and supplies instance of knowledge base, must not be null
   * @param libs                  libraries to be added into context
   */
  public JProlContext(
      final String name,
      final File currentFolder,
      final Function<String, KnowledgeBase> knowledgeBaseSupplier,
      final AbstractJProlLibrary... libs
  ) {
    this(name, currentFolder, knowledgeBaseSupplier, null, libs);
  }

  /**
   * Create instance.
   *
   * @param name                  name of the context, must not be null
   * @param currentFolder         current file folder for IO  operations
   * @param knowledgeBaseSupplier function gets context name and supplies instance of knowledge base, must not be null
   * @param globalVariablesStore  global variable store, it is some key value store shared between root and child contexts and allows keep global values, but can't share variables between async tasks. Can be null.
   * @param libs                  libraries to be added into context
   */
  public JProlContext(
      final String name,
      final File currentFolder,
      final Function<String, KnowledgeBase> knowledgeBaseSupplier,
      final KeyValueTermStore globalVariablesStore,
      final AbstractJProlLibrary... libs
  ) {
    this(
        name,
        currentFolder,
        knowledgeBaseSupplier,
        ForkJoinPool::commonPool,
        globalVariablesStore,
        libs
    );
  }

  /**
   * Create instance.
   *
   * @param name                    name of the context, must not be null
   * @param currentFolder           current file folder for IO  operations
   * @param knowledgeBaseSupplier   function gets context name and supplies instance of knowledge base, must not be null
   * @param executorServiceSupplier supplier of executor service for async tasks, must not be null
   * @param globalVariablesStore    global variable store, it is some key value store shared between root and child contexts and allows keep global values, but can't share variables between async tasks. Can be null.
   * @param libs                    libraries to be added into context
   */
  public JProlContext(
      final String name,
      final File currentFolder,
      final Function<String, KnowledgeBase> knowledgeBaseSupplier,
      final Supplier<ExecutorService> executorServiceSupplier,
      final KeyValueTermStore globalVariablesStore,
      final AbstractJProlLibrary... libs
  ) {
    this(
        null,
        name,
        currentFolder,
        knowledgeBaseSupplier.apply(name + "_kbase"),
        executorServiceSupplier,
        emptyMap(),
        emptyList(),
        emptyList(),
        emptySet(),
        emptyMap(),
        new ConcurrentHashMap<>(),
        globalVariablesStore,
        Arrays.asList(libs)
    );
  }

  /**
   * Create instance.
   *
   * @param name name of the context, must not be null
   * @param libs libraries to be added into context
   */
  public JProlContext(final String name, final AbstractJProlLibrary... libs) {
    this(name, ConcurrentInMemoryKnowledgeBase::new, libs);
  }

  /**
   * Create instance.
   *
   * @param name                  name of the context, must not be null
   * @param knowledgeBaseSupplier function gets context name and supplies instance of knowledge base, must not be null
   * @param libs                  libraries to be added into context
   */
  public JProlContext(final String name,
                      final Function<String, KnowledgeBase> knowledgeBaseSupplier,
                      final AbstractJProlLibrary... libs) {
    this(name, new File(System.getProperty("user.home")), knowledgeBaseSupplier, libs);
  }

  // for internal use to make clone of context
  private JProlContext(
      final JProlContext parentContext,
      final String contextId,
      final File currentFolder,
      final KnowledgeBase base,
      final Supplier<ExecutorService> executorServiceSupplier,
      final Map<JProlSystemFlag, Term> systemFlags,
      final List<JProlContextListener> contextListeners,
      final List<IoResourceProvider> ioProviders,
      final Set<String> dynamicSignatures,
      final Map<String, Map<JProlTriggerType, List<JProlContextTrigger>>> triggers,
      final Map<String, JProlNamedLock> namedLocks,
      final KeyValueTermStore globalVariablesStore,
      final List<AbstractJProlLibrary> addLibraries
  ) {
    this.payloads = parentContext == null ? new ConcurrentHashMap<>() : parentContext.payloads;
    this.globalVariablesStore = globalVariablesStore;
    this.namedLocks = namedLocks;
    this.parentContext = parentContext;
    this.contextId = requireNonNull(contextId, "Context Id is null");
    this.currentFolder = requireNonNull(currentFolder);
    this.knowledgeBase = requireNonNull(base, "Knowledge base is null");
    this.asyncTaskExecutorService = new LazyExecutorService(executorServiceSupplier);
    this.contextListeners.addAll(contextListeners);

    if (!triggers.isEmpty()) {
      triggers.forEach((k, v) -> {
        final Map<JProlTriggerType, List<JProlContextTrigger>> triggerMap =
            new ConcurrentHashMap<>();
        v.forEach((t, r) -> triggerMap.put(t, new CopyOnWriteArrayList<>(r)));
        this.triggers.put(k, triggerMap);
      });
    }

    Arrays.stream(JProlSystemFlag.values())
        .filter(x -> !x.isReadOnly())
        .forEach(x -> this.systemFlags.put(x, x.getDefaultValue()));

    this.systemFlags.putAll(systemFlags);
    this.onSystemFlagsUpdated();

    this.ioProviders.addAll(ioProviders);
    this.dynamicSignatures.addAll(dynamicSignatures);

    // we should not duplicate data in knowledge base through library consult if the base is shared with parent
    final boolean fullLibraryProcessingNeeded =
        this.parentContext == null || this.parentContext.knowledgeBase != this.knowledgeBase;

    this.addLibrary(BOOTSTRAP_LIBRARY, fullLibraryProcessingNeeded);
    addLibraries.stream()
        .filter(x -> x.getClass() != BOOTSTRAP_LIBRARY.getClass())
        .forEach(x -> this.addLibrary(x, fullLibraryProcessingNeeded));
  }

  private static void clearContextGlobalVariables(final JProlContext context) {
    final String contextGlobalVariablePrefix = ProlUtils.makeContextAwareGlobalValuePrefix(context);
    final KeyValueTermStore store = context.globalVariablesStore;
    if (store != null) {
      store.removeif((s, t) -> s.startsWith(contextGlobalVariablePrefix));
    }
  }

  /**
   * Find payload associated with a term in the context.
   *
   * @param term a term for which to find payload, can be null
   * @param <T>  type of expected payload
   * @return optional found value
   * @since 3.0.0
   */
  @SuppressWarnings("unchecked")
  public <T> Optional<T> findPayload(final Term term) {
    this.assertNotDisposed();
    if (term == null) {
      return Optional.empty();
    }
    return Optional.ofNullable((T) this.payloads.get(term));
  }

  /**
   * Set payload associated with a term or remove if null value.
   *
   * @param term  target term, must not be null
   * @param value associated value, if it is null then remove value
   * @since 3.0.0
   */
  public void setPayload(final Term term, final Object value) {
    this.assertNotDisposed();
    if (value == null) {
      this.payloads.remove(term);
    } else {
      this.payloads.put(term, value);
    }
  }

  /**
   * Get unmodifiable map of payloads for the context.
   *
   * @return unmodifable payload map, can't be null
   * @since 3.0.0
   */
  public Map<Term, Object> getPayloadsMap() {
    return Collections.unmodifiableMap(this.payloads);
  }

  /**
   * Create new string based choice point.
   *
   * @param goal the goal as a string, like "some(A,b)."
   * @return new choice point, must not be null
   * @throws PrologParserException if error during goal parse
   * @since 3.0.0
   */
  public JProlChoicePoint makeChoicePoint(final String goal) {
    return this.makeChoicePoint(goal, null);
  }

  /**
   * Create new string based choice point.
   *
   * @param goal                        the goal as a string, like "some(A,b)."
   * @param choicePointAssociatedObject any object which will be saved in choice point and can be used in predicates, can be null
   * @return new choice point, must not be null
   * @throws PrologParserException if error during goal parse
   * @since 3.0.0
   */
  public JProlChoicePoint makeChoicePoint(final String goal,
                                          final Object choicePointAssociatedObject) {
    return this.makeChoicePoint(goal, choicePointAssociatedObject, null);
  }

  /**
   * Create new string based choice point.
   *
   * @param goal                        the goal as a string, like "some(A,b)."
   * @param choicePointAssociatedObject any object which will be saved in choice point and can be used in predicates, can be null
   * @param predefinedVarValues         map of predefined variable values to be used for goal, can be null
   * @return new choice point, must not be null
   * @throws PrologParserException if error during goal parse
   * @since 3.0.0
   */
  public JProlChoicePoint makeChoicePoint(final String goal,
                                          final Object choicePointAssociatedObject,
                                          final Map<String, Term> predefinedVarValues) {
    final Term parsedGoal = new JProlTreeBuilder(
        this,
        new StringReader(goal),
        true
    ).readPhraseAndMakeTree();
    return this.makeChoicePoint(parsedGoal, choicePointAssociatedObject, predefinedVarValues);

  }

  /**
   * Create new string based choice point.
   *
   * @param goal the goal as a term, must not be null
   * @return new choice point, must not be null
   * @since 3.0.0
   */
  public JProlChoicePoint makeChoicePoint(final Term goal) {
    return this.makeChoicePoint(goal, null);
  }

  /**
   * Create new string based choice point.
   *
   * @param goal                        the goal as a term, must not be null
   * @param choicePointAssociatedObject any object which will be saved in choice point and can be used in predicates, can be null
   * @return new choice point, must not be null
   * @since 3.0.0
   */
  public JProlChoicePoint makeChoicePoint(final Term goal,
                                          final Object choicePointAssociatedObject) {
    return this.makeChoicePoint(goal, choicePointAssociatedObject, null);
  }

  /**
   * Create new string based choice point.
   *
   * @param goal                        the goal as a term, must not be null
   * @param choicePointAssociatedObject any object which will be saved in choice point and can be used in predicates, can be null
   * @param predefinedVarValues         map of predefined variable values to be used for goal, can be null
   * @return new choice point, must not be null
   * @since 3.0.0
   */
  public JProlChoicePoint makeChoicePoint(
      final Term goal,
      final Object choicePointAssociatedObject,
      final Map<String, Term> predefinedVarValues) {
    return new JProlChoicePoint(goal, this, predefinedVarValues, choicePointAssociatedObject);
  }

  /**
   * Method called for uncaught exception to not lost it.
   *
   * @param source exception source context, must not be null
   * @param error  caught error, must not be null
   * @since 3.0.0
   */
  protected void onUncaughtException(final JProlContext source, final Throwable error) {
    final JProlContext root = this.findRootContext();
    if (this.isRootContext()) {
      error.printStackTrace();
    } else {
      root.onUncaughtException(this, error);
    }
  }

  /**
   * Get flag that executor must be disposed on halt. By default returns false;
   *
   * @param source source context which make call, can't be null
   * @return true if executor should be disposed, false otherwise
   * @since 3.0.0
   */
  protected boolean isDisposeExecutorOnHalt(final JProlContext source) {
    return false;
  }

  /**
   * Get flag to dispose executor on close. False by default.
   *
   * @return true if executor must be disposed, false otherwise
   * @since 3.0.0
   */
  protected boolean isDisposeExecutorOnClose() {
    return false;
  }

  public boolean isShareKnowledgeBaseWithAsyncTasks() {
    return this.shareKnowledgeBaseWithAsyncTasks;
  }

  public boolean isRootContext() {
    return this.parentContext == null;
  }

  public JProlContext getParentContext() {
    return this.parentContext;
  }

  public File getCurrentFolder() {
    return this.currentFolder;
  }

  public boolean isVerify() {
    return this.verify;
  }

  public UndefinedPredicateBehavior getUndefinedPredicateBehavior() {
    return this.undefinedPredicateBehaviour;
  }

  public boolean isTrace() {
    return this.trace;
  }

  public Term getSystemFlag(final JProlSystemFlag flag) {
    return this.systemFlags.getOrDefault(flag, flag.getDefaultValue());
  }

  public void setSystemFlag(final JProlSystemFlag flag, final Term term) {
    this.assertNotDisposed();
    if (flag.isReadOnly()) {
      throw new IllegalStateException("Flag is marked as read-only: " + flag);
    } else {
      final Term value = requireNonNull(term.tryGroundOrDefault(null));
      this.systemFlags.put(flag, value);
      this.onSystemFlagsUpdated();
    }
  }

  private void onSystemFlagsUpdated() {
    this.shareKnowledgeBaseWithAsyncTasks =
        parseBoolean(this.systemFlags.get(JProlSystemFlag.SHARE_KNOWLEDGE_BASE).getText());
    this.verify =
        parseBoolean(this.systemFlags.get(JProlSystemFlag.VERIFY).getText());
    this.trace = parseBoolean(this.systemFlags.get(JProlSystemFlag.TRACE).getText());
    this.undefinedPredicateBehaviour = UndefinedPredicateBehavior
        .find(this.systemFlags.get(JProlSystemFlag.UNKNOWN).getText())
        .orElseThrow(() -> new ProlDomainErrorException(
            Arrays.toString(UndefinedPredicateBehavior.values()),
            this.systemFlags.get(JProlSystemFlag.UNKNOWN))
        );
  }

  public JProlContext addContextListener(final JProlContextListener listener) {
    this.assertNotDisposed();
    this.contextListeners.add(listener);
    return this;
  }

  public JProlContext removeContextListener(final JProlContextListener listener) {
    this.contextListeners.remove(listener);
    return this;
  }

  public JProlContext addIoResourceProvider(final IoResourceProvider provider) {
    this.assertNotDisposed();
    this.ioProviders.add(provider);
    return this;
  }

  public JProlContext removeIoResourceProvider(final IoResourceProvider provider) {
    this.ioProviders.remove(provider);
    return this;
  }

  public Optional<Reader> findResourceReader(final String readerId) {
    this.assertNotDisposed();
    return this.ioProviders.stream().map(x -> x.findReader(this, readerId)).filter(Objects::nonNull)
        .findFirst();
  }

  public Optional<Writer> findResourceWriter(final String writerId, final boolean append) {
    this.assertNotDisposed();
    return this.ioProviders.stream().map(x -> x.findWriter(this, writerId, append))
        .filter(Objects::nonNull).findFirst();
  }

  public String getName() {
    return this.contextId;
  }

  void fireTraceEvent(final TraceEvent event, final JProlChoicePoint choicePoint) {
    if (!this.contextListeners.isEmpty()) {
      this.contextListeners.forEach(l -> l.onChoicePointTraceEvent(this, choicePoint, event));
    }
  }

  public int getCurrentAsyncTaskNumber() {
    return this.startedAsyncTasks.size();
  }

  /**
   * Wait for specified time for all async task completed.
   *
   * @param time time to wait, can be null for unlimited wait
   * @return true if everything ok and all task completed, false if not completed during time
   * @throws InterruptedException if wait id interrupted
   * @since 3.0.0
   */
  public boolean waitAllAsyncTasks(final Duration time) throws InterruptedException {
    this.assertNotDisposed();
    this.asyncLocker.lock();
    try {
      final long maxTime =
          time == null ? Long.MAX_VALUE : System.currentTimeMillis() + time.toMillis();
      while (!this.startedAsyncTasks.isEmpty() && !Thread.currentThread().isInterrupted()) {
        if (System.currentTimeMillis() >= maxTime) {
          break;
        }
        try {
          if (time == null) {
            this.asyncCounterCondition.await();
          } else {
            this.asyncCounterCondition.await(Math.max(maxTime - System.currentTimeMillis(), 0L),
                TimeUnit.MILLISECONDS);
          }
        } catch (InterruptedException ex) {
          Thread.currentThread().interrupt();
          throw ex;
        }
      }
      return this.startedAsyncTasks.isEmpty() && !Thread.currentThread().isInterrupted();
    } finally {
      this.asyncLocker.unlock();
    }
  }

  private void signalAllConditions() {
    this.asyncLocker.lock();
    try {
      this.asyncCounterCondition.signalAll();
    } finally {
      this.asyncLocker.unlock();
    }
  }

  private void onAsyncTaskCompleted(
      final long id,
      final JProlContext copyContext,
      final JProlContextListener contextListener,
      final Throwable error) {
    final List<JProlContextListener> listeners = List.copyOf(this.contextListeners);
    try {
      this.removeContextListener(contextListener);
      if (error != null && !listeners.isEmpty()) {
        if (!(error instanceof ProlChoicePointInterruptedException)) {
          listeners.forEach(x -> {
            try {
              if (error instanceof ProlAbortExecutionException) {
                x.onAsyncTaskAborted(this, copyContext, id, (ProlAbortExecutionException) error);
              } else {
                x.onAsyncUncaughtTaskException(this, copyContext, id, error);
              }
            } catch (Exception ex) {
              this.onUncaughtException(this, ex);
            }
          });
        }
      }
    } finally {
      this.startedAsyncTasks.remove(id);
      this.signalAllConditions();
      copyContext.dispose();
    }
  }

  private void assertConcurrentKnowledgeBase() {
    if (!this.knowledgeBase.isConcurrent()) {
      throw new ProlKnowledgeBaseException("Async operations requires concurrent knowledge base");
    }
  }

  /**
   * Prove the goal asynchronously, all variants.
   *
   * @param goal                        the target goal
   * @param choicePointAssociatedObject any object to be associated with created choice points, can be null
   * @param shareKnowledgeBase          if true then make copy of
   * @return a future contains counter of successful goal prove
   */
  public CompletableFuture<Term> asyncProveAll(final Term goal,
                                               final Object choicePointAssociatedObject,
                                               final boolean shareKnowledgeBase) {
    this.assertNotDisposed();
    this.assertConcurrentKnowledgeBase();

    final long taskId = TASK_COUNTER.incrementAndGet();
    final JProlContext contextCopy = this.copyInstance(shareKnowledgeBase, false);
    final JProlContextListener rootContextListener = new JProlContextListener() {
      @Override
      public void onContextDispose(final JProlContext source) {
        source.removeContextListener(this);
        Thread.currentThread().interrupt();
        contextCopy.dispose();
      }
    };
    this.addContextListener(rootContextListener);

    return this.startedAsyncTasks.computeIfAbsent(taskId, id -> {
      final CompletableFuture<Term> future = CompletableFuture.supplyAsync(() -> {
        Throwable error = null;
        try {
          final JProlChoicePoint asyncGoal =
              contextCopy.makeChoicePoint(requireNonNull(goal), choicePointAssociatedObject);
          int proved = 0;
          while (!this.isDisposed() && !contextCopy.isDisposed()) {
            if (asyncGoal.prove() == null) {
              break;
            } else {
              proved++;
            }
          }

          if (!contextCopy.isDisposed()) {
            contextCopy.dispose();
          }
          return Terms.newLong(proved);
        } catch (Throwable ex) {
          error = ex;

          if (ex instanceof ProlHaltExecutionException) {
            this.notifyHalt();
          }

          if (!(ex instanceof CompletionException)
              || !(ex.getCause() instanceof ProlInterruptException)) {
            throw new ProlForkExecutionException("Error during async/1", goal,
                new Throwable[] {ex});
          } else {
            throw ex;
          }
        } finally {
          this.onAsyncTaskCompleted(id, contextCopy, rootContextListener, error);
        }
      }, this.asyncTaskExecutorService);

      this.contextListeners.forEach(x -> {
        try {
          x.onAsyncTaskStarted(JProlContext.this, contextCopy, taskId, future);
        } catch (Throwable ex) {
          this.onUncaughtException(this, ex);
        }
      });

      return future;
    });
  }

  /**
   * Prove the goal asynchronously, only once.
   *
   * @param goal                        the target goal
   * @param choicePointAssociatedObject any object to be associated with created choice points, can be null
   * @param shareKnowledgeBase          if true then make copy of
   * @return a future contains result term of prove
   */
  public CompletableFuture<Term> asyncProveOnce(final Term goal,
                                                final Object choicePointAssociatedObject,
                                                final boolean shareKnowledgeBase) {
    this.assertNotDisposed();
    this.assertConcurrentKnowledgeBase();

    final long taskId = TASK_COUNTER.incrementAndGet();

    final JProlContext contextCopy = this.copyInstance(shareKnowledgeBase, false);
    final JProlContextListener rootContextListener = new JProlContextListener() {
      @Override
      public void onContextDispose(final JProlContext source) {
        source.removeContextListener(this);
        Thread.currentThread().interrupt();
        contextCopy.dispose();
      }
    };

    this.addContextListener(rootContextListener);
    return this.startedAsyncTasks.computeIfAbsent(taskId, id -> {
      final CompletableFuture<Term> future = CompletableFuture.supplyAsync(() -> {
        Throwable error = null;
        try {
          final JProlChoicePoint asyncGoal =
              contextCopy.makeChoicePoint(requireNonNull(goal), choicePointAssociatedObject);

          final Term result = asyncGoal.prove();
          asyncGoal.resetLogicalAlternativesFlag();
          return result;
        } catch (Throwable ex) {

          if (ex instanceof ProlHaltExecutionException) {
            this.notifyHalt();
          }

          error = ex;
          if (!(ex instanceof CompletionException) ||
              !(ex.getCause() instanceof ProlInterruptException)) {
            throw new ProlForkExecutionException("Error during async/1", goal,
                new Throwable[] {ex});
          } else {
            throw ex;
          }
        } finally {
          this.onAsyncTaskCompleted(id, contextCopy, rootContextListener, error);
        }
      }, this.asyncTaskExecutorService);

      this.contextListeners.forEach(x -> {
        try {
          x.onAsyncTaskStarted(JProlContext.this, contextCopy, taskId, future);
        } catch (Throwable ex) {
          this.onUncaughtException(this, ex);
        }
      });

      return future;
    });
  }

  public ExecutorService getAsyncTaskExecutorService() {
    this.assertNotDisposed();
    return this.asyncTaskExecutorService;
  }

  private Optional<JProlNamedLock> findLockerForId(final String lockerId,
                                                   final boolean createIfAbsent) {
    if (createIfAbsent) {
      return Optional.of(this.namedLocks.computeIfAbsent(lockerId, s -> new JProlNamedLock()));
    } else {
      return Optional.ofNullable(this.namedLocks.get(lockerId));
    }
  }

  public void lockFor(final String lockId) throws InterruptedException {
    this.assertNotDisposed();
    try {
      this.findLockerForId(lockId, true)
          .orElseThrow(
              () -> new IllegalArgumentException("Named locker is not presented: " + lockId))
          .acquire();
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
      throw new InterruptedException("Locker wait has been interrupted: " + lockId);
    }
  }

  public boolean tryLockFor(final String lockId) {
    this.assertNotDisposed();
    return this.findLockerForId(lockId, true).orElseThrow(
            () -> new IllegalArgumentException("Named locker is not presented: " + lockId))
        .tryAcquire();
  }

  public void unlockFor(final String lockId) {
    this.findLockerForId(lockId, false).ifPresent(JProlNamedLock::release);
  }

  private void assertNotDisposed() {
    if (this.isDisposed()) {
      throw new ProlException("Context is disposed: " + this.contextId);
    }
  }

  /**
   * Add and register a library in context, the library will be fully processed and all internal library consult completed.
   *
   * @param library library to be added, must not be null
   * @return true if added, false otherwise
   * @throws NullPointerException if library object is null
   */
  public boolean addLibrary(final AbstractJProlLibrary library) {
    return this.addLibrary(library, true);
  }

  /**
   * Add library into context. If library class already presented then library will be ignored.
   *
   * @param library        library to be added, must not be null
   * @param fullProcessing if true then make fully search of consult annotations in library and process them, if false then library just added into internal store
   * @return true if successfully added, false if library class already presented in internal store
   * @throws NullPointerException if library object is null
   * @since 3.0.0
   */
  public boolean addLibrary(final AbstractJProlLibrary library, final boolean fullProcessing) {
    assertNotDisposed();
    if (library == null) {
      throw new NullPointerException("Library is null");
    }

    if (this.libraries.stream().anyMatch(x -> x.getClass() == library.getClass())) {
      return false;
    }

    this.libraries.add(0, library);
    if (fullProcessing) {
      final StringBuilder consultBuffer = new StringBuilder();

      final JProlConsultText consultText = library.getClass().getAnnotation(JProlConsultText.class);
      if (consultText != null) {
        final String text = String.join("\n", consultText.value());
        if (!text.isEmpty()) {
          consultBuffer.append(text);
        }
      }

      final JProlConsultFile consultFile = library.getClass().getAnnotation(JProlConsultFile.class);
      if (consultFile != null) {
        final String resourceText = Arrays.stream(consultFile.value())
            .filter(x -> !(x == null || x.trim().isEmpty()))
            .map(File::new)
            .map(x -> {
              try {
                return ProlUtils.readAsUtf8(x);
              } catch (IOException ex) {
                throw new RuntimeIOException("Can't read file: " + x, ex);
              }
            })
            .collect(Collectors.joining("\n"));
        consultBuffer.append(resourceText);
      }

      final JProlConsultUrl consultUrls =
          library.getClass().getAnnotation(JProlConsultUrl.class);
      if (consultUrls != null) {
        final String resourceText = Arrays.stream(consultUrls.value())
            .filter(x -> !(x == null || x.trim().isEmpty()))
            .map(x -> {
              final URL resourceUrl;
              try {
                resourceUrl = URI.create(x).toURL();
              } catch (MalformedURLException ex) {
                throw new IllegalArgumentException("Malformed URL: " + x, ex);
              }
              return resourceUrl;
            })
            .map(x -> {
              final StringBuilder buffer = new StringBuilder();
              URLConnection connection;
              try {
                connection = x.openConnection();
                try (final Reader reader = new InputStreamReader(connection.getInputStream(),
                    StandardCharsets.UTF_8)) {
                  while (!this.isDisposed()) {
                    final int value = reader.read();
                    if (value < 0) {
                      break;
                    }
                    buffer.append((char) value);
                  }
                }
              } catch (IOException ex) {
                throw new RuntimeIOException("Can't read resource for IO error", ex);
              }
              return buffer.toString();
            })
            .collect(Collectors.joining("\n"));
        consultBuffer.append(resourceText);
      }

      final JProlConsultResource consultResource =
          library.getClass().getAnnotation(JProlConsultResource.class);
      if (consultResource != null) {
        final String resourceText = Arrays.stream(consultResource.value())
            .filter(x -> !(x == null || x.trim().isEmpty()))
            .map(x -> {
              final InputStream stream = library.getClass().getResourceAsStream(x);
              if (stream == null) {
                throw new NullPointerException("Can't find resource: " + x);
              }
              return stream;
            })
            .map(x -> {
              final StringBuilder buffer = new StringBuilder();
              try (final Reader reader = new InputStreamReader(x, StandardCharsets.UTF_8)) {
                while (!this.isDisposed()) {
                  final int value = reader.read();
                  if (value < 0) {
                    break;
                  }
                  buffer.append((char) value);
                }
              } catch (IOException ex) {
                throw new RuntimeIOException("Can't read resource for IO error", ex);
              }
              return buffer.toString();
            })
            .collect(Collectors.joining("\n"));
        consultBuffer.append(resourceText);
      }

      if (consultBuffer.length() > 0) {
        this.consult(new StringReader(consultBuffer.toString()), null, false);
      }
    }

    this.contextListeners.forEach(y -> {
      try {
        y.onLibraryAdded(this, library);
      } catch (Exception ex) {
        this.onUncaughtException(this, ex);
      }
    });

    return true;
  }

  public KnowledgeBase getKnowledgeBase() {
    return this.knowledgeBase;
  }

  public boolean removeLibrary(final AbstractJProlLibrary library) {
    this.assertNotDisposed();

    if (library == null) {
      throw new IllegalArgumentException("Library must not be null");
    }
    final boolean result = this.libraries.remove(library);
    if (result) {
      this.contextListeners.forEach(x -> x.onLibraryRemoved(this, library));
    }
    return result;
  }

  public PredicateInvoker findProcessor(final TermStruct predicate) {
    return this.libraries
        .stream()
        .map(lib -> lib.findPredicateProcessor(predicate))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(NULL_INVOKER);
  }

  public boolean hasZeroArityPredicateForName(final String name) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.hasZeroArityPredicate(name));
  }

  public List<TermStruct> findAllForPredicateIndicatorInLibs(final Term predicateIndicator) {
    return this.libraries.stream()
        .flatMap(lib -> lib.findAllForPredicateIndicator(predicateIndicator).stream())
        .collect(toList());
  }

  public List<PredicateInvoker> findAllPredicateInvokersForSignature(final String signature) {
    return this.libraries.stream()
        .flatMap(lib -> lib.findAllPredicateInvokersForSignature(signature).stream())
        .collect(toList());
  }

  public boolean hasPredicateAtLibraryForSignature(final String signature) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.hasPredicateForSignature(signature));
  }

  public boolean isSystemOperator(final String name) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.isSystemOperator(name));
  }

  public TermOperatorContainer findSystemOperatorForName(final String name) {
    return this.libraries.stream()
        .map(lib -> lib.findSystemOperatorForName(name))
        .filter(Objects::nonNull)
        .findFirst().orElse(null);
  }

  public TermOperator findSystemOperatorForNameAndAssociativity(final String name,
                                                                final OpAssoc associativity) {
    return this.libraries.stream()
        .map(lib -> lib.findSystemOperatorForName(name))
        .filter(Objects::nonNull)
        .map(x -> x.getForExactType(associativity))
        .filter(Objects::nonNull)
        .findFirst().orElse(null);
  }

  public Iterator<AbstractJProlLibrary> makeLibraryIterator() {
    this.assertNotDisposed();
    return this.libraries.iterator();
  }

  public boolean hasSystemOperatorStartsWith(final String str) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.isSystemOperatorStartsWith(str));
  }

  private boolean waitStartedTasksCompletion(final Duration timeout) {
    this.asyncLocker.lock();
    try {
      final long endTime = System.currentTimeMillis() + timeout.toMillis();
      while (!this.startedAsyncTasks.isEmpty()) {
        final long delay = endTime - System.currentTimeMillis();
        if (delay <= 0L) {
          break;
        }
        this.asyncCounterCondition.await(delay, TimeUnit.MILLISECONDS);
      }
      return this.startedAsyncTasks.isEmpty();
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
      return this.startedAsyncTasks.isEmpty();
    } finally {
      this.asyncLocker.unlock();
    }
  }

  public void cancelAllAsyncTasks() {
    this.assertNotDisposed();
    this.startedAsyncTasks.values().forEach(x -> x.cancel(true));
  }

  public void dispose() {
    this.dispose(false);
  }

  public void dispose(final boolean disposeExecutor) {
    if (this.disposed.compareAndSet(false, true)) {
      try {
        this.contextListeners.forEach(listener -> {
          try {
            listener.onContextDispose(this);
          } catch (Exception ex) {
            this.onUncaughtException(this, ex);
          }
        });

        this.startedAsyncTasks.values().forEach(x -> x.cancel(true));

        if (this.isRootContext()) {
          this.namedLocks.forEach((key, value) -> {
            try {
              value.release();
            } catch (Exception ex) {
              this.onUncaughtException(this, ex);
            }
          });
          this.namedLocks.clear();
        }

        if (this.isRootContext() && disposeExecutor) {
          this.asyncTaskExecutorService.shutdown();
        }
        this.signalAllConditions();
        this.waitStartedTasksCompletion(Duration.ofSeconds(15));
        this.startedAsyncTasks.clear();

        this.triggers.values().stream()
            .flatMap(x -> x.values().stream())
            .flatMap(Collection::stream)
            .distinct()
            .forEach(x -> {
              try {
                x.onContextDispose(this);
              } catch (Exception ex) {
                this.onUncaughtException(this, ex);
              }
            });
        this.triggers.clear();

        this.libraries.forEach(library -> {
          try {
            library.onContextDispose(this);
          } catch (Throwable ex) {
            this.onUncaughtException(this, ex);
          } finally {
            if (this.isRootContext()) {
              try {
                library.release();
              } catch (Throwable ex) {
                this.onUncaughtException(this, ex);
              }
            }
          }
        });
        final List<AbstractJProlLibrary> copyOfLibraries = List.copyOf(this.libraries);
        this.libraries.clear();
        if (!copyOfLibraries.isEmpty() && !this.contextListeners.isEmpty()) {
          this.contextListeners.forEach(x -> copyOfLibraries.forEach(y -> {
            try {
              x.onLibraryRemoved(this, y);
            } catch (Exception ex) {
              this.onUncaughtException(this, ex);
            }
          }));
        }
        this.contextListeners.clear();

        if (this.isRootContext()) {
          this.payloads.clear();

          // must be in the end because this thread also can be executed in the executor.
          if (disposeExecutor) {
            this.asyncTaskExecutorService.shutdownNow();
            this.signalAllConditions();
            try {
              Method methodClose = null;
              try {
                methodClose = this.asyncTaskExecutorService.getClass().getMethod("close");
              } catch (NoSuchMethodException ex) {
                // ignore
              }
              if (methodClose != null) {
                methodClose.invoke(this.asyncTaskExecutorService);
              }
            } catch (Throwable ex) {
              this.onUncaughtException(this, ex);
            }
          }
        }
      } finally {
        clearContextGlobalVariables(this);
      }
    }
  }

  public boolean isDisposed() {
    return this.disposed.get();
  }

  public void addTrigger(final JProlContextTrigger trigger) {
    assertNotDisposed();
    trigger.getSignatures().forEach((signature, types) -> {
      String validatedSignature = ProlUtils.reassembleSignatureOrNull(signature);
      if (validatedSignature == null) {
        throw new IllegalArgumentException("Illegal signature format: " + signature);
      }
      validatedSignature = ProlUtils.normalizeSignature(validatedSignature);
      final Map<JProlTriggerType, List<JProlContextTrigger>> map =
          this.triggers.computeIfAbsent(validatedSignature, key -> new ConcurrentHashMap<>());
      types.forEach(x -> map.computeIfAbsent(x, key -> new CopyOnWriteArrayList<>()).add(trigger));
    });
  }

  public void removeTrigger(final JProlContextTrigger trigger) {
    this.assertNotDisposed();
    this.triggers.values()
        .forEach(map -> map.values().forEach(x -> x.removeIf(next -> next == trigger)));
  }

  public boolean removeTrigger(final String signature) {
    this.assertNotDisposed();
    return this.triggers.remove(signature) != null;
  }

  public boolean hasTrigger(final String signature,
                            final JProlTriggerType triggerType) {
    this.assertNotDisposed();
    final Map<JProlTriggerType, List<JProlContextTrigger>> map = this.triggers.get(signature);
    return map != null && map.containsKey(triggerType);
  }

  public void notifyAboutUndefinedPredicate(final JProlChoicePoint choicePoint,
                                            final String signature, final Term term) {
    this.assertNotDisposed();
    switch (this.getUndefinedPredicateBehavior()) {
      case ERROR: {
        if (!this.dynamicSignatures.contains(signature)) {
          throw new ProlExistenceErrorException("predicate",
              "Undefined predicate: " + signature, term);
        }
      }
      break;
      case WARNING: {
        this.contextListeners
            .forEach(x -> x.onUndefinedPredicateWarning(this, choicePoint, signature));
      }
      break;
      case FAIL: {
        //nothing
      }
      break;
      default:
        throw new Error("Unexpected behavior: " + this.getUndefinedPredicateBehavior());
    }
  }

  public void notifyTriggersForSignature(final String signature,
                                         final Term struct,
                                         final JProlTriggerType triggerType) {
    this.assertNotDisposed();

    final Map<JProlTriggerType, List<JProlContextTrigger>> triggerMap =
        this.triggers.get(signature);
    if (triggerMap != null && triggerMap.containsKey(triggerType)) {
      final List<JProlContextTrigger> triggers = triggerMap.get(triggerType);
      if (triggers != null && !triggers.isEmpty()) {
        final TriggerEvent triggerEvent = new TriggerEvent(this, struct, signature, triggerType);
        triggers.forEach(x -> x.onTriggerEvent(triggerEvent));
      }
    }
  }

  /**
   * Consult script provided as String
   *
   * @param script script, must not be null
   * @since 3.0.0
   */
  public void consult(final String script) {
    this.consult(new StringReader(script));
  }

  /**
   * Consult script provided as a Reader
   *
   * @param reader script reader, must not be null
   */
  public void consult(final Reader reader) {
    this.consult(reader, null);
  }

  public Set<String> findDynamicSignatures() {
    this.assertNotDisposed();
    return new LazySet<>(this.dynamicSignatures);
  }

  public void addDynamicSignatures(final Set<String> signatures,
                                   final SourcePosition sourcePosition) {
    this.assertNotDisposed();

    for (final String signature : signatures) {
      if (signature == null) {
        throw new NullPointerException();
      }
      this.assertSignatureNotStaticallyPresented(signature, sourcePosition);
    }

    this.dynamicSignatures.addAll(signatures);
  }

  protected void assertSignatureNotStaticallyPresented(final String signature,
                                                       final SourcePosition sourcePosition) {
    if (this.getSystemFlag(JProlSystemFlag.ALLOW_LIBRARY_SIGNATURE_CONFLICT) == FALSE) {
      if (this.hasPredicateAtLibraryForSignature(signature)) {
        final String libraryName =
            this.libraries.stream().filter(x -> x.hasPredicateForSignature(signature)).findFirst()
                .map(x -> x.getLibraryUid() + " (" + x.getClass().getCanonicalName() + ')')
                .orElse("<UNKNOWN>");
        throw new ProlPermissionErrorException("modify", "static_procedure",
            "Signature '" + signature + "'is statically defined in " + libraryName + " library",
            newAtom(signature, Objects.requireNonNullElse(sourcePosition, UNKNOWN)));
      }
    }
  }

  public void clearDynamicSignatures() {
    this.assertNotDisposed();
    this.dynamicSignatures.clear();
  }

  public boolean isDynamicSignature(final String termSignature) {
    return this.dynamicSignatures.contains(termSignature);
  }

  private boolean doClauseInKnowledgeBase(final Term term,
                                          final boolean expectedIndicator,
                                          final BiFunction<String, TermStruct, Boolean> processingFunction,
                                          final boolean falseOnViolation) {
    this.assertNotDisposed();
    final boolean operationResult;
    final String signature;
    final TermStruct termStruct;

    if (expectedIndicator) {
      signature = ProlUtils.indicatorAsStringOrNull(term);
      if (signature == null) {
        throw new ProlDomainErrorException("indicator", term);
      }
      termStruct = (TermStruct) term;
    } else {
      if (term.getTermType() == STRUCT) {
        termStruct = (TermStruct) term;
      } else {
        ProlAssertions.assertCallable(term);
        termStruct = newStruct(term);
      }
      signature = termStruct.isClause() ?
          termStruct.getArgumentAt(0).getSignature() : termStruct.getSignature();
    }

    this.assertSignatureNotStaticallyPresented(signature, term.getSourcePosition());

    if (this.dynamicSignatures.contains(signature)) {
      operationResult = processingFunction.apply(signature, termStruct);
    } else {
      if (falseOnViolation) {
        return false;
      }
      throw new ProlPermissionErrorException("modify", "static_procedure",
          "Allowed only for dynamic predicates " + signature,
          newAtom(signature, term.getSourcePosition()));
    }
    return operationResult;
  }

  public boolean assertA(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> knowledgeBase.assertA(this, struct), false);
  }

  public boolean assertZ(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> this.knowledgeBase.assertZ(this, struct), false);
  }

  public boolean retractAll(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> this.knowledgeBase.retractAll(this, struct), true);
  }

  public boolean abolish(final Term term) {
    final Term indicator = term.tryGround();
    ProlAssertions.assertIndicator(indicator);
    return this.doClauseInKnowledgeBase(term,
        true,
        (signature, struct) -> this.knowledgeBase.abolish(this, signature), false);
  }

  public boolean retractA(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> this.knowledgeBase.retractA(this, struct), false);
  }

  public boolean retractZ(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> this.knowledgeBase.retractZ(this, struct), false);
  }

  public void consult(final String script, final QueryInteractor queryInteractor) {
    this.consult(new StringReader(script), queryInteractor);
  }

  public void consult(final Reader source, final QueryInteractor queryInteractor) {
    this.consult(source, queryInteractor, true);
  }

  protected void consult(
      final Reader source,
      final QueryInteractor queryInteractor,
      final boolean checkLibraryConflicts
  ) {
    try (final JProlTreeBuilder treeBuilder = new JProlTreeBuilder(this, source, false)) {
      do {
        final Term nextItem = treeBuilder.readPhraseAndMakeTree();
        if (nextItem == null) {
          break;
        }

        final SourcePosition sourcePosition = nextItem.getSourcePosition();

        try {
          switch (nextItem.getTermType()) {
            case ATOM: {
              if (checkLibraryConflicts) {
                this.assertSignatureNotStaticallyPresented(nextItem.getSignature(), sourcePosition);
              }
              this.knowledgeBase.assertZ(this, newStruct(nextItem));
            }
            break;
            case STRUCT: {
              final TermStruct struct = (TermStruct) nextItem;
              final Term functor = struct.getFunctor();

              if (functor.getTermType() == TermType.OPERATOR) {
                final TermOperator op = (TermOperator) functor;
                final String text = op.getText();
                final OpAssoc type = op.getType();

                if (struct.isClause()) {
                  switch (type) {
                    case XFX: {
                      // new rule
                      final Term head = struct.getArgumentAt(0);
                      if (checkLibraryConflicts) {
                        this.assertSignatureNotStaticallyPresented(head.getSignature(),
                            head.getSourcePosition());
                      }
                      this.knowledgeBase.assertZ(this, struct);
                    }
                    break;
                    case FX: {
                      // directive
                      if (!this.processDirective(struct.getArgumentAt(0))) {
                        throw new ProlHaltExecutionException(2);
                      }
                    }
                    break;
                  }

                } else if (QUERY.equals(text)) {
                  final Term query = struct.getArgumentAt(0);
                  if (queryInteractor != null && queryInteractor.isQueryAllowed(this, query)) {
                    final Map<String, TermVar> variableMap = new LazyMap<>();
                    final AtomicInteger solutionCounter = new AtomicInteger();

                    final JProlChoicePoint thisGoal = this.makeChoicePoint(query);

                    boolean doFindNextSolution;
                    do {
                      variableMap.clear();
                      if (prove(thisGoal, variableMap)) {
                        doFindNextSolution = queryInteractor
                            .onQuerySuccess(this, query, variableMap,
                                solutionCounter.incrementAndGet());
                        if (!doFindNextSolution) {
                          throw new ProlHaltExecutionException("proving has been halted", 1,
                              sourcePosition);
                        }
                      } else {
                        queryInteractor.onQueryFail(this, query, solutionCounter.get());
                        doFindNextSolution = false;
                      }
                    } while (doFindNextSolution);
                  }
                } else {
                  if (checkLibraryConflicts) {
                    assertSignatureNotStaticallyPresented(struct.getSignature(),
                        struct.getSourcePosition());
                  }
                  this.knowledgeBase.assertZ(this, struct);
                }
              } else {
                if (checkLibraryConflicts) {
                  assertSignatureNotStaticallyPresented(struct.getSignature(),
                      struct.getSourcePosition());
                }
                this.knowledgeBase.assertZ(this, struct);
              }
            }
            break;
            default: {
              throw new ProlKnowledgeBaseException(
                  "Such element can't be saved at knowledge base [" + nextItem + ']');
            }
          }
        } catch (ProlChoicePointInterruptedException | ProlHaltExecutionException ex) {
          throw ex;
        } catch (ProlAbstractCatchableException ex) {
          final SourcePosition errorPosition = ex.getSourcePosition();

          String message = ex.getCause() == null ? ex.getMessage() : ex.getCause().getMessage();
          if (message == null) {
            message = ex.getClass().getSimpleName();
          }

          throw new PrologParserException(
              message,
              errorPosition.getLine(),
              errorPosition.getPosition(),
              ex
          );
        } catch (Exception ex) {
          throw new PrologParserException(
              ex.getCause() == null ? ex.getMessage() : ex.getCause().getMessage(),
              sourcePosition.getLine(), sourcePosition.getPosition(), ex);
        }
      } while (!this.isDisposed());
    }
  }

  private boolean prove(final JProlChoicePoint goalCp, final Map<String, TermVar> varTable) {
    final Term result = goalCp.prove();

    if (result != null && varTable != null) {
      result.variables().forEach(e -> varTable.put(e.getText(), e));
    }

    return result != null;
  }

  private boolean processDirective(final Term directive) {
    final JProlChoicePoint goal = this.makeChoicePoint(directive);
    return goal.prove() != null;
  }

  @Override
  public String toString() {
    return "ProlContext(" + contextId + ')' + '[' + super.toString() + ']';
  }

  /**
   * Check that a guarded predicate allowed, a predicate like consult/1 or abolish/1 so mainly working with IO, threads and changing knowledge base.
   *
   * @param sourceLibrary      source library class where the predicate is defined
   * @param choicePoint        choice point where the predicate is called
   * @param predicateIndicator predicate indicator
   * @return true if allowed false otherwise
   * @since 3.0.0
   */
  public boolean isGuardPredicateAllowed(
      final Class<? extends AbstractJProlLibrary> sourceLibrary,
      final JProlChoicePoint choicePoint,
      final String predicateIndicator
  ) {
    return true;
  }

  /**
   * Make context copy.
   *
   * @param shareKnowledgeBase   if true then share the knowledge base with the new instance, make copy otherwise.
   * @param copyContextListeners make copy of context listeners
   * @return copy of the current context state
   * @since 3.0.0
   */
  public JProlContext copyInstance(
      final boolean shareKnowledgeBase,
      final boolean copyContextListeners
  ) {
    return new JProlContext(
        this,
        this.contextId + "::copy" + System.nanoTime(),
        this.currentFolder,
        shareKnowledgeBase ? this.knowledgeBase : this.knowledgeBase.makeCopy(),
        this.asyncTaskExecutorService.getSupplier(),
        this.systemFlags,
        copyContextListeners ? this.contextListeners : List.of(),
        this.ioProviders,
        this.dynamicSignatures,
        this.triggers,
        this.namedLocks,
        this.globalVariablesStore,
        this.libraries
    );
  }

  public ParserContext makeParserContext() {
    return new ParserContext() {
      @Override
      public boolean hasOpStartsWith(final PrologParser prologParser, final String s) {
        return JProlContext.this.knowledgeBase.hasOperatorStartsWith(JProlContext.this, s);
      }

      @Override
      public OpContainer findOpForName(final PrologParser prologParser, final String s) {
        final TermOperatorContainer container =
            JProlContext.this.knowledgeBase.findOperatorForName(JProlContext.this, s);
        return container == null ? null : container.asOpContainer();
      }

      @Override
      public int getFlags() {
        return ParserContext.FLAG_ZERO_STRUCT
            | ParserContext.FLAG_ZERO_QUOTATION_CHARCODE
            | ParserContext.FLAG_ZERO_QUOTATION_ALLOWS_WHITESPACE_CHAR
            | ParserContext.FLAG_BLOCK_COMMENTS;
      }
    };
  }

  @Override
  public void close() {
    this.dispose(this.isDisposeExecutorOnClose());
  }

  /**
   * Send asynchronous signal tp halt root context.
   */
  public void notifyHalt() {
    final JProlContext rootContext = this.findRootContext();
    if (!rootContext.isDisposed()) {
      new Thread(() -> {
        if (!rootContext.isDisposed()) {
          rootContext.dispose(rootContext.isDisposeExecutorOnHalt(this));
        }
      }, "halt-jprol-context-" + System.identityHashCode(rootContext)).start();
    }
  }


  /**
   * Find the root context for the context, if it is the root then it will be returned as the result.
   *
   * @return the root context, must not be null
   * @since 3.0.0
   */
  public JProlContext findRootContext() {
    JProlContext result = this;
    while (!result.isRootContext()) {
      result = result.getParentContext();
    }
    return result;
  }

  /**
   * Get the global variables store. Child contexts use the root store.
   *
   * @return internal name-term store if it was provided during context create
   * @since 3.0.0
   */
  public Optional<KeyValueTermStore> getGlobalVariablesStore() {
    return Optional.ofNullable(this.globalVariablesStore);
  }

  private static final class JProlNamedLock {
    private final Semaphore semaphore = new Semaphore(1);
    private final AtomicLong ownerThreadId = new AtomicLong(-1L);

    public void acquire() throws InterruptedException {
      final long id = Thread.currentThread().getId();
      if (ownerThreadId.get() == id) {
        return;
      }
      this.semaphore.acquire();
      this.ownerThreadId.set(id);
    }

    public void release() {
      if (this.ownerThreadId.compareAndSet(Thread.currentThread().getId(), -1L)) {
        this.semaphore.release();
      }
    }

    public boolean tryAcquire() {
      final long id = Thread.currentThread().getId();
      if (ownerThreadId.get() == id) {
        return true;
      }
      if (this.semaphore.tryAcquire()) {
        this.ownerThreadId.set(id);
        return true;
      }
      return false;
    }
  }
}
