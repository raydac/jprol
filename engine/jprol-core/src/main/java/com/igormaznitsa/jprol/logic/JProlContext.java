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
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.logic.PredicateInvoker.NULL_PROCESSOR;
import static java.util.Arrays.asList;
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
import com.igormaznitsa.jprol.logic.triggers.JProlTrigger;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggerType;
import com.igormaznitsa.jprol.logic.triggers.TriggerEvent;
import com.igormaznitsa.jprol.trace.JProlContextListener;
import com.igormaznitsa.jprol.trace.TraceEvent;
import com.igormaznitsa.jprol.utils.LazyMap;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.ProlUtils;
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
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
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
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

public class JProlContext implements AutoCloseable {
  private final String contextId;

  private final Map<String, Map<JProlTriggerType, List<JProlTrigger>>> triggers =
      new ConcurrentHashMap<>();
  private final Map<String, ReentrantLock> namedLockers = new ConcurrentHashMap<>();
  private final List<AbstractJProlLibrary> libraries = new CopyOnWriteArrayList<>();
  private final AtomicBoolean disposed = new AtomicBoolean(false);
  private final KnowledgeBase knowledgeBase;
  private final ExecutorService asyncTaskExecutorService;
  private final List<JProlContextListener> contextListeners = new CopyOnWriteArrayList<>();
  private final Map<JProlSystemFlag, Term> systemFlags = new ConcurrentHashMap<>();
  private final AtomicInteger asyncTaskCounter = new AtomicInteger();
  private final Set<String> dynamicSignatures = new ConcurrentSkipListSet<>();

  private final ReentrantLock asyncLocker = new ReentrantLock();
  private final Condition asyncCounterCondition = asyncLocker.newCondition();

  private final ParserContext parserContext = new ParserContext() {
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

  private final List<IoResourceProvider> ioProviders = new CopyOnWriteArrayList<>();
  private final File currentFolder;
  private final JProlContext parentContext;
  private boolean templateValidate;
  private boolean debug;
  private boolean shareKnowledgeBaseBetweenThreads;
  private UndefinedPredicateBehavior undefinedPredicateBehaviour;

  public JProlContext(
      final String name,
      final File currentFolder,
      final AbstractJProlLibrary... libs
  ) {
    this(name, currentFolder, ConcurrentInMemoryKnowledgeBase::new, libs);
  }

  public JProlContext(
      final String name,
      final File currentFolder,
      final Function<String, KnowledgeBase> knowledgeBaseSupplier,
      final AbstractJProlLibrary... libs
  ) {
    this(
        name,
        currentFolder,
        knowledgeBaseSupplier,
        ForkJoinPool.commonPool(),
        libs
    );
  }

  public JProlContext(
      final String name,
      final File currentFolder,
      final Function<String, KnowledgeBase> knowledgeBaseSupplier,
      final ExecutorService asyncTaskExecutorService,
      final AbstractJProlLibrary... libs
  ) {
    this(
        null,
        name,
        currentFolder,
        knowledgeBaseSupplier.apply(name + "_kbase"),
        asyncTaskExecutorService,
        emptyMap(),
        emptyList(),
        emptyList(),
        emptySet(),
        emptyMap(),
        libs
    );
  }

  public JProlContext(final String name, final AbstractJProlLibrary... libs) {
    this(name, ConcurrentInMemoryKnowledgeBase::new, libs);
  }

  public JProlContext(final String name,
                      final Function<String, KnowledgeBase> knowledgeBaseSupplier,
                      final AbstractJProlLibrary... libs) {
    this(name, new File(System.getProperty("user.home")), knowledgeBaseSupplier, libs);
  }

  private JProlContext(
      final JProlContext parentContext,
      final String contextId,
      final File currentFolder,
      final KnowledgeBase base,
      final ExecutorService asyncTaskExecutorService,
      final Map<JProlSystemFlag, Term> systemFlags,
      final List<JProlContextListener> contextListeners,
      final List<IoResourceProvider> ioProviders,
      final Set<String> dynamicSignatures,
      final Map<String, Map<JProlTriggerType, List<JProlTrigger>>> triggers,
      final AbstractJProlLibrary... additionalLibraries
  ) {
    this.parentContext = parentContext;
    this.contextId = requireNonNull(contextId, "Context Id is null");
    this.currentFolder = requireNonNull(currentFolder);
    this.knowledgeBase = requireNonNull(base, "Knowledge base is null");
    this.asyncTaskExecutorService = requireNonNull(asyncTaskExecutorService);
    this.contextListeners.addAll(contextListeners);

    if (!triggers.isEmpty()) {
      triggers.forEach((k, v) -> {
        final Map<JProlTriggerType, List<JProlTrigger>> triggerMap = new ConcurrentHashMap<>();
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

    this.libraries.add(new JProlBootstrapLibrary());

    asList(additionalLibraries).forEach(this::addLibrary);

    this.callInConstructorEnd();
  }

  public boolean isShareKnowledgeBaseBetweenThreads() {
    return this.shareKnowledgeBaseBetweenThreads;
  }

  public JProlContext getParentContext() {
    return this.parentContext;
  }

  public File getCurrentFolder() {
    return this.currentFolder;
  }

  private void callInConstructorEnd() {
    this.libraries.forEach(x -> x.onRegisteredInContext(this));
  }

  public boolean isTemplateValidate() {
    return this.templateValidate;
  }

  public UndefinedPredicateBehavior getUndefinedPredicateBehavior() {
    return this.undefinedPredicateBehaviour;
  }

  public boolean isDebug() {
    return this.debug;
  }

  public Term getSystemFlag(final JProlSystemFlag flag) {
    return this.systemFlags.getOrDefault(flag, flag.getDefaultValue());
  }

  public void setSystemFlag(final JProlSystemFlag flag, final Term term) {
    if (flag.isReadOnly()) {
      throw new IllegalStateException("Flag is marked as read-only: " + flag);
    } else {
      final Term value = requireNonNull(term.findNonVarOrDefault(null));
      this.systemFlags.put(flag, value);
      this.onSystemFlagsUpdated();
    }
  }

  private void onSystemFlagsUpdated() {
    this.shareKnowledgeBaseBetweenThreads =
        Boolean.parseBoolean(this.systemFlags.get(JProlSystemFlag.SHARE_KNOWLEDGE_BASE).getText());
    this.templateValidate =
        Boolean.parseBoolean(this.systemFlags.get(JProlSystemFlag.VERIFY).getText());
    this.debug = Boolean.parseBoolean(this.systemFlags.get(JProlSystemFlag.DEBUG).getText());
    this.undefinedPredicateBehaviour = UndefinedPredicateBehavior
        .find(this.systemFlags.get(JProlSystemFlag.UNKNOWN).getText())
        .orElseThrow(() -> new ProlDomainErrorException(
            Arrays.toString(UndefinedPredicateBehavior.values()),
            this.systemFlags.get(JProlSystemFlag.UNKNOWN))
        );
  }

  public JProlContext addContextListener(final JProlContextListener listener) {
    this.contextListeners.add(listener);
    return this;
  }

  public JProlContext removeContextListener(final JProlContextListener listener) {
    this.contextListeners.remove(listener);
    return this;
  }

  public JProlContext addIoResourceProvider(final IoResourceProvider provider) {
    this.ioProviders.add(provider);
    return this;
  }

  public JProlContext removeIoResourceProvider(final IoResourceProvider provider) {
    this.ioProviders.remove(provider);
    return this;
  }

  public Optional<Reader> findResourceReader(final String readerId) {
    return this.ioProviders.stream().map(x -> x.findReader(this, readerId)).filter(Objects::nonNull)
        .findFirst();
  }

  public Optional<Writer> findResourceWriter(final String writerId, final boolean append) {
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
    return this.asyncTaskCounter.get();
  }

  public void waitAllAsyncTasks() {
    this.asyncLocker.lock();
    try {
      while (this.asyncTaskCounter.get() > 0 && !this.isDisposed()) {
        try {
          this.asyncCounterCondition.await();
        } catch (InterruptedException ex) {
          Thread.currentThread().interrupt();
          break;
        }
      }
    } finally {
      this.asyncLocker.unlock();
    }
  }

  private void onAsyncTaskCompleted() {
    this.asyncTaskCounter.decrementAndGet();
    this.asyncLocker.lock();
    try {
      this.asyncCounterCondition.signalAll();
    } finally {
      this.asyncLocker.unlock();
    }
  }

  private void assertConcurrentKnowledgeBase() {
    if (!this.knowledgeBase.isConcurrent()) {
      throw new ProlKnowledgeBaseException("Async operations requires concurrent knowledge base");
    }
  }

  @SuppressWarnings("StatementWithEmptyBody")
  public CompletableFuture<Void> proveAllAsync(final Term goal, final boolean shareKnowledgeBase) {
    this.assertNotDisposed();
    this.assertConcurrentKnowledgeBase();

    this.asyncTaskCounter.incrementAndGet();
    return CompletableFuture.runAsync(() -> {
          final JProlContext contextCopy = this.makeCopy(shareKnowledgeBase);
          final JProlChoicePoint asyncGoal =
              new JProlChoicePoint(requireNonNull(goal), contextCopy);
          while (asyncGoal.prove() != null && !this.isDisposed() && !contextCopy.isDisposed()) {
            // do nothing
          }
          if (!contextCopy.isDisposed()) {
            contextCopy.dispose();
          }
        }, this.asyncTaskExecutorService)
        .handle((x, e) -> {
          this.onAsyncTaskCompleted();
          if (e != null) {
            if (!(e instanceof CompletionException) ||
                !(e.getCause() instanceof ProlInterruptException)) {
              throw new ProlForkExecutionException("Error during async/1", goal,
                  new Throwable[] {e});
            }
          }
          return x;
        });
  }

  public CompletableFuture<Term> proveOnceAsync(final Term goal,
                                                final boolean shareKnowledgeBase) {
    this.assertNotDisposed();
    this.assertConcurrentKnowledgeBase();

    this.asyncTaskCounter.incrementAndGet();
    return CompletableFuture.supplyAsync(() -> {
      final JProlChoicePoint asyncGoal =
          new JProlChoicePoint(requireNonNull(goal), this.makeCopy(shareKnowledgeBase));
      final Term result = asyncGoal.prove();
      asyncGoal.cutVariants();
      return result;
    }, this.asyncTaskExecutorService).handle((x, e) -> {
      this.onAsyncTaskCompleted();
      if (e != null) {
        if (!(e instanceof CompletionException) ||
            !(e.getCause() instanceof ProlInterruptException)) {
          throw new ProlForkExecutionException("Error during async/1", goal, new Throwable[] {e});
        }
      }
      return x;
    });
  }

  public ExecutorService getContextExecutorService() {
    return this.asyncTaskExecutorService;
  }

  private Optional<ReentrantLock> findLockerForId(final String lockerId,
                                                  final boolean createIfAbsent) {
    if (createIfAbsent) {
      return Optional.of(this.namedLockers.computeIfAbsent(lockerId, s -> new ReentrantLock()));
    } else {
      return Optional.ofNullable(this.namedLockers.get(lockerId));
    }
  }

  public void lockFor(final String lockId) {
    try {
      this.findLockerForId(lockId, true)
          .orElseThrow(
              () -> new IllegalArgumentException("Named locker is not presented: " + lockId))
          .lockInterruptibly();
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
      throw new RuntimeException("Locker wait has been interrupted: " + lockId, ex);
    }
  }

  public boolean tryLockFor(final String lockId) {
    return this.findLockerForId(lockId, true).orElseThrow(
        () -> new IllegalArgumentException("Named locker is not presented: " + lockId)).tryLock();
  }

  public void unlockFor(final String lockId) {
    this.findLockerForId(lockId, false).ifPresent(ReentrantLock::unlock);
  }

  private void assertNotDisposed() {
    if (this.isDisposed()) {
      throw new ProlException("Context is disposed: " + this.contextId);
    }
  }

  public boolean addLibrary(final AbstractJProlLibrary library) {
    assertNotDisposed();
    if (library == null) {
      throw new IllegalArgumentException("Library must not be null");
    }
    if (this.libraries.contains(library)) {
      return false;
    }

    this.libraries.add(0, library);

    final JProlConsultText consultText = library.getClass().getAnnotation(JProlConsultText.class);
    if (consultText != null) {
      final String text = String.join("\n", consultText.value());
      if (!text.isEmpty()) {
        this.consult(new StringReader(text), null);
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
      this.consult(new StringReader(resourceText), null);
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
      this.consult(new StringReader(resourceText), null);
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
      this.consult(new StringReader(resourceText), null);
    }

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
    final boolean result = libraries.remove(library);
    if (result) {
      library.onLibraryRemove(this);
    }
    return result;
  }

  public PredicateInvoker findProcessor(final TermStruct predicate) {
    return this.libraries
        .stream()
        .map(lib -> lib.findProcessorForPredicate(predicate))
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(NULL_PROCESSOR);
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
    return this.libraries.iterator();
  }

  public boolean hasSystemOperatorStartsWith(final String str) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.isSystemOperatorStartsWith(str));
  }

  public void dispose() {
    if (this.disposed.compareAndSet(false, true)) {
      this.asyncTaskExecutorService.shutdownNow();
      try {
        this.asyncTaskExecutorService.awaitTermination(30, TimeUnit.SECONDS);
      } catch (InterruptedException ex) {
        Thread.currentThread().interrupt();
      }

      this.triggers.values().stream()
          .flatMap(x -> x.values().stream())
          .flatMap(Collection::stream)
          .distinct()
          .forEach(x -> x.onContextDispose(this));

      this.libraries.forEach(library -> {
        try {
          library.onContextDispose(this);
        } finally {
          if (this.parentContext == null) {
            library.release();
          }
        }
      });
      this.contextListeners.forEach(listener -> listener.onContextDispose(this));
      this.contextListeners.clear();
    }
  }

  public boolean isDisposed() {
    final boolean parentDisposed = this.parentContext != null && this.parentContext.isDisposed();
    final boolean thisDisposed = this.disposed.get();

    if (parentDisposed && !thisDisposed) {
      this.dispose();
    }

    return thisDisposed;
  }

  public void addTrigger(final JProlTrigger trigger) {
    assertNotDisposed();
    trigger.getSignatures().forEach((signature, types) -> {
      String validatedSignature = ProlUtils.reassembleSignatureOrNull(signature);
      if (validatedSignature == null) {
        throw new IllegalArgumentException("Illegal signature format: " + signature);
      }
      validatedSignature = ProlUtils.normalizeSignature(validatedSignature);
      final Map<JProlTriggerType, List<JProlTrigger>> map =
          this.triggers.computeIfAbsent(validatedSignature, key -> new ConcurrentHashMap<>());
      types.forEach(x -> map.computeIfAbsent(x, key -> new CopyOnWriteArrayList<>()).add(trigger));
    });
  }

  public void removeTrigger(final JProlTrigger trigger) {
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
    final Map<JProlTriggerType, List<JProlTrigger>> map = this.triggers.get(signature);
    return map != null && map.containsKey(triggerType);
  }

  public void notifyAboutUndefinedPredicate(final JProlChoicePoint choicePoint,
                                            final String signature, final Term term) {
    this.assertNotDisposed();
    switch (this.getUndefinedPredicateBehavior()) {
      case ERROR: {
        throw new ProlExistenceErrorException("predicate",
            "Undefined predicate: " + term.getSignature(),
            term);
      }
      case WARNING: {
        this.contextListeners
            .forEach(x -> x.onUndefinedPredicateWarning(this, choicePoint, term.getSignature()));
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
                                         final JProlTriggerType triggerType) {
    this.assertNotDisposed();

    final Map<JProlTriggerType, List<JProlTrigger>> triggerMap = this.triggers.get(signature);
    if (triggerMap != null && triggerMap.containsKey(triggerType)) {
      final List<JProlTrigger> triggers = triggerMap.get(triggerType);
      if (triggers != null && !triggers.isEmpty()) {
        final TriggerEvent triggerEvent = new TriggerEvent(this, signature, triggerType);
        triggers.forEach(x -> x.onTriggerEvent(triggerEvent));
      }
    }
  }

  public void consult(final Reader source) {
    this.consult(source, null);
  }

  public Set<String> findDynamicSignatures() {
    this.assertNotDisposed();
    return new HashSet<>(this.dynamicSignatures);
  }

  public void addDynamicSignatures(final Set<String> signatures,
                                   final SourcePosition sourcePosition) {
    this.assertNotDisposed();

    for (final String signature : signatures) {
      if (signature == null) {
        throw new NullPointerException();
      }
      if (this.hasPredicateAtLibraryForSignature(signature)) {
        throw new ProlPermissionErrorException("modify", "static_procedure",
            "Signature '" + signature + "'is statically presented in a registered library",
            newAtom(signature, Objects.requireNonNullElse(sourcePosition, UNKNOWN)));
      }
    }

    this.dynamicSignatures.addAll(signatures);
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
                                          final BiFunction<String, TermStruct, Boolean> processingFunction) {
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
          termStruct.getElement(0).getSignature() : termStruct.getSignature();
    }

    if (this.dynamicSignatures.contains(signature)) {
      operationResult = processingFunction.apply(signature, termStruct);
    } else {
      throw new ProlPermissionErrorException("modify", "static_procedure",
          "Allowed only for dynamic predicates " + signature,
          newAtom(signature, term.getSourcePosition()));
    }
    return operationResult;
  }

  public boolean assertA(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> knowledgeBase.assertA(this, struct));
  }

  public boolean assertZ(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> this.knowledgeBase.assertZ(this, struct));
  }

  public boolean retractAll(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> this.knowledgeBase.retractAll(this, struct));
  }

  public boolean abolish(final Term term) {
    final Term indicator = term.findNonVarOrSame();
    ProlAssertions.assertIndicator(indicator);
    return this.doClauseInKnowledgeBase(term,
        true,
        (signature, struct) -> this.knowledgeBase.abolish(this, signature));
  }

  public boolean retractA(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> this.knowledgeBase.retractA(this, struct));
  }

  public boolean retractZ(final Term term) {
    return this.doClauseInKnowledgeBase(term,
        false,
        (signature, struct) -> this.knowledgeBase.retractZ(this, struct));
  }

  public void consult(final Reader source, final ConsultInteract iterator) {
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
                      this.knowledgeBase.assertZ(this, struct);
                    }
                    break;
                    case FX: {
                      // directive
                      if (!this.processDirective(struct.getElement(0))) {
                        throw new ProlHaltExecutionException(2);
                      }
                    }
                    break;
                  }

                } else if ("?-".equals(text)) {
                  final Term termGoal = struct.getElement(0);

                  if (iterator != null && iterator.onFoundInteractiveGoal(this, termGoal)) {

                    final Map<String, TermVar> variableMap = new LazyMap<>();
                    final AtomicInteger solutionCounter = new AtomicInteger();

                    final JProlChoicePoint thisGoal = new JProlChoicePoint(termGoal, this, null);

                    boolean doFindNextSolution;
                    do {
                      variableMap.clear();
                      if (solveGoal(thisGoal, variableMap)) {
                        doFindNextSolution = iterator
                            .onSolution(this, termGoal, variableMap,
                                solutionCounter.incrementAndGet());
                        if (!doFindNextSolution) {
                          throw new ProlHaltExecutionException("search halted or stopped", 1);
                        }
                      } else {
                        iterator.onFail(this, termGoal, solutionCounter.get());
                        doFindNextSolution = false;
                      }
                    } while (doFindNextSolution);
                  }
                } else {
                  this.knowledgeBase.assertZ(this, struct);
                }
              } else {
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
              errorPosition.getLine(), errorPosition.getPosition(), ex);
        } catch (Exception ex) {
          throw new PrologParserException(
              ex.getCause() == null ? ex.getMessage() : ex.getCause().getMessage(),
              sourcePosition.getLine(), sourcePosition.getPosition(), ex);
        }
      } while (!this.isDisposed());
    }
  }

  private boolean solveGoal(final JProlChoicePoint goal, final Map<String, TermVar> varTable) {
    final Term result = goal.prove();

    if (result != null && varTable != null) {
      result.variables().forEach(e -> varTable.put(e.getText(), e));
    }

    return result != null;
  }

  private boolean processDirective(final Term directive) {
    final JProlChoicePoint goal = new JProlChoicePoint(directive, this, null);
    return goal.prove() != null;
  }


  @Override
  public String toString() {
    return "ProlContext(" + contextId + ')' + '[' + super.toString() + ']';
  }

  /**
   * Check that a critical predicate allowed, a predicate like consult/1 or abolish/1 so mainly working with IO, threads and changing knowledge base.
   *
   * @param sourceLibrary      source library class where the predicate is defined
   * @param choicePoint        choice point where the predicate is called
   * @param predicateIndicator predicate indicator
   * @return true if allowed false otherwise
   * @since 2.2.0
   */
  public boolean isCriticalPredicateAllowed(
      final Class<? extends AbstractJProlLibrary> sourceLibrary,
      final JProlChoicePoint choicePoint,
      final String predicateIndicator
  ) {
    return true;
  }

  public JProlContext makeCopy(final boolean shareKnowledgeBase) {
    return new JProlContext(
        this,
        this.contextId + "_copy",
        this.currentFolder,
        shareKnowledgeBase ? this.knowledgeBase : this.knowledgeBase.makeCopy(),
        this.asyncTaskExecutorService,
        this.systemFlags,
        this.contextListeners,
        this.ioProviders,
        this.dynamicSignatures,
        this.triggers
    );
  }

  public ParserContext getParserContext() {
    return this.parserContext;
  }

  @Override
  public void close() {
    this.dispose();
  }
}
