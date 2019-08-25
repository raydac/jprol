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

import com.igormaznitsa.jprol.annotations.JProlConsultClasspath;
import com.igormaznitsa.jprol.annotations.JProlConsultFile;
import com.igormaznitsa.jprol.annotations.JProlConsultText;
import com.igormaznitsa.jprol.data.*;
import com.igormaznitsa.jprol.exceptions.*;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.kbase.KnowledgeContext;
import com.igormaznitsa.jprol.kbase.KnowledgeContextFactory;
import com.igormaznitsa.jprol.kbase.inmemory.InMemoryKnowledgeBase;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.libs.JProlBootstrapLibrary;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import com.igormaznitsa.jprol.logic.triggers.JProlTrigger;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggerType;
import com.igormaznitsa.jprol.logic.triggers.TriggerEvent;
import com.igormaznitsa.jprol.trace.JProlContextListener;
import com.igormaznitsa.jprol.trace.TraceEvent;
import com.igormaznitsa.jprol.utils.Utils;
import com.igormaznitsa.prologparser.ParserContext;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.logic.PredicateInvoker.NULL_PROCESSOR;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.concat;

public final class JProlContext {
  private final String contextId;

  private final Map<String, List<JProlTrigger>> triggersOnAssert = new ConcurrentHashMap<>();
  private final Map<String, List<JProlTrigger>> triggersOnRetract = new ConcurrentHashMap<>();
  private final Map<String, ReentrantLock> namedLockerObjects = new ConcurrentHashMap<>();
  private final List<AbstractJProlLibrary> libraries = new CopyOnWriteArrayList<>();
  private final AtomicBoolean disposed = new AtomicBoolean(false);
  private final KnowledgeBase knowledgeBase;
  private final ExecutorService executorService;
  private final List<JProlContextListener> contextListeners = new CopyOnWriteArrayList<>();
  private final Map<JProlSystemFlag, Term> systemFlags = new ConcurrentHashMap<>();
  private final AtomicInteger currentAsyncTaskNumber = new AtomicInteger();
  private final KnowledgeContextFactory knowledgeContextFactory;
  private final ThreadLocal<KnowledgeContext> knowledgeContext;

  private final ParserContext parserContext = new ParserContext() {
    @Override
    public boolean hasOpStartsWith(final PrologParser prologParser, final String s) {
      return JProlContext.this.knowledgeBase.hasOperatorStartsWith(JProlContext.this, s);
    }

    @Override
    public OpContainer findOpForName(final PrologParser prologParser, final String s) {
      final TermOperatorContainer container = JProlContext.this.knowledgeBase.findOperatorForName(JProlContext.this, s);
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
  private boolean templateValidate;
  private boolean debug;
  private UndefinedPredicateBehavior undefinedPredicateBehaviour;

  public JProlContext(final KnowledgeContextFactory knowledgeContextFactory, final String name, final AbstractJProlLibrary... libs) {
    this(
        knowledgeContextFactory,
        name,
        new InMemoryKnowledgeBase(name + "_kbase"),
        ForkJoinPool.commonPool(),
        emptyMap(),
        emptyList(),
        emptyList(),
        libs
    );
  }

  private JProlContext(
      final KnowledgeContextFactory knowledgeContextFactory,
      final String contextId,
      final KnowledgeBase base,
      final ExecutorService executorService,
      final Map<JProlSystemFlag, Term> systemFlags,
      final List<JProlContextListener> contextListeners,
      final List<IoResourceProvider> ioProviders,
      final AbstractJProlLibrary... additionalLibraries
  ) {
    this.knowledgeContextFactory = Objects.requireNonNull(knowledgeContextFactory);
    this.knowledgeContext = ThreadLocal.withInitial(this.knowledgeContextFactory::getDefaultKnowledgeContext);
    this.contextId = requireNonNull(contextId, "Context Id is null");
    this.knowledgeBase = requireNonNull(base, "Knowledge base is null");
    this.executorService = requireNonNull(executorService);
    this.contextListeners.addAll(contextListeners);

    Arrays.stream(JProlSystemFlag.values())
        .filter(x -> !x.isReadOnly())
        .forEach(x -> this.systemFlags.put(x, x.getDefaultValue()));

    this.systemFlags.putAll(systemFlags);
    this.onSystemFlagsUpdated();

    this.ioProviders.addAll(ioProviders);

    this.libraries.add(new JProlBootstrapLibrary());
    this.libraries.addAll(asList(additionalLibraries));
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

  public KnowledgeContextFactory getKnowledgeContextFactory() {
    return this.knowledgeContextFactory;
  }

  public KnowledgeContext getKnowledgeContext() {
    return this.knowledgeContext.get();
  }

  public void setKnowledgeContext(final KnowledgeContext context) {
    this.knowledgeContext.set(Objects.requireNonNull(context));
  }

  public void setSystemFlag(final JProlSystemFlag flag, final Term term) {
    if (flag.isReadOnly()) {
      throw new IllegalStateException("Flag is marked as read-only: " + flag);
    } else {
      final Term value = Objects.requireNonNull(term.findNonVarOrDefault(null));
      this.systemFlags.put(flag, value);
      this.onSystemFlagsUpdated();
    }
  }

  private void onSystemFlagsUpdated() {
    this.templateValidate = Boolean.parseBoolean(this.systemFlags.get(JProlSystemFlag.VERIFY).getText());
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
    return this.ioProviders.stream().map(x -> x.findReader(this, readerId)).filter(Objects::nonNull).findFirst();
  }

  public Optional<Writer> findResourceWriter(final String writerId, final boolean append) {
    return this.ioProviders.stream().map(x -> x.findWriter(this, writerId, append)).filter(Objects::nonNull).findFirst();
  }

  public final String getName() {
    return this.contextId;
  }

  void fireTraceEvent(final TraceEvent event, final ChoicePoint choicePoint) {
    if (!this.contextListeners.isEmpty()) {
      this.contextListeners.forEach(l -> l.onChoicePointTraceEvent(this, choicePoint, event));
    }
  }

  public int getCurrentAsyncTaskNumber() {
    return this.currentAsyncTaskNumber.get();
  }

  public void waitForAllAsyncDone() {
    while (this.currentAsyncTaskNumber.get() > 0) {
      synchronized (this.currentAsyncTaskNumber) {
        try {
          this.currentAsyncTaskNumber.wait();
        } catch (InterruptedException ex) {
          Thread.currentThread().interrupt();
          return;
        }
      }
    }
  }

  private void onAsyncGoalCompleted(final Term goal) {
    this.currentAsyncTaskNumber.decrementAndGet();
    synchronized (this.currentAsyncTaskNumber) {
      this.currentAsyncTaskNumber.notifyAll();
    }
  }

  public void submitAsync(final Term goal) {
    this.assertNotDisposed();

    this.currentAsyncTaskNumber.incrementAndGet();
    try {
      getContextExecutorService().submit(() -> {
        try {
          final ChoicePoint asyncGoal = new ChoicePoint(requireNonNull(goal), JProlContext.this);

          while (!Thread.currentThread().isInterrupted()) {
            final Term result = asyncGoal.next();
            if (result == null) {
              break;
            }
          }
        } finally {
          JProlContext.this.onAsyncGoalCompleted(goal);
        }
      });
    } catch (RejectedExecutionException ex) {
      this.currentAsyncTaskNumber.decrementAndGet();
      synchronized (this.currentAsyncTaskNumber) {
        this.currentAsyncTaskNumber.notifyAll();
      }
      throw new ProlForkExecutionException("Can't submit term for async resolving", goal, new Throwable[] {ex});
    }
  }

  public ExecutorService getContextExecutorService() {
    return this.executorService;
  }

  private Optional<ReentrantLock> findLockerForId(final String lockerId, final boolean createIfAbsent) {
    if (createIfAbsent) {
      return Optional.of(this.namedLockerObjects.computeIfAbsent(lockerId, s -> new ReentrantLock()));
    } else {
      return Optional.ofNullable(this.namedLockerObjects.get(lockerId));
    }
  }

  public void lockLockerForName(final String lockerId) {
    try {
      this.findLockerForId(lockerId, true).orElseThrow(() -> new Error("Locker must be presented: " + lockerId)).lockInterruptibly();
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
      throw new RuntimeException("Locker wait has been interrupted: " + lockerId, ex);
    }
  }

  public boolean trylockLockerForName(final String lockerId) {
    return this.findLockerForId(lockerId, true).orElseThrow(() -> new Error("Locker must be presented: " + lockerId)).tryLock();
  }

  public void unlockLockerForName(final String lockerId) {
    this.findLockerForId(lockerId, false).ifPresent(ReentrantLock::unlock);
  }

  private void assertNotDisposed() {
    if (this.disposed.get()) {
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
      final String text = Arrays.stream(consultText.value()).collect(Collectors.joining("\n"));
      if (text.length() > 0) {
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
              return Utils.readAsUtf8(x);
            } catch (IOException ex) {
              throw new Error("Can't read file: " + x);
            }
          })
          .collect(Collectors.joining("\n"));
      this.consult(new StringReader(resourceText), null);
    }

    final JProlConsultClasspath consultClasspath = library.getClass().getAnnotation(JProlConsultClasspath.class);
    if (consultClasspath != null) {
      final String resourceText = Arrays.stream(consultClasspath.value())
          .filter(x -> !(x == null || x.trim().isEmpty()))
          .map(x -> {
            final InputStream inStream = ClassLoader.getSystemClassLoader().getResourceAsStream(x);
            if (inStream == null) {
              throw new Error("Can't find resource: " + x);
            }
            return inStream;
          })
          .map(x -> {
            final StringBuilder buffer = new StringBuilder();
            try (final Reader reader = new InputStreamReader(new BufferedInputStream(x), StandardCharsets.UTF_8)) {
              while (!Thread.currentThread().isInterrupted()) {
                final int value = reader.read();
                if (value < 0) {
                  break;
                }
                buffer.append((char) value);
              }
            } catch (IOException ex) {
              throw new Error("Can't read resource", ex);
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
    return libraries.remove(library);
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

  public boolean hasPredicateAtLibraryForSignature(final String signature) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.hasPredicateForSignature(signature));
  }

  public boolean isSystemOperator(final String name) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.isSystemOperator(name));
  }

  public TermOperatorContainer getSystemOperatorForName(final String name) {
    return this.libraries.stream()
        .map(lib -> lib.findSystemOperatorForName(name))
        .filter(Objects::nonNull)
        .findFirst().orElse(null);
  }

  public Iterator<AbstractJProlLibrary> makeLibraryIterator() {
    return this.libraries.iterator();
  }

  public boolean hasSystemOperatorStartsWith(final String str) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.hasSyatemOperatorStartsWith(str));
  }

  public void dispose() {
    if (this.disposed.compareAndSet(false, true)) {
      this.executorService.shutdownNow();

      concat(this.triggersOnAssert.entrySet().stream(), this.triggersOnRetract.entrySet().stream())
          .flatMap(x -> x.getValue().stream())
          .distinct()
          .forEach(x -> x.onContextHalting(this));

      this.triggersOnAssert.clear();
      this.triggersOnRetract.clear();

      this.libraries.forEach((library) -> library.onContextDispose(this));
    }
  }

  public boolean isDisposed() {
    return disposed.get();
  }

  public void registerTrigger(final JProlTrigger trigger) {
    assertNotDisposed();
    final Map<String, JProlTriggerType> signatures = trigger.getSignatures();

    signatures.forEach((key, triggerType) -> {
      String signature = Utils.validateSignature(key);
      if (signature == null) {
        throw new IllegalArgumentException("Unsupported signature: " + key);
      }
      signature = Utils.normalizeSignature(signature);

      if (triggerType == JProlTriggerType.TRIGGER_ASSERT || triggerType == JProlTriggerType.TRIGGER_ASSERT_RETRACT) {
        this.triggersOnAssert.computeIfAbsent(signature, k -> new CopyOnWriteArrayList<>()).add(trigger);
      }

      if (triggerType == JProlTriggerType.TRIGGER_RETRACT || triggerType == JProlTriggerType.TRIGGER_ASSERT_RETRACT) {
        this.triggersOnRetract.computeIfAbsent(signature, k -> new CopyOnWriteArrayList<>()).add(trigger);
      }
    });
  }

  public void unregisterTrigger(final JProlTrigger trigger) {
    Stream.of(this.triggersOnAssert.entrySet().iterator(), this.triggersOnRetract.entrySet().iterator()).forEach(iterator -> {
      while (iterator.hasNext()) {
        final Entry<String, List<JProlTrigger>> entry = iterator.next();
        final List<JProlTrigger> lst = entry.getValue();
        if (lst.remove(trigger)) {
          if (lst.isEmpty()) {
            iterator.remove();
          }
        }
      }
    });
  }

  public boolean hasRegisteredTriggersForSignature(final String normalizedSignature,
                                                   final JProlTriggerType observedEvent) {
    boolean result;
    switch (observedEvent) {
      case TRIGGER_ASSERT: {
        result = this.triggersOnAssert.containsKey(normalizedSignature);
      }
      break;
      case TRIGGER_RETRACT: {
        result = this.triggersOnRetract.containsKey(normalizedSignature);
      }
      break;
      case TRIGGER_ASSERT_RETRACT: {
        result = this.triggersOnAssert.containsKey(normalizedSignature);
        if (!result) {
          result = this.triggersOnRetract.containsKey(normalizedSignature);
        }
      }
      break;
      default: {
        throw new IllegalArgumentException("Unsupported observed event [" + observedEvent.name() + ']');
      }
    }

    return result;
  }

  public void notifyAboutUndefinedPredicate(final ChoicePoint choicePoint, final String signature) {
    switch (this.getUndefinedPredicateBehavior()) {
      case ERROR: {
        throw new ProlExistenceErrorException("predicate", "Undefined predicate: " + signature, choicePoint.getGoalTerm());
      }
      case WARNING: {
        this.contextListeners.forEach(x -> x.onUndefinedPredicateWarning(this, choicePoint, signature));
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

  public void notifyTriggersForSignature(final String normalizedSignature,
                                         final JProlTriggerType observedEvent) {
    final List<JProlTrigger> listOfTriggers;

    switch (observedEvent) {
      case TRIGGER_ASSERT: {
        listOfTriggers = this.triggersOnAssert.getOrDefault(normalizedSignature, emptyList());
      }
      break;
      case TRIGGER_RETRACT: {
        listOfTriggers = this.triggersOnRetract.getOrDefault(normalizedSignature, emptyList());
      }
      break;
      case TRIGGER_ASSERT_RETRACT: {
        final List<JProlTrigger> triggersAssert = this.triggersOnAssert.getOrDefault(normalizedSignature, emptyList());
        final List<JProlTrigger> triggersRetract = this.triggersOnRetract.getOrDefault(normalizedSignature, emptyList());
        listOfTriggers = triggersRetract.isEmpty() && triggersAssert.isEmpty() ? emptyList() : concat(triggersAssert.stream(), triggersRetract.stream()).collect(toList());
      }
      break;
      default: {
        throw new IllegalArgumentException("Unsupported trigger event [" + observedEvent.name());
      }
    }

    if (!listOfTriggers.isEmpty()) {
      final TriggerEvent event = new TriggerEvent(this, normalizedSignature, observedEvent);
      listOfTriggers.forEach(x -> x.onTriggerEvent(event));
    }
  }

  public void consult(final Reader source) {
    this.consult(source, null);
  }

  public void consult(final Reader source, final ConsultInteractor interactor) {
    final JProlTreeBuilder treeBuilder = new JProlTreeBuilder(this);
    do {
      final JProlTreeBuilder.Result parseResult = treeBuilder.readPhraseAndMakeTree(source);
      if (parseResult == null) {
        break;
      }

      final int line = parseResult.line;
      final int strpos = parseResult.pos;

      final Term nextItem = parseResult.term;

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
              final OpAssoc type = op.getOperatorType();

              if (struct.isClause()) {
                switch (type) {
                  case XFX: {
                    // new rule
                    this.knowledgeBase.assertZ(this, struct);
                  }
                  break;
                  case FX: {
                    // directive
                    if (!processDirective(struct.getElement(0))) {
                      throw new ProlHaltExecutionException(2);
                    }
                  }
                  break;
                }

              } else if ("?-".equals(text)) {
                final Term termGoal = struct.getElement(0);

                if (interactor != null && interactor.onFoundInteractiveGoal(this, termGoal)) {

                  final Map<String, TermVar> varmap = new HashMap<>();
                  final AtomicInteger solutioncounter = new AtomicInteger();

                  final ChoicePoint thisGoal = new ChoicePoint(termGoal, this, null);

                  boolean doFindNextSolution;
                  do {
                    varmap.clear();
                    if (solveGoal(thisGoal, varmap)) {
                      doFindNextSolution = interactor.onSolution(this, termGoal, varmap, solutioncounter.incrementAndGet());
                      if (!doFindNextSolution) {
                        throw new ProlHaltExecutionException(String.format("Solution search halted: %s", termGoal), 1);
                      }
                    } else {
                      interactor.onFail(this, termGoal, solutioncounter.get());
                      doFindNextSolution = false;
                    }
                  } while (doFindNextSolution && !Thread.currentThread().isInterrupted());
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
            throw new ProlKnowledgeBaseException("Such element can't be saved at knowledge base [" + nextItem + ']');
          }
        }
      } catch (Exception ex) {
        throw new PrologParserException(ex.getCause() == null ? ex.getMessage() : ex.getCause().getMessage(), line, strpos, ex);
      }
    } while (!Thread.currentThread().isInterrupted());
  }

  private boolean solveGoal(final ChoicePoint goal, final Map<String, TermVar> varTable) {
    final Term result = goal.next();

    if (result != null && varTable != null) {
      result.variables().forEach(e -> varTable.put(e.getText(), e));
    }

    return result != null;
  }

  private boolean processDirective(final Term directive) {
    final ChoicePoint goal = new ChoicePoint(directive, this, null);
    return goal.next() != null;
  }


  @Override
  public String toString() {
    return "ProlContext(" + contextId + ')' + '[' + super.toString() + ']';
  }

  public JProlContext makeCopy() {
    return new JProlContext(
        this.knowledgeContextFactory,
        this.contextId + "_copy",
        this.knowledgeBase.makeCopy(),
        this.executorService,
        this.systemFlags,
        this.contextListeners,
        this.ioProviders
    );
  }

  public ParserContext getParserContext() {
    return this.parserContext;
  }

}
