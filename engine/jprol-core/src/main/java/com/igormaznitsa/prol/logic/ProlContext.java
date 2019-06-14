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

package com.igormaznitsa.prol.logic;

import com.igormaznitsa.prol.annotations.ConsultText;
import com.igormaznitsa.prol.containers.InMemoryKnowledgeBase;
import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.*;
import com.igormaznitsa.prol.io.*;
import com.igormaznitsa.prol.libraries.AbstractProlLibrary;
import com.igormaznitsa.prol.libraries.PredicateProcessor;
import com.igormaznitsa.prol.libraries.ProlCoreLibrary;
import com.igormaznitsa.prol.logic.triggers.ProlTrigger;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.logic.triggers.TriggerEvent;
import com.igormaznitsa.prol.parser.ProlReader;
import com.igormaznitsa.prol.parser.ProlTokenizer;
import com.igormaznitsa.prol.parser.ProlTreeBuilder;
import com.igormaznitsa.prol.trace.TraceEvent;
import com.igormaznitsa.prol.trace.TracingChoicePointListener;
import com.igormaznitsa.prol.utils.Utils;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.igormaznitsa.prol.data.Terms.newStruct;
import static com.igormaznitsa.prol.libraries.PredicateProcessor.NULL_PROCESSOR;
import static java.util.stream.Stream.concat;

public final class ProlContext {
  public static final String ENGINE_VERSION = "2.0.0";
  public static final String ENGINE_NAME = "Prol";

  public static final String STREAM_USER = "user";

  private final String contextId;

  private final Map<String, ProlTextInputStream> inputStreams = new ConcurrentHashMap<>();
  private final Map<String, ProlTextOutputStream> outputStreams = new ConcurrentHashMap<>();
  private final Map<String, ProlMemoryPipe> memPipes = new ConcurrentHashMap<>();
  private final Map<String, List<ProlTrigger>> triggersOnAssert = new ConcurrentHashMap<>();
  private final Map<String, List<ProlTrigger>> triggersOnRetract = new ConcurrentHashMap<>();
  private final Map<String, ReentrantLock> namedLockerObjects = new ConcurrentHashMap<>();
  private final List<AbstractProlLibrary> libraries = new CopyOnWriteArrayList<>();
  private final AtomicBoolean disposed = new AtomicBoolean(false);
  private final KnowledgeBase knowledgeBase;
  private final ExecutorService executorService;
  private final ProlStreamManager streamManager;
  private final AtomicInteger activeTaskCounter = new AtomicInteger();
  private final List<TracingChoicePointListener> traceListeners = new CopyOnWriteArrayList<>();
  private volatile Optional<ProlTextReader> inReader = Optional.empty();
  private volatile Optional<ProlTextWriter> outWriter = Optional.empty();

  public ProlContext(final String name) {
    this(name, DefaultProlStreamManagerImpl.getInstance());
  }

  private volatile Optional<ProlTextWriter> errWriter = Optional.empty();

  public ProlContext(final String name, final ProlStreamManager streamManager) {
    this(name, streamManager, new InMemoryKnowledgeBase(name + "_kbase"), null);
  }

  public ProlContext(final String contextId, final ProlStreamManager streamManager, final KnowledgeBase base, final ExecutorService executorService) {
    Utils.assertNotNull(contextId, "Contex Id must not be null");
    Utils.assertNotNull(streamManager, "Stream manager must be provided");
    Utils.assertNotNull(base, "Knowledge base must not be null");

    this.contextId = contextId;
    this.streamManager = streamManager;
    this.knowledgeBase = base;

    this.executorService = executorService == null ? ForkJoinPool.commonPool() : executorService;

    this.libraries.add(new ProlCoreLibrary());
    see(STREAM_USER);
    tell(STREAM_USER, true);
  }

  public void addTraceListener(final TracingChoicePointListener listener) {
    this.traceListeners.add(listener);
  }

  public void removeTraceListener(final TracingChoicePointListener listener) {
    this.traceListeners.remove(listener);
  }

  public final String getName() {
    return this.contextId;
  }

  void fireTraceEvent(final TraceEvent event, final ChoicePoint choicePoint) {
    if (!this.traceListeners.isEmpty()) {
      this.traceListeners.forEach(l -> l.onTraceChoicePointEvent(event, choicePoint));
    }
  }

  public int getActiveAsyncTaskNumber() {
    return this.activeTaskCounter.get();
  }

  public void waitAllAsyncDone() {
    while (this.activeTaskCounter.get() > 0) {
      synchronized (this) {
        if (this.activeTaskCounter.get() > 0) {
          try {
            this.wait();
          } catch (InterruptedException ex) {
            Thread.currentThread().interrupt();
          }
        }
      }
    }
  }

  public Future<?> submitAsync(final Term goal) {
    this.assertNotDisposed();

    if (goal == null) {
      throw new NullPointerException("The term to prove is null");
    }

    this.activeTaskCounter.incrementAndGet();
    try {
      return getContextExecutorService().submit(() -> {
        try {
          final ChoicePoint asyncGoal = new ChoicePoint(goal, ProlContext.this);

          while (!Thread.currentThread().isInterrupted()) {
            final Term result = asyncGoal.next();
            if (result == null) {
              break;
            }
          }
        } finally {
          synchronized (ProlContext.this) {
            this.activeTaskCounter.decrementAndGet();
            ProlContext.this.notifyAll();
          }
        }
      });
    } catch (Exception ex) {
      synchronized (ProlContext.this) {
        this.activeTaskCounter.decrementAndGet();
        ProlContext.this.notifyAll();
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
      this.findLockerForId(lockerId, true).get().lockInterruptibly();
    } catch (InterruptedException ex) {
      Thread.currentThread().interrupt();
      throw new RuntimeException("Locker wait has been interrupted: " + lockerId, ex);
    }
  }

  public boolean trylockLockerForName(final String lockerId) {
    return this.findLockerForId(lockerId, true).get().tryLock();
  }

  public void unlockLockerForName(final String lockerId) {
    this.findLockerForId(lockerId, false).ifPresent(ReentrantLock::unlock);
  }

  private void assertNotDisposed() {
    if (this.disposed.get()) {
      throw new ProlException("Context is disposed: " + this.contextId);
    }
  }

  public ProlMemoryPipe findMemPipe(final String pipeId) {
    return this.memPipes.get(pipeId);
  }

  public ProlStreamManager getStreamManager() {
    assertNotDisposed();
    return this.streamManager;
  }

  public Optional<ProlTextWriter> getOutWriter() {
    assertNotDisposed();
    return this.outWriter;
  }

  public Optional<ProlTextWriter> getErrWriter() {
    assertNotDisposed();
    return this.errWriter;
  }

  public Optional<ProlTextReader> getInReader() {
    assertNotDisposed();
    return inReader;
  }

  public void tell(final String resourceId, final boolean append) {
    assertNotDisposed();
    try {
      if (resourceId.length() > 0 && resourceId.charAt(0) == '+') {
        // it's pipe
        ProlMemoryPipe out = memPipes.get(resourceId);
        if (out == null) {
          out = new ProlMemoryPipe(resourceId, this);
          memPipes.put(resourceId, out);
        }
        outWriter = Optional.of(out);
      } else {
        ProlTextOutputStream out = outputStreams.get(resourceId);
        if (out == null) {
          out = new ProlTextOutputStream(resourceId, this, append);
          outputStreams.put(resourceId, out);
        }
        outWriter = Optional.of(out);
      }
    } catch (IOException ex) {
      throw new RuntimeException(ex);
    }
  }

  public void see(final String resourceId) {
    assertNotDisposed();
    try {
      if (resourceId.length() > 0 && resourceId.charAt(0) == '+') {
        ProlMemoryPipe in = memPipes.get(resourceId);
        if (in == null) {
          in = new ProlMemoryPipe(resourceId, this);
          memPipes.put(resourceId, in);
        }
        inReader = Optional.of(in);
      } else {
        ProlTextInputStream in = inputStreams.get(resourceId);
        if (in == null) {
          in = new ProlTextInputStream(resourceId, this);
          inputStreams.put(resourceId, in);
        }
        inReader = Optional.of(in);
      }
    } catch (IOException ex) {
      throw new RuntimeException(ex);
    }
  }

  public void seen() {
    assertNotDisposed();
    this.inReader.ifPresent(reader -> {
      try {
        if (!reader.getResourceId().equals(STREAM_USER)) {
          reader.close();

          if (reader instanceof ProlMemoryPipe) {
            memPipes.remove(reader.getResourceId());
          } else {
            inputStreams.remove(reader.getResourceId());
          }
        }
      } catch (IOException ex) {
        throw new RuntimeException(ex);
      } finally {
        see(STREAM_USER);
      }
    });
  }

  public void told() {
    assertNotDisposed();
    this.outWriter.ifPresent(writer -> {
      try {
        if (!writer.getResourceId().equals(STREAM_USER)) {
          if (writer instanceof ProlMemoryPipe) {
            //memPipes.remove(outWriter.getResourceId());
            ((ProlMemoryPipe) writer).closeForWriteOnly();
          } else {
            writer.close();
            outputStreams.remove(writer.getResourceId());
          }
        }
      } catch (IOException ex) {
        throw new RuntimeException(ex);
      } finally {
        tell(STREAM_USER, true);
      }
    });
  }

  public boolean addLibrary(final AbstractProlLibrary library) throws IOException {
    assertNotDisposed();
    if (library == null) {
      throw new IllegalArgumentException("Library must not be null");
    }
    if (this.libraries.contains(library)) {
      return false;
    }

    libraries.add(0, library);

    final ConsultText consult = library.getClass().getAnnotation(ConsultText.class);
    if (consult != null) {
      final String text = consult.value();
      if (text.length() > 0) {
        this.consult(new StringReader(text));
      }
    }
    return true;
  }

  public KnowledgeBase getKnowledgeBase() {
    return this.knowledgeBase;
  }

  public boolean removeLibrary(final AbstractProlLibrary library) {
    this.assertNotDisposed();
    if (library == null) {
      throw new IllegalArgumentException("Library must not be null");
    }
    return libraries.remove(library);
  }

  public PredicateProcessor findProcessor(final TermStruct predicate) {
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
        .collect(Collectors.toList());
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

  public boolean hasSystemOperatorStartsWith(final String str) {
    return this.libraries.stream()
        .anyMatch(lib -> lib.hasSyatemOperatorStartsWith(str));
  }

  public void dispose() {
    if (this.disposed.compareAndSet(false, true)) {
      executorService.shutdownNow();

      final Set<ProlTrigger> notifiedTriggers = new HashSet<>();
      concat(triggersOnAssert.entrySet().stream(), triggersOnRetract.entrySet().stream())
          .forEachOrdered(mapentry -> mapentry.getValue().forEach((trigger) -> {
            try {
              if (!notifiedTriggers.contains(trigger)) {
                trigger.onContextHalting(this);
              }
            } finally {
              notifiedTriggers.add(trigger);
            }
          }));

      triggersOnAssert.clear();
      triggersOnRetract.clear();

      try {
        inReader = Optional.empty();
        outWriter = Optional.empty();
        errWriter = Optional.empty();

        concat(memPipes.values().stream(), concat(inputStreams.values().stream(), outputStreams.values().stream()))
            .forEach(channel -> Utils.doSilently(channel::close));

        this.memPipes.clear();
        this.inputStreams.clear();
        this.outputStreams.clear();

        getContextExecutorService().shutdownNow();
      } finally {
        // notify all libraries that the context is disposed
        libraries.forEach((library) -> library.contextHasBeenHalted(this));
      }
    }
  }

  public boolean isDisposed() {
    return disposed.get();
  }

  public void registerTrigger(final ProlTrigger trigger) {
    assertNotDisposed();
    final Map<String, ProlTriggerType> signatures = trigger.getSignatures();

    signatures.forEach((key, triggerType) -> {
      String signature = Utils.validateSignature(key);
      if (signature == null) {
        throw new IllegalArgumentException("Unsupported signature: " + key);
      }
      signature = Utils.normalizeSignature(signature);

      if (triggerType == ProlTriggerType.TRIGGER_ASSERT || triggerType == ProlTriggerType.TRIGGER_ASSERT_RETRACT) {
        this.triggersOnAssert.computeIfAbsent(signature, k -> new CopyOnWriteArrayList<>()).add(trigger);
      }

      if (triggerType == ProlTriggerType.TRIGGER_RETRACT || triggerType == ProlTriggerType.TRIGGER_ASSERT_RETRACT) {
        this.triggersOnRetract.computeIfAbsent(signature, k -> new CopyOnWriteArrayList<>()).add(trigger);
      }
    });
  }

  public void unregisterTrigger(final ProlTrigger trigger) {
    Stream.of(triggersOnAssert.entrySet().iterator(), triggersOnRetract.entrySet().iterator()).forEach(iterator -> {
      while (iterator.hasNext()) {
        final Entry<String, List<ProlTrigger>> entry = iterator.next();
        final List<ProlTrigger> lst = entry.getValue();
        if (lst.remove(trigger)) {
          if (lst.isEmpty()) {
            iterator.remove();
          }
        }
      }
    });
  }

  public boolean hasRegisteredTriggersForSignature(final String normalizedSignature, final ProlTriggerType observedEvent) {
    boolean result;
    switch (observedEvent) {
      case TRIGGER_ASSERT: {
        result = triggersOnAssert.containsKey(normalizedSignature);
      }
      break;
      case TRIGGER_RETRACT: {
        result = triggersOnRetract.containsKey(normalizedSignature);
      }
      break;
      case TRIGGER_ASSERT_RETRACT: {
        result = triggersOnAssert.containsKey(normalizedSignature);
        if (!result) {
          result = triggersOnRetract.containsKey(normalizedSignature);
        }
      }
      break;
      default: {
        throw new IllegalArgumentException("Unsupported observed event [" + observedEvent.name() + ']');
      }
    }

    return result;
  }

  public void notifyTriggersForSignature(final String normalizedSignature, final ProlTriggerType observedEvent) {
    ProlTrigger[] triggersToProcess = null;
    List<ProlTrigger> listOfTriggers;

    switch (observedEvent) {
      case TRIGGER_ASSERT: {
        listOfTriggers = triggersOnAssert.get(normalizedSignature);
      }
      break;
      case TRIGGER_RETRACT: {
        listOfTriggers = triggersOnRetract.get(normalizedSignature);
      }
      break;
      case TRIGGER_ASSERT_RETRACT: {
        final List<ProlTrigger> trigAssert = triggersOnAssert.get(normalizedSignature);
        final List<ProlTrigger> trigRetract = triggersOnRetract.get(normalizedSignature);

        if (trigAssert != null && trigRetract == null) {
          listOfTriggers = trigAssert;
        } else if (trigAssert == null && trigRetract != null) {
          listOfTriggers = trigRetract;
        } else {
          listOfTriggers = new ArrayList<>(trigAssert);
          listOfTriggers.addAll(trigRetract);
        }
      }
      break;
      default: {
        throw new IllegalArgumentException("Unsupported trigger event [" + observedEvent.name());
      }
    }

    if (listOfTriggers != null) {
      triggersToProcess = listOfTriggers.toArray(new ProlTrigger[0]);
    }


    if (triggersToProcess != null) {
      final TriggerEvent event = new TriggerEvent(this, normalizedSignature, observedEvent);
      for (final ProlTrigger trigger : triggersToProcess) {
        trigger.onTriggerEvent(event);
      }
    }
  }

  public void consult(final Reader source) throws IOException {

    final ProlReader reader = new ProlReader(source);

    final ProlTreeBuilder treeBuilder = new ProlTreeBuilder(this);
    final ProlTokenizer tokenizer = new ProlTokenizer();

    final Thread thisthread = Thread.currentThread();

    while (!thisthread.isInterrupted()) {

      final Term nextItem = treeBuilder.readPhraseAndMakeTree(tokenizer, reader);
      if (nextItem == null) {
        break;
      }

      final int line = tokenizer.getLastTokenLineNum();
      final int strpos = tokenizer.getLastTokenStrPos();

      try {
        switch (nextItem.getTermType()) {
          case ATOM: {
            this.knowledgeBase.assertZ(this, newStruct(nextItem));
          }
          break;
          case STRUCT: {
            final TermStruct struct = (TermStruct) nextItem;
            final Term functor = struct.getFunctor();

            switch (functor.getTermType()) {
              case OPERATOR: {
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
                  // goal
                  final Reader userreader = this.getStreamManager().getReaderForResource("user");
                  final Writer userwriter = this.getStreamManager().getWriterForResource("user", true);

                  final Term termGoal = struct.getElement(0);

                  if (userwriter != null) {
                    userwriter.write(String.format("Goal: %s%n", termGoal.forWrite()));
                  }
                  final Map<String, TermVar> varmap = new HashMap<>();
                  int solutioncounter = 0;

                  final ChoicePoint thisGoal = new ChoicePoint(termGoal, this, null);

                  while (!Thread.currentThread().isInterrupted()) {
                    varmap.clear();
                    if (solveGoal(thisGoal, varmap)) {
                      solutioncounter++;
                      if (userwriter != null) {
                        userwriter.write(String.format("%nYES%n"));
                        if (!varmap.isEmpty()) {
                          for (Entry<String, TermVar> avar : varmap.entrySet()) {
                            final String name = avar.getKey();
                            final TermVar value = avar.getValue();
                            userwriter.write(String.format("%s=%s%n", name, value.isFree() ? "???" : value.forWrite()));
                            userwriter.flush();
                          }
                        }
                      }
                    } else {
                      if (userwriter != null) {
                        userwriter.write(String.format("%n%d %s%n%nNO%n", solutioncounter, (solutioncounter > 1 ? "solutions" : "solution")));
                      }
                      break;
                    }
                    if (userwriter != null && userreader != null) {
                      userwriter.append("Next solution? ");
                      final int chr = userreader.read();
                      if (!(chr == ';' || chr == 'y' || chr == 'Y')) {
                        break;
                      } else {
                        userwriter.write('\n');
                      }
                    }
                  }
                  if (userwriter != null) {
                    userwriter.flush();
                  }

                  throw new ProlHaltExecutionException("Halted because goal failed.", 1);
                } else {
                  this.knowledgeBase.assertZ(this, struct);
                }
              }
              break;
              default: {
                this.knowledgeBase.assertZ(this, struct);
              }
              break;
            }
          }
          break;
          default: {
            throw new ProlKnowledgeBaseException("Such element can't be saved at knowledge base [" + nextItem + ']');
          }
        }
      } catch (Throwable ex) {
        if (ex instanceof ThreadDeath) {
          throw (ThreadDeath) ex;
        }
        throw new ParserException(ex.getMessage(), line, strpos, ex);
      }
    }
  }

  public boolean processGoal(final Term goalterm, final Map<String, TermVar> varTable) {
    final ChoicePoint goal = new ChoicePoint(goalterm, this, null);

    Term result = goal.next();

    if (result != null && varTable != null) {
      result.variables().forEach(e -> varTable.put(e.getText(), e));
    }

    return result != null;
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

  public ProlContext makeCopy() {
    return new ProlContext(this.contextId + "_copy", this.streamManager, this.knowledgeBase.makeCopy(), this.executorService);
  }

  public boolean hasOperatorStartsWith(String operator) {
    return this.knowledgeBase.hasOperatorStartsWith(this, operator);
  }

  public TermOperatorContainer findOperatorForName(String operator) {
    return this.knowledgeBase.findOperatorForName(this, operator);
  }
}
