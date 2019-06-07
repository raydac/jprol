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

import com.igormaznitsa.prol.annotations.Consult;
import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.containers.OperatorContainer;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.exceptions.ProlException;
import com.igormaznitsa.prol.io.*;
import com.igormaznitsa.prol.libraries.AbstractProlLibrary;
import com.igormaznitsa.prol.libraries.PredicateProcessor;
import com.igormaznitsa.prol.libraries.ProlCoreLibrary;
import com.igormaznitsa.prol.logic.triggers.ProlTrigger;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.logic.triggers.TriggerEvent;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.parser.ProlTreeBuilder;
import com.igormaznitsa.prol.trace.TraceListener;
import com.igormaznitsa.prol.utils.Utils;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.*;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import static java.util.stream.Stream.concat;

public final class ProlContext {
  public static final String ENGINE_VERSION = "2.0.0";
  public static final String ENGINE_NAME = "Prol";
  protected static final Logger LOG = Logger.getLogger(ProlContext.class.getCanonicalName());
  private static final String CONTEXT_HALTED_MSG = "Context halted";
  private static final String USER_STREAM = "user";
  private final ProlCoreLibrary coreLibraryInstance;
  private final String contextName;
  private final ReentrantLock knowledgeBaseLocker = new ReentrantLock();
  private final List<AbstractProlLibrary> libraries;
  private final ProlStreamManager streamManager;
  private final Map<String, ProlTextInputStream> inputStreams;
  private final Map<String, ProlTextOutputStream> outputStreams;
  private final Map<String, ProlMemoryPipe> pipes;
  private final Map<String, List<ProlTrigger>> triggersOnAssert;
  private final Map<String, List<ProlTrigger>> triggersOnRetract;
  private final ReentrantLock executorAndlockTableLocker = new ReentrantLock();
  private final ReentrantLock mappedObjectLocker = new ReentrantLock();
  private final ReentrantLock ioLocker = new ReentrantLock();
  private final ReentrantLock libLocker = new ReentrantLock();
  private final ReentrantLock triggerLocker = new ReentrantLock();
  private final KnowledgeBaseFactory knowledgeBaseFactory;
  private KnowledgeBase knowledgeBase;
  private ProlTextReader currentInputStream;
  private ProlTextWriter currentOutputStream;
  private ProlTextWriter currentErrorStream;
  private volatile boolean halted;
  private ThreadPoolExecutor executorService;
  private Map<String, ReentrantLock> lockerTable;
  private TraceListener defaultTraceListener;

  public ProlContext(final String name) {
    this(name, DefaultProlStreamManagerImpl.getInstance(), null);
  }

  public ProlContext(final String name, final ProlStreamManager streamManager) {
    this(name, streamManager, null);
  }

  public ProlContext(final String name, final ProlStreamManager streamManager, final KnowledgeBase knowledgebase) {
    this(streamManager, name, DefaultKnowledgeBaseFactory.getInstance());
    knowledgeBaseLocker.lock();
    try {
      if (knowledgebase == null) {
        this.knowledgeBase = knowledgeBaseFactory.makeDefaultKnowledgeBase(this, name + "_kbase");
      } else {
        this.knowledgeBase = knowledgebase;
      }
    } finally {
      knowledgeBaseLocker.unlock();
    }
    try {
      addLibrary(coreLibraryInstance);
    } catch (IOException ex) {
      throw new Error("Can't load core library", ex);
    }
  }

  private ProlContext(final ProlStreamManager streamManager, final String name, final KnowledgeBaseFactory kbfactory) {
    if (name == null) {
      throw new NullPointerException("The context name must not be null");
    }
    if (streamManager == null) {
      throw new NullPointerException("The stream manager for a context must be defined");
    }
    if (kbfactory == null) {
      throw new NullPointerException("The knowledge base factory is null");
    }

    this.coreLibraryInstance = new ProlCoreLibrary();
    this.contextName = name;

    this.knowledgeBaseFactory = kbfactory;

    this.libraries = new ArrayList<>(16);
    this.streamManager = streamManager;

    this.inputStreams = new HashMap<>();
    this.outputStreams = new HashMap<>();
    this.pipes = new HashMap<>();
    this.triggersOnAssert = new HashMap<>();
    this.triggersOnRetract = new HashMap<>();

    try {
      see(USER_STREAM);
      tell(USER_STREAM, true);
    } catch (IOException ex) {
      throw new Error("Can't init user streams", ex);
    }
  }

  public TraceListener getDefaultTraceListener() {
    return this.defaultTraceListener;
  }

  public void setDefaultTraceListener(final TraceListener traceListener) {
    this.defaultTraceListener = traceListener;
  }

  public final String getName() {
    return this.contextName;
  }

  public KnowledgeBaseFactory getKnowledgeBaseFactory() {
    return this.knowledgeBaseFactory;
  }

  public void changeKnowledgeBase(final String knowledge_base_id, final String knowledge_base_type) {
    if (knowledge_base_id == null) {
      throw new NullPointerException("Knowledge base Id is null");
    }
    if (knowledge_base_type == null) {
      throw new NullPointerException("Knowledge base type is null");
    }

    KnowledgeBase kb = this.knowledgeBaseFactory.makeKnowledgeBase(this, knowledge_base_id, knowledge_base_type);
    if (kb == null) {
      throw new IllegalArgumentException("Can't make knowledge base [" + knowledge_base_id + ',' + knowledge_base_type + ']');
    }
    this.knowledgeBaseLocker.lock();
    try {
      this.knowledgeBase = kb;
    } finally {
      this.knowledgeBaseLocker.unlock();
    }
  }

  public Future<?> solveAsynchronously(final String goal, final TraceListener traceListener) throws IOException, InterruptedException {
    if (goal == null) {
      throw new NullPointerException("The goal is null");
    }
    final Term term = new ProlTreeBuilder(this).readPhraseAndMakeTree(goal);
    return this.solveAsynchronously(term, traceListener);
  }

  public Future<?> solveAsynchronously(final Term goal, final TraceListener traceListener) {
    if (isHalted()) {
      throw new IllegalStateException("The context is halted");
    }

    if (goal == null) {
      throw new NullPointerException("The term to prove is null");
    }

    final ProlContext thisContext = this;

    return getContextExecutorService().submit(() -> {
      final Goal asyncGoal;
      try {
        asyncGoal = new Goal(goal, thisContext, traceListener);
      } catch (Exception ex) {
        LOG.log(Level.SEVERE, "Can't create a goal from the term \'" + goal.toString() + '\'', ex);
        return;
      }

      try {

        while (!Thread.currentThread().isInterrupted()) {
          final Term result = asyncGoal.solve();
          if (result == null) {
            break;
          }
        }
      } catch (InterruptedException ex) {
        LOG.log(Level.INFO, "Asynchronous thread for \'" + goal.toString() + "\' has been interrupted", ex);
      }
    });
  }

  public ThreadPoolExecutor getContextExecutorService() {
    if (executorService == null) {
      executorAndlockTableLocker.lock();
      try {
        if (executorService == null) {
          final ProlContextInsideThreadFactory threadFactory = new ProlContextInsideThreadFactory(this);
          executorService = (ThreadPoolExecutor) Executors.newCachedThreadPool(threadFactory);
          executorService.setRejectedExecutionHandler(threadFactory);
        }
      } finally {
        executorAndlockTableLocker.unlock();
      }
    }
    return executorService;
  }

  private Map<String, ReentrantLock> getLockerMap() {
    if (lockerTable == null) {
      executorAndlockTableLocker.lock();
      try {
        if (lockerTable == null) {
          lockerTable = new HashMap<>();
        }
      } finally {
        executorAndlockTableLocker.unlock();
      }

    }
    return lockerTable;
  }

  public ReentrantLock getLockerForName(final String name) {
    final Map<String, ReentrantLock> lockMap = getLockerMap();
    ReentrantLock locker = null;
    executorAndlockTableLocker.lock();
    try {
      locker = lockMap.get(name);
      if (locker == null) {
        locker = new ReentrantLock();
        lockMap.put(name, locker);
      }
    } finally {
      executorAndlockTableLocker.unlock();
    }
    return locker;
  }

  public void lockLockerForName(final String name) {
    final Map<String, ReentrantLock> lockMap = getLockerMap();
    ReentrantLock locker = null;

    final ReentrantLock exeLocker = executorAndlockTableLocker;

    exeLocker.lock();
    try {
      locker = lockMap.get(name);
      if (locker == null) {
        locker = new ReentrantLock();
        lockMap.put(name, locker);
      }
    } finally {
      exeLocker.unlock();
    }
    locker.lock();
  }

  public boolean trylockLockerForName(final String name) {
    final Map<String, ReentrantLock> lockMap = getLockerMap();
    ReentrantLock locker = null;

    final ReentrantLock exeLocker = executorAndlockTableLocker;

    exeLocker.lock();
    try {
      locker = lockMap.get(name);
      if (locker == null) {
        locker = new ReentrantLock();
        lockMap.put(name, locker);
      }
    } finally {
      exeLocker.unlock();
    }
    return locker.tryLock();
  }

  public void unlockLockerForName(final String name) {
    final Map<String, ReentrantLock> lockMap = getLockerMap();
    ReentrantLock locker = null;

    final ReentrantLock exeLocker = executorAndlockTableLocker;

    exeLocker.lock();
    try {
      locker = lockMap.get(name);
      if (locker == null) {
        throw new IllegalArgumentException("There is not any registered locker for name \'" + name + '\'');
      }
    } finally {
      exeLocker.unlock();
    }
    locker.unlock();
  }

  public final ProlMemoryPipe getMemoryPipeForName(final String identifier) {
    ioLocker.lock();
    try {
      return pipes.get(identifier);
    } finally {
      ioLocker.unlock();
    }
  }

  public final ProlStreamManager getStreamManager() {
    if (halted) {
      throw new ProlException(CONTEXT_HALTED_MSG);
    }
    ioLocker.lock();
    try {
      return streamManager;
    } finally {
      ioLocker.unlock();
    }
  }

  public final ProlTextWriter getCurrentOutStream() {
    if (halted) {
      throw new ProlException(CONTEXT_HALTED_MSG);
    }
    ioLocker.lock();
    try {
      return currentOutputStream;
    } finally {
      ioLocker.unlock();
    }
  }

  public final ProlTextWriter getCurrentErrStream() {
    if (halted) {
      throw new ProlException(CONTEXT_HALTED_MSG);
    }
    ioLocker.lock();
    try {
      return currentErrorStream;
    } finally {
      ioLocker.unlock();
    }
  }

  public ProlTextReader getCurrentInputStream() {
    if (halted) {
      throw new ProlException(CONTEXT_HALTED_MSG);
    }
    ioLocker.lock();
    try {
      return currentInputStream;
    } finally {
      ioLocker.unlock();
    }
  }

  public void tell(final String resourceId, final boolean append) throws IOException {
    if (halted) {
      throw new IllegalStateException(CONTEXT_HALTED_MSG);
    }

    ioLocker.lock();
    try {
      if (resourceId.length() > 0 && resourceId.charAt(0) == '+') {
        // it's pipe
        ProlMemoryPipe out = pipes.get(resourceId);
        if (out == null) {
          out = new ProlMemoryPipe(resourceId, this);
          pipes.put(resourceId, out);
        }
        currentOutputStream = out;
      } else {
        ProlTextOutputStream out = outputStreams.get(resourceId);
        if (out == null) {
          out = new ProlTextOutputStream(resourceId, this, append);
          outputStreams.put(resourceId, out);
        }
        currentOutputStream = out;
      }
    } finally {
      ioLocker.unlock();
    }
  }

  public void see(final String resourceId) throws IOException {
    if (halted) {
      throw new IllegalStateException(CONTEXT_HALTED_MSG);
    }

    ioLocker.lock();
    try {
      if (resourceId.length() > 0 && resourceId.charAt(0) == '+') {
        // it's a pipe
        ProlMemoryPipe in = pipes.get(resourceId);
        if (in == null) {
          in = new ProlMemoryPipe(resourceId, this);
          pipes.put(resourceId, in);
        }
        currentInputStream = in;
      } else {
        ProlTextInputStream in = inputStreams.get(resourceId);
        if (in == null) {
          in = new ProlTextInputStream(resourceId, this);
          inputStreams.put(resourceId, in);
        }
        currentInputStream = in;
      }
    } finally {
      ioLocker.unlock();
    }
  }

  public void seen() throws IOException {
    if (halted) {
      throw new IllegalStateException(CONTEXT_HALTED_MSG);
    }
    ioLocker.lock();
    try {
      try {
        if (currentInputStream == null) {
          return;
        }

        if (!currentInputStream.getResourceId().equals(USER_STREAM)) {
          currentInputStream.close();

          if (currentInputStream instanceof ProlMemoryPipe) {
            // remove the pipe channel
            pipes.remove(currentInputStream.getResourceId());
          } else {
            inputStreams.remove(currentInputStream.getResourceId());
          }
        }
      } finally {
        see(USER_STREAM);
      }
    } finally {
      ioLocker.unlock();
    }
  }

  public void told() throws IOException {
    if (halted) {
      throw new IllegalStateException(CONTEXT_HALTED_MSG);
    }

    ioLocker.lock();
    try {
      try {
        if (currentOutputStream == null) {
          return;
        }
        if (!currentOutputStream.getResourceId().equals(USER_STREAM)) {
          if (currentOutputStream instanceof ProlMemoryPipe) {
            //pipes.remove(currentOutputStream.getResourceId());
            ((ProlMemoryPipe) currentOutputStream).closeForWriteOnly();
          } else {
            currentOutputStream.close();
            outputStreams.remove(currentOutputStream.getResourceId());
          }
        }
      } finally {
        tell(USER_STREAM, true);
      }
    } finally {
      ioLocker.unlock();
    }
  }

  public boolean addLibrary(final AbstractProlLibrary library) throws IOException {
    if (halted) {
      throw new IllegalStateException(CONTEXT_HALTED_MSG);
    }

    if (library == null) {
      throw new IllegalArgumentException("Library must not be null");
    }

    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      if (libraries.contains(library)) {
        return false;
      }
      libraries.add(library);

      // consult with the library
      final Consult consult = library.getClass().getAnnotation(Consult.class);

      if (consult != null) {
        // check url
        final String url = consult.URL();
        if (url != null && url.length() > 0) {
          Utils.consultFromURLConnection(url, this);
        }
        // check urls
        final String[] urls = consult.URLs();
        if (urls != null && urls.length > 0) {
          for (final String urlToBeProcessed : urls) {
            if (urlToBeProcessed != null && urlToBeProcessed.length() > 0) {
              Utils.consultFromURLConnection(urlToBeProcessed, this);
            }
          }
        }

        // check text
        final String text = consult.Text();
        if (text != null && text.length() > 0) {
          // there is any text
          new ProlConsult(text, this).consult();
        }

        // check texts
        final String[] texts = consult.Texts();
        if (texts != null) {
          for (String text1 : texts) {
            new ProlConsult(text1, this).consult();
          }
        }
      }
    } finally {
      locker.unlock();
    }
    return true;
  }

  public KnowledgeBase getKnowledgeBase() {
    this.knowledgeBaseLocker.lock();
    try {
      return this.knowledgeBase;
    } finally {
      this.knowledgeBaseLocker.unlock();
    }
  }

  public boolean removeLibrary(final AbstractProlLibrary library) {
    if (library == null) {
      throw new IllegalArgumentException("Library must not be null");
    }
    if (halted) {
      throw new IllegalStateException(CONTEXT_HALTED_MSG);
    }

    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      return libraries.remove(library);
    } finally {
      locker.unlock();
    }
  }

  public void writeKnowledgeBase(final PrintWriter writer) {
    if (writer == null) {
      throw new IllegalArgumentException("Writer must not be null");
    }
    this.knowledgeBaseLocker.lock();
    try {
      this.knowledgeBase.write(writer);
    } finally {
      this.knowledgeBaseLocker.unlock();
    }
  }

  public PredicateProcessor findProcessor(final TermStruct predicate) {
    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final AbstractProlLibrary lib = libraries.get(li);

        final PredicateProcessor processor = lib.findProcessorForPredicate(predicate);
        if (processor != null) {
          return processor;
        }

        li--;
      }
      return PredicateProcessor.NULL_PROCESSOR;
    } finally {
      locker.unlock();
    }
  }

  public boolean hasZeroArityPredicateForName(final String name) {
    final ReentrantLock locker = this.libLocker;

    locker.lock();
    try {
      int li = this.libraries.size() - 1;
      boolean result = false;
      while (li >= 0) {
        final AbstractProlLibrary lib = this.libraries.get(li);
        if (lib.hasZeroArityPredicate(name)) {
          result = true;
          break;
        }
        li--;
      }
      return result;
    } finally {
      locker.unlock();
    }
  }

  public boolean hasPredicateAtLibraryForSignature(final String signature) {
    final ReentrantLock locker = this.libLocker;

    locker.lock();
    try {
      int li = this.libraries.size() - 1;
      while (li >= 0) {
        final AbstractProlLibrary lib = this.libraries.get(li);
        if (lib.hasPredicateForSignature(signature)) {
          return true;
        }
        li--;
      }
      return false;
    } finally {
      locker.unlock();
    }
  }

  public boolean isSystemOperator(final String name) {
    final ReentrantLock locker = this.libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final AbstractProlLibrary lib = libraries.get(li);
        if (lib.isSystemOperator(name)) {
          return true;
        }
        li--;
      }
      return false;
    } finally {
      locker.unlock();
    }
  }

  public OperatorContainer getSystemOperatorForName(final String name) {
    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final AbstractProlLibrary lib = libraries.get(li);
        final OperatorContainer result = lib.findSystemOperatorForName(name);
        if (result != null) {
          return result;
        }
        li--;
      }
      return null;
    } finally {
      locker.unlock();
    }
  }

  public boolean hasSystemOperatorStartsWith(final String str) {
    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final AbstractProlLibrary lib = libraries.get(li);
        if (lib.hasSyatemOperatorStartsWith(str)) {
          return true;
        }
        li--;
      }
      return false;
    } finally {
      locker.unlock();
    }
  }

  public void halt() {
    if (halted) {
      throw new IllegalStateException("Context already halted");
    } else {
      halted = true;
    }

    // stop all async threads
    executorAndlockTableLocker.lock();
    try {
      if (executorService != null) {
        executorService.shutdownNow();
      }
    } finally {
      executorAndlockTableLocker.unlock();
    }

    // notify all triggers that the context is halting
    triggerLocker.lock();
    try {
      final Set<ProlTrigger> notifiedTriggers = new HashSet<>();
      concat(triggersOnAssert.entrySet().stream(), triggersOnRetract.entrySet().stream())
          .forEachOrdered(mapentry -> mapentry.getValue().forEach((trigger) -> {
            try {
              if (!notifiedTriggers.contains(trigger)) {
                trigger.onContextHalting(this);
              }
            } catch (Throwable ex) {
              LOG.log(Level.SEVERE, "Exception during a context halting notification", ex);
            } finally {
              notifiedTriggers.add(trigger);
            }
          }));

      triggersOnAssert.clear();
      triggersOnRetract.clear();
    } finally {
      triggerLocker.unlock();
    }

    ioLocker.lock();
    libLocker.lock();
    try {
      try {
        currentInputStream = null;
        currentOutputStream = null;
        currentErrorStream = null;

        concat(pipes.values().stream(), concat(inputStreams.values().stream(), outputStreams.values().stream()))
            .forEach(channel -> Utils.doSilently(channel::close));

        this.pipes.clear();
        this.inputStreams.clear();
        this.outputStreams.clear();

        getContextExecutorService().shutdownNow();
      } finally {
        // notify all libraries that the context is halted
        libraries.forEach((library) -> {
          try {
            library.contextHasBeenHalted(this);
          } catch (Exception ex) {
            LOG.log(Level.SEVERE, "library.contextHasBeenHalted();", ex);
          }
        });
      }
    } finally {
      libLocker.unlock();
      ioLocker.unlock();
    }

  }

  public boolean isHalted() {
    return halted;
  }

  /**
   * Register a notification trigger
   *
   * @param trigger the trigger to be registered, must not be null
   */
  public void registerTrigger(final ProlTrigger trigger) {
    if (halted) {
      throw new IllegalStateException(CONTEXT_HALTED_MSG);
    }
    final Map<String, ProlTriggerType> signatures = trigger.getSignatures();

    triggerLocker.lock();
    try {
      for (Entry<String, ProlTriggerType> entry : signatures.entrySet()) {
        String signature = Utils.validateSignature(entry.getKey());

        if (signature == null) {
          throw new IllegalArgumentException("unsupported signature format [" + entry.getKey() + ']');
        }

        signature = Utils.normalizeSignature(signature);

        final ProlTriggerType triggerType = entry.getValue();

        List<ProlTrigger> triggerListAssert = null;
        List<ProlTrigger> triggerListRetract = null;

        if (triggerType == ProlTriggerType.TRIGGER_ASSERT || triggerType == ProlTriggerType.TRIGGER_ASSERT_RETRACT) {
          triggerListAssert = triggersOnAssert.get(signature);
          if (triggerListAssert == null) {
            triggerListAssert = new ArrayList<>();
            triggersOnAssert.put(signature, triggerListAssert);
          }
        }

        if (triggerType == ProlTriggerType.TRIGGER_RETRACT || triggerType == ProlTriggerType.TRIGGER_ASSERT_RETRACT) {
          triggerListRetract = triggersOnRetract.get(signature);
          if (triggerListRetract == null) {
            triggerListRetract = new ArrayList<>();
            triggersOnRetract.put(signature, triggerListRetract);
          }
        }

        if (triggerListAssert != null) {
          triggerListAssert.add(trigger);
          LOG.info("Registered handler as TRIGGER_ASSERT " + " for \'" + signature + "\', the handler is " + trigger.toString());
        }

        if (triggerListRetract != null) {
          triggerListRetract.add(trigger);
          LOG.info("Registered handler as TRIGGER_RETRACT " + " for \'" + signature + "\', the handler is " + trigger.toString());
        }
      }
    } finally {
      triggerLocker.unlock();
    }
  }

  /**
   * Unregister a notification trigger
   *
   * @param trigger the trigger to be removed from the inside trigger list, must
   *                not be null
   */
  public void unregisterTrigger(final ProlTrigger trigger) {
    triggerLocker.lock();
    try {
      // remove the trigger from both maps

      //assert
      Iterator<Entry<String, List<ProlTrigger>>> iterator = triggersOnAssert.entrySet().iterator();
      while (iterator.hasNext()) {
        final Entry<String, List<ProlTrigger>> entry = iterator.next();
        final List<ProlTrigger> lst = entry.getValue();
        if (lst.remove(trigger)) {
          if (lst.isEmpty()) {
            iterator.remove();
          }
        }
      }
      //retract
      iterator = triggersOnRetract.entrySet().iterator();
      while (iterator.hasNext()) {
        final Entry<String, List<ProlTrigger>> entry = iterator.next();
        final List<ProlTrigger> lst = entry.getValue();
        if (lst.remove(trigger)) {
          if (lst.isEmpty()) {
            iterator.remove();
          }
        }
      }
    } finally {
      triggerLocker.unlock();
    }
  }

  /**
   * Check that there is any registered trigger for the event type+signature
   * pair
   *
   * @param normalizedSignature the normalized signature to be checked, must not
   *                            be null
   * @param observedEvent       the event type to be checked, must not be null
   * @return true if there is any registered trigger for the pair, else false
   */
  public boolean hasRegisteredTriggersForSignature(final String normalizedSignature, final ProlTriggerType observedEvent) {
    triggerLocker.lock();
    try {
      boolean result = false;
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
    } finally {
      triggerLocker.unlock();
    }
  }

  /**
   * Notify all registered triggers which are registered for the signature+event
   * type pair
   *
   * @param normalizedSignature the normalized signature, must not be null
   * @param observedEvent       the detected trigger type, must not be null
   */
  public void notifyTriggersForSignature(final String normalizedSignature, final ProlTriggerType observedEvent) {
    ProlTrigger[] triggersToProcess = null;
    triggerLocker.lock();
    try {
      List<ProlTrigger> listOfTriggers = null;

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

    } finally {
      triggerLocker.unlock();
    }

    if (triggersToProcess != null) {
      final TriggerEvent event = new TriggerEvent(this, normalizedSignature, observedEvent);
      for (final ProlTrigger trigger : triggersToProcess) {
        try {
          trigger.onTriggerEvent(event);
        } catch (Exception ex) {
          LOG.log(Level.SEVERE, "Exception during a trigger processing [" + trigger.toString() + ']', ex);
        }
      }
    }
  }

  @Override
  public String toString() {
    return "ProlContext(" + contextName + ')' + '[' + super.toString() + ']';
  }

  /**
   * Allows to make copy of the context and its knowledge base
   *
   * @return new context containing snapshot of current knowledge base
   * @throws IOException it will be thrown if there is any exception during
   * initialization of IO streams
   */
  public ProlContext makeCopy() throws IOException {
    final ProlContext newContext = new ProlContext(this.streamManager, this.contextName + "_copy", this.knowledgeBaseFactory);

    newContext.libraries.addAll(libraries);
    knowledgeBaseLocker.lock();
    try {
      newContext.knowledgeBase = knowledgeBase == null ? null : knowledgeBase.makeCopy(newContext);
    } finally {
      knowledgeBaseLocker.unlock();
    }
    return newContext;
  }

  /**
   * Inside helper class to make Threads and handle their uncaught exceptions
   *
   * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
   */
  private static final class ProlContextInsideThreadFactory implements ThreadFactory, Thread.UncaughtExceptionHandler, RejectedExecutionHandler {

    private final String ownercontextName;

    ProlContextInsideThreadFactory(final ProlContext owner) {
      this.ownercontextName = owner.contextName;
    }

    @Override
    public Thread newThread(final Runnable runner) {
      final Thread thread = new Thread(runner, "Prol_" + ownercontextName + '_' + runner.toString());
      thread.setDaemon(true);
      thread.setUncaughtExceptionHandler(this);
      return thread;
    }

    @Override
    public void uncaughtException(final Thread thread, final Throwable exception) {
      LOG.log(Level.SEVERE, "Uncaught exception detected at " + thread.getName() + '[' + exception.toString() + ']', exception);
    }

    @Override
    public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
      LOG.log(Level.SEVERE, "Rejected execution!  {0}''{1}''", new Object[] {r.toString(), ownercontextName});
      throw new InternalError("A Prol thread was rejected. [" + ownercontextName + ']');
    }
  }
}
