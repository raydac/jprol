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

import com.igormaznitsa.prol.io.ProlStreamManager;
import com.igormaznitsa.prol.annotations.Consult;
import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.containers.OperatorContainer;
import com.igormaznitsa.prol.data.ConvertableToTerm;
import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermFloat;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.exceptions.ProlException;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.io.ProlMemoryPipe;
import com.igormaznitsa.prol.io.ProlTextInputStream;
import com.igormaznitsa.prol.io.ProlTextOutputStream;
import com.igormaznitsa.prol.io.ProlTextReader;
import com.igormaznitsa.prol.io.ProlTextWriter;
import com.igormaznitsa.prol.libraries.ProlAbstractLibrary;
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The class describes the prol engine context
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class ProlContext {

  /**
   * Inside helper class to make Threads and handle their uncaught exceptions
   *
   * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
   */
  private static final class ProlContextInsideThreadFactory implements ThreadFactory, Thread.UncaughtExceptionHandler, RejectedExecutionHandler {

    private final Logger ownLOG;
    private final String ownercontextName;

    ProlContextInsideThreadFactory(final ProlContext owner) {
      this.ownLOG = ProlContext.LOG;
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
      LOG.log(Level.SEVERE, "Rejected execution!  {0}''{1}''", new Object[]{r.toString(), ownercontextName});
      throw new InternalError("A Prol thread was rejected. [" + ownercontextName + ']');
    }
  }
  /**
   * Inside logger, the canonical class name is used as the logger identifier
   * (ProlContext.class.getCanonicalName())
   */
  protected static final Logger LOG = Logger.getLogger(ProlContext.class.getCanonicalName());
  /**
   * Message constant for the context halted situation
   */
  private static final String CONTEXT_HALTED_MSG = "Context halted";
  /**
   * The variable contains the context name
   */
  private final String contextName;
  /**
   * The constant has the string representation of the engine version
   */
  public static final String ENGINE_VERSION = "1.1.5";
  /**
   * The constant has the string representation of the engine name
   */
  public static final String ENGINE_NAME = "Prol";
  /**
   * The constant has the string representation of the user stream resource
   * identifier
   */
  private static final String USER_STREAM = "user";
  /**
   * This variable contains the knowledge base object for the context
   */
  private KnowledgeBase knowledgeBase;
  /**
   * The locker is being used to make knowledge base operations thread safe
   */
  private final ReentrantLock knowledgeBaseLocker = new ReentrantLock();
  /**
   * The variable contains the library list for the context
   */
  private final List<ProlAbstractLibrary> libraries;
  /**
   * The constant has the prol core library instance
   */
  private static final ProlCoreLibrary PROL_CORE_LIBRARY = new ProlCoreLibrary();
  /**
   * The variable contains the stream manager for the context
   */
  private final ProlStreamManager streamManager;
  /**
   * The table contains opened input streams
   */
  private final Map<String, ProlTextInputStream> inputStreams;
  /**
   * The table contains opened output streams
   */
  private final Map<String, ProlTextOutputStream> outputStreams;
  /**
   * The table contains opened memory pipes
   */
  private final Map<String, ProlMemoryPipe> pipes;
  /**
   * The table has trigger lists for assert processing
   */
  private final Map<String, List<ProlTrigger>> triggersOnAssert;
  /**
   * The table has trigger lists for retract processing
   */
  private final Map<String, List<ProlTrigger>> triggersOnRetract;
  /**
   * The table contains current active input stream
   */
  private ProlTextReader currentInputStream;
  /**
   * The table contains current active output stream
   */
  private ProlTextWriter currentOutputStream;
  /**
   * The flag shows that the context was halted (if it is true)
   */
  private volatile boolean halted;
  /**
   * Executor service for inside use, it will be lazy inited
   */
  private ThreadPoolExecutor executorService;
  /**
   * Inside map of lockers
   */
  private Map<String, ReentrantLock> lockerTable;
  /**
   * This lock is being used to synchronize simultaneous work with the inside
   * executor and the locker table
   */
  private final ReentrantLock executorAndlockTableLocker = new ReentrantLock();
  /**
   * The list contains searchers allow to find Java Object for Terms or String
   * names
   */
  private final List<ProlMappedObjectSearcher> mappedObjectSearchers = new ArrayList<ProlMappedObjectSearcher>();
  /**
   * Locker for work with mappedObjects
   */
  private final ReentrantLock mappedObjectLocker = new ReentrantLock();
  /**
   * Locker for IO operations
   */
  private final ReentrantLock ioLocker = new ReentrantLock();
  /**
   * Locker for library operations
   */
  private final ReentrantLock libLocker = new ReentrantLock();
  /**
   * Locker for trigger operations
   */
  private final ReentrantLock triggerLocker = new ReentrantLock();
  /**
   * Inside variable contains the default trace listener for the context
   */
  private TraceListener defaultTraceListener;
  /**
   * The variable contains the knowledge base factory which will be used by the
   * context
   */
  private final KnowledgeBaseFactory knowledgeBaseFactory;

  /**
   * Get the default trace listener for the context
   *
   * @return the default trace listener, it can be null
   */
  public TraceListener getDefaultTraceListener() {
    return defaultTraceListener;
  }

  /**
   * Set the default trace listener for the context
   *
   * @param traceListener the trace listener which one will be the default trace
   * listener for the context, it can be null
   */
  public void setDefaultTraceListener(final TraceListener traceListener) {
    defaultTraceListener = traceListener;
  }

  /**
   * Get the context name which was supplied when the instance was created
   *
   * @return the context name as String, must not be null
   */
  public final String getName() {
    return contextName;
  }

  /**
   * Get the knowledge base factory used by the context
   *
   * @return the knowledge base factory
   */
  public KnowledgeBaseFactory getKnowledgeBaseFactory() {
    return knowledgeBaseFactory;
  }

  /**
   * A constructor allows to use automatically the default prol stream manager
   *
   * @param name the name for created context
   * @throws IOException it will be thrown if there are problems to set default
   * user input-output streams ('user')
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   * @throws NullPointerException it will be thrown if a needed argument is null
   * @see DefaultProlStreamManagerImpl
   */
  public ProlContext(final String name) throws IOException, InterruptedException {
    this(name, DefaultProlStreamManagerImpl.getInstance(), null);
  }

  /**
   * A constructor allows to set the name and a stream manager
   *
   * @param name the name for created context
   * @param streamManager the stream manager to be used bye the context, it can
   * be null
   * @throws IOException it will be thrown if there are problems to set default
   * user input-output streams ('user')
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   * @throws NullPointerException it will be thrown if a needed argument is null
   * @see DefaultProlStreamManagerImpl
   */
  public ProlContext(final String name, final ProlStreamManager streamManager) throws IOException, InterruptedException {
    this(name, streamManager, null);
  }

  /**
   * A constructor
   *
   * @param name the name for created context
   * @param streamManager the stream manager for created context, it can be null
   * @param knowledgebase the knowledge base to be used for the context (the
   * instance not copy will be used!), it can be null so a MemoryKnowledgeBase
   * instance will be created and used
   * @throws IOException it will be thrown if there are problems to set default
   * user input-output streams ('user')
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   * @throws NullPointerException it will be thrown if a needed argument is null
   * @see DefaultProlStreamManagerImpl
   */
  public ProlContext(final String name, final ProlStreamManager streamManager, final KnowledgeBase knowledgebase) throws IOException, InterruptedException {
    this(streamManager, name, DefaultKnowledgeBaseFactory.getInstance());
    knowledgeBaseLocker.lock();
    try {
      if (knowledgebase == null) {
        this.knowledgeBase = knowledgeBaseFactory.makeDefaultKnowledgeBase(this, name + "_kbase");
      }
      else {
        this.knowledgeBase = knowledgebase;
      }
    }
    finally {
      knowledgeBaseLocker.unlock();
    }
    addLibrary(PROL_CORE_LIBRARY);
  }

  /**
   * Inside constructor for special purposes
   *
   * @param streamManager the stream manager to be used for stream manipulations
   * of the context, it can be null
   * @param name the name of the context instance
   * @param kbfactory a knowledge base factory to be used by the context, must
   * not be null
   * @throws IOException it will be thrown if there is any exception during the
   * stream initialization operations
   * @see DefaultProlStreamManagerImpl
   */
  private ProlContext(final ProlStreamManager streamManager, final String name, final KnowledgeBaseFactory kbfactory) throws IOException {
    if (name == null) {
      throw new NullPointerException("The context name must not be null");
    }
    this.contextName = name;
    if (streamManager == null) {
      throw new NullPointerException("The stream manager for a context must be defined");
    }

    if (kbfactory == null) {
      throw new NullPointerException("The knowledge base factory is null");
    }

    this.knowledgeBaseFactory = kbfactory;

    this.libraries = new ArrayList<ProlAbstractLibrary>(16);
    this.streamManager = streamManager;

    this.inputStreams = new HashMap<String, ProlTextInputStream>();
    this.outputStreams = new HashMap<String, ProlTextOutputStream>();
    this.pipes = new HashMap<String, ProlMemoryPipe>();
    this.triggersOnAssert = new HashMap<String, List<ProlTrigger>>();
    this.triggersOnRetract = new HashMap<String, List<ProlTrigger>>();

    see(USER_STREAM);
    tell(USER_STREAM, true);
  }

  /**
   * To change knowledge base for the context with the current knowledge base
   * factory factory
   *
   * @param knowledge_base_id the id of new knowledge base, must not be null
   * @param knowledge_base_type the knowledge base type, must not be null
   * @throws IllegalArgumentException if it can't make a knowledge base for such
   * parameters
   */
  public void changeKnowledgeBase(final String knowledge_base_id, final String knowledge_base_type) {
    if (knowledge_base_id == null) {
      throw new NullPointerException("Knowledge base Id is null");
    }
    if (knowledge_base_type == null) {
      throw new NullPointerException("Knowledge base type is null");
    }

    KnowledgeBase kb = knowledgeBaseFactory.makeKnowledgeBase(this, knowledge_base_id, knowledge_base_type);
    if (kb == null) {
      throw new IllegalArgumentException("Can't make knowledge base [" + knowledge_base_id + ',' + knowledge_base_type + ']');
    }
    knowledgeBaseLocker.lock();
    try {
      knowledgeBase = kb;
    }
    finally {
      knowledgeBaseLocker.unlock();
    }
  }

  /**
   * Add a searcher into the searcher list. The searches will be used to find
   * associated java object for a Term or just a string name
   *
   * @param searcher the searcher to be added
   */
  public void addMappedObjectSearcher(final ProlMappedObjectSearcher searcher) {
    if (halted) {
      throw new IllegalStateException(CONTEXT_HALTED_MSG);
    }
    if (searcher != null) {
      mappedObjectLocker.lock();
      try {
        mappedObjectSearchers.add(searcher);
      }
      finally {
        mappedObjectLocker.unlock();
      }
    }
  }

  /**
   * Remove a searcher from the searcher list
   *
   * @param searcher the searcher to be removed from the list
   */
  public void removeMappedObjectFinder(final ProlMappedObjectSearcher searcher) {
    if (searcher != null) {
      mappedObjectLocker.lock();
      try {
        mappedObjectSearchers.remove(searcher);
      }
      finally {
        mappedObjectLocker.unlock();
      }
    }
  }

  /**
   * Find the mapped object through registered searchers.
   *
   * @param name the string name of searched object, must not be null
   * @return found Object or null if it is not found
   */
  public Object findMappedObjectForName(final String name) {
    Object result = null;

    final ReentrantLock locker = mappedObjectLocker;

    locker.lock();
    try {
      final Iterator<ProlMappedObjectSearcher> getters = mappedObjectSearchers.iterator();
      while (getters.hasNext()) {
        final ProlMappedObjectSearcher current = getters.next();
        result = current.findProlMappedObject(name);
        if (result != null) {
          break;
        }
      }
    }
    finally {
      locker.unlock();
    }
    return result;
  }

  /**
   * Find the key name for a mapped object
   *
   * @param mappedObject the object for which one we are looking for the name
   * @return the name as a String or null if it is not found
   */
  public String findNameForMappedObject(final Object mappedObject) {
    String result = null;
    final ReentrantLock locker = mappedObjectLocker;

    locker.lock();
    try {
      final Iterator<ProlMappedObjectSearcher> getters = mappedObjectSearchers.iterator();
      while (getters.hasNext()) {
        final ProlMappedObjectSearcher current = getters.next();
        result = current.findProlMappedTerm(mappedObject);
        if (result != null) {
          break;
        }
      }
    }
    finally {
      locker.unlock();
    }
    return result;
  }

  /**
   * Find the mapped object through registered searchers. Remember that only
   * pure Term object can be used (not any its successor or TermNumber)
   *
   * @param term the term for which we should find the mapped object
   * @return Object if it has been found, else null
   */
  public Object findMappedObjectForTerm(final Term term) {
    Object result = null;

    if (term != null && term.getClass() == Term.class) {
      final String termAsText = term.getText();
      result = findMappedObjectForName(termAsText);
    }
    return result;
  }

  /**
   * Prove a goal asynchronously which is represented as a String object
   *
   * @param goal the goal which should be proven, must not be null
   * @param traceListener the trace listener for the new goal, can be null
   * @return the future object for the new created thread
   * @throws IOException it will be thrown if the string representation is wrong
   * @throws InterruptedException it will be thrown if the term parsing process
   * is interrupted
   * @throws NullPointerException it will be thrown if the goal is null
   * @throws IllegalStateException if the context is halted
   */
  public Future<?> solveAsynchronously(final String goal, final TraceListener traceListener) throws IOException, InterruptedException {
    if (goal == null) {
      throw new NullPointerException("The goal is null");
    }
    final Term term = new ProlTreeBuilder(this).readPhraseAndMakeTree(goal);
    return this.solveAsynchronously(term, traceListener);
  }

  /**
   * Prove a goal asynchronously. The thread will be added in the asynchronous
   * daemon (!) thread pool of the context and can be checked through the
   * "waytasync/0" term
   *
   * @param goal the term which should be proven, it will be proven in a loop
   * until it fails, it must not be null
   * @param traceListener the trace listener for the goal to be proven, it can
   * be null
   * @return the future object for the new created thread
   * @throws NullPointerException it will be thrown if the goal is null
   * @throws IllegalStateException if the context is halted
   */
  public Future<?> solveAsynchronously(final Term goal, final TraceListener traceListener) {
    if (isHalted()) {
      throw new IllegalStateException("The context is halted");
    }

    if (goal == null) {
      throw new NullPointerException("The term to prove is null");
    }

    final ProlContext thisContext = this;

    return getContextExecutorService().submit(new Runnable() {

      @Override
      public void run() {
        Goal asyncGoal = null;
        try {
          asyncGoal = new Goal(goal, thisContext, traceListener);
        }
        catch (Exception ex) {
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
        }
        catch (InterruptedException ex) {
          LOG.log(Level.INFO, "Asynchronous thread for \'" + goal.toString() + "\' has been interrupted", ex);
        }

      }
    });
  }

  /**
   * Getter for the inside context executor service, it will be lazy inited when
   * the first call detected
   *
   * @return an ExecutorService object which can be used for task execution
   */
  public ThreadPoolExecutor getContextExecutorService() {
    if (executorService == null) {
      executorAndlockTableLocker.lock();
      try {
        if (executorService == null) {
          //int cpuNumber = 2;
          //final OperatingSystemMXBean osBean = ManagementFactory.getOperatingSystemMXBean();
          //if (osBean != null) {
          //    cpuNumber = osBean.getAvailableProcessors() + 1;
          //}

          final ProlContextInsideThreadFactory threadFactory = new ProlContextInsideThreadFactory(this);
          executorService = (ThreadPoolExecutor) Executors.newCachedThreadPool(threadFactory);
          executorService.setRejectedExecutionHandler(threadFactory);
        }
      }
      finally {
        executorAndlockTableLocker.unlock();
      }
    }
    return executorService;
  }

  /**
   * Getter for the inside context locker map, it will be lazy inited when the
   * first call detected. It is direct link to the map!
   *
   * @return a Map containing Name-Locker pairs, you must synchronize the Map
   * for your use
   */
  private Map<String, ReentrantLock> getLockerMap() {
    if (lockerTable == null) {
      executorAndlockTableLocker.lock();
      try {
        if (lockerTable == null) {
          lockerTable = new HashMap<String, ReentrantLock>();
        }
      }
      finally {
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
    }
    finally {
      executorAndlockTableLocker.unlock();
    }
    return locker;
  }

  /**
   * Lock an inside context locker for its name
   *
   * @param name the locker name, must not be null
   */
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
    }
    finally {
      exeLocker.unlock();
    }
    locker.lock();
  }

  /**
   * Try lock an inside context locker for its name
   *
   * @param name the name of the locker, must not be null
   * @return true if the locker has been locked successfully else false
   */
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
    }
    finally {
      exeLocker.unlock();
    }
    return locker.tryLock();
  }

  /**
   * Unlock inside context locker for name
   *
   * @param name the locker name, must not be null
   * @throws IllegalArgumentException it will be thrown if the locker name is
   * unknown in the context
   * @throws IllegalMonitorStateException it will be thrown if the locker is
   * being keept by other thread
   */
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
    }
    finally {
      exeLocker.unlock();
    }
    locker.unlock();
  }

  /**
   * Get a memory pipe for its name (it starts from the '+' char)
   *
   * @param identifier the pipe name identifier, must not be null
   * @return the memory pipe for the name or null if the pipe is not found
   */
  public final ProlMemoryPipe getMemoryPipeForName(final String identifier) {
    ioLocker.lock();
    try {
      return pipes.get(identifier);
    }
    finally {
      ioLocker.unlock();
    }
  }

  /**
   * Get the stream manager for the context
   *
   * @return the stream manager, must not be null
   */
  public final ProlStreamManager getStreamManager() {
    if (halted) {
      throw new ProlException(CONTEXT_HALTED_MSG);
    }
    ioLocker.lock();
    try {
      return streamManager;
    }
    finally {
      ioLocker.unlock();
    }
  }

  /**
   * Get current active output stream
   *
   * @return current active output stream, must not be null
   */
  public final ProlTextWriter getCurrentOutStream() {
    if (halted) {
      throw new ProlException(CONTEXT_HALTED_MSG);
    }
    ioLocker.lock();
    try {
      return currentOutputStream;
    }
    finally {
      ioLocker.unlock();
    }
  }

  /**
   * Get current active input stream
   *
   * @return current active input stream, must not be null
   */
  public ProlTextReader getCurrentInputStream() {
    if (halted) {
      throw new ProlException(CONTEXT_HALTED_MSG);
    }
    ioLocker.lock();
    try {
      return currentInputStream;
    }
    finally {
      ioLocker.unlock();
    }
  }

  /**
   * Activate an output stream for its identifier if the identifier starts with
   * '+' it will be opened as a memory pipe
   *
   * @param resourceId the identifier of the output stream, must not be null
   * @param append if true, the output stream will be opened for append new
   * information at this end
   * @throws IOException it will be thrown if there will be any transport
   * problem
   */
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
      }
      else {
        ProlTextOutputStream out = outputStreams.get(resourceId);
        if (out == null) {
          out = new ProlTextOutputStream(resourceId, this, append);
          outputStreams.put(resourceId, out);
        }
        currentOutputStream = out;
      }
    }
    finally {
      ioLocker.unlock();
    }
  }

  /**
   * Activate an input stream for its identifier if the identifier starts with
   * '+' it will be opened as a memory pipe
   *
   * @param resourceId the identifier of the input stream, must not be null
   * @throws IOException it will be thrown if there will be any transport error
   */
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
      }
      else {
        ProlTextInputStream in = inputStreams.get(resourceId);
        if (in == null) {
          in = new ProlTextInputStream(resourceId, this);
          inputStreams.put(resourceId, in);
        }
        currentInputStream = in;
      }
    }
    finally {
      ioLocker.unlock();
    }
  }

  /**
   * Deactivate and close current active input stream (if it is not 'user'
   * stream)
   *
   * @throws IOException it will be thrown if there is any transport problem
   */
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
          }
          else {
            inputStreams.remove(currentInputStream.getResourceId());
          }
        }
      }
      finally {
        see(USER_STREAM);
      }
    }
    finally {
      ioLocker.unlock();
    }
  }

  /**
   * Deactivate and close current active output stream (if it is not 'user'
   * stream)
   *
   * @throws IOException it will be thrown if there is any transport problem
   */
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
          }
          else {
            currentOutputStream.close();
            outputStreams.remove(currentOutputStream.getResourceId());
          }
        }
      }
      finally {
        tell(USER_STREAM, true);
      }
    }
    finally {
      ioLocker.unlock();
    }
  }

  /**
   * Add a library into the context and process the consult annotations of the
   * library.
   *
   * @param library the library to be added
   * @return true if the library has been added successfully, else false (if the
   * library alreade presented)
   * @throws IOException it will be thrown if there are problems to read an
   * outside data
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   * @see com.igormaznitsa.prol.annotations.Consult
   */
  public boolean addLibrary(final ProlAbstractLibrary library) throws IOException, InterruptedException {
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
          for (int li = 0; li < urls.length; li++) {
            final String urlToBeProcessed = urls[li];
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
          for (int li = 0; li < texts.length; li++) {
            new ProlConsult(texts[li], this).consult();
          }
        }
      }
    }
    finally {
      locker.unlock();
    }
    return true;
  }

  /**
   * Get the knowledge base for the context
   *
   * @return the knowledge base for the context, must not be null
   */
  public KnowledgeBase getKnowledgeBase() {
    knowledgeBaseLocker.lock();
    try {
      return knowledgeBase;
    }
    finally {
      knowledgeBaseLocker.unlock();
    }
  }

  /**
   * Remove a library from the context
   *
   * @param library the library to be removed from the library list of he
   * context
   * @return true if the library has been removed, else false
   */
  public boolean removeLibrary(final ProlAbstractLibrary library) {
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
    }
    finally {
      locker.unlock();
    }
  }

  /**
   * Write all dynamic content of the context knowledge base into stream
   *
   * @param writer a print writer to be used for output data, must not be null
   */
  public void writeKnowledgeBase(final PrintWriter writer) {
    if (writer == null) {
      throw new IllegalArgumentException("Writer must not be null");
    }
    knowledgeBaseLocker.lock();
    try {
      knowledgeBase.write(writer);
    }
    finally {
      knowledgeBaseLocker.unlock();
    }
  }

  /**
   * Find first predicate processor compatible with the predicate
   *
   * @param predicate the structure for which we will look for a processor, must
   * not be null
   * @return found processor object or NULL_PROCESSOR if has not found anything
   * @see com.igormaznitsa.prol.libraries.PredicateProcessor
   */
  public PredicateProcessor findProcessor(final TermStruct predicate) {
    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final ProlAbstractLibrary lib = libraries.get(li);

        final PredicateProcessor processor = lib.findProcessorForPredicate(predicate);
        if (processor != null) {
          return processor;
        }

        li--;
      }
      return PredicateProcessor.NULL_PROCESSOR;
    }
    finally {
      locker.unlock();
    }
  }

  /**
   * Check that as minimum one from libraries in the context has a predicate has
   * a signature
   *
   * @param signature the signature to be checked, must not be null
   * @return true if a predicate has been found, else false
   */
  public boolean hasPredicateAtLibraryForSignature(final String signature) {
    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final ProlAbstractLibrary lib = libraries.get(li);
        if (lib.hasPredicateForSignature(signature)) {
          return true;
        }
        li--;
      }
      return false;
    }
    finally {
      locker.unlock();
    }
  }

  /**
   * Check that there is a system operator for a name
   *
   * @param name the name to be checked, must not not null
   * @return true if there is such a system operator else false
   */
  public boolean isSystemOperator(final String name) {
    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final ProlAbstractLibrary lib = libraries.get(li);
        if (lib.isSystemOperator(name)) {
          return true;
        }
        li--;
      }
      return false;
    }
    finally {
      locker.unlock();
    }
  }

  /**
   * Find system operator for name in libraries
   *
   * @param name the name to be looked for
   * @return found operator container for the name or null if there is not any
   * with such name
   */
  public OperatorContainer getSystemOperatorForName(final String name) {
    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final ProlAbstractLibrary lib = libraries.get(li);
        final OperatorContainer result = lib.findSystemOperatorForName(name);
        if (result != null) {
          return result;
        }
        li--;
      }
      return null;
    }
    finally {
      locker.unlock();
    }
  }

  /**
   * Check that a library has a system operator start with a substring
   *
   * @param str the substring to be checked, must not be null
   * @return true if there is such operator, else false
   */
  public boolean hasSystemOperatorStartsWith(final String str) {
    final ReentrantLock locker = libLocker;

    locker.lock();
    try {
      int li = libraries.size() - 1;
      while (li >= 0) {
        final ProlAbstractLibrary lib = libraries.get(li);
        if (lib.hasSyatemOperatorStartsWith(str)) {
          return true;
        }
        li--;
      }
      return false;
    }
    finally {
      locker.unlock();
    }
  }

  /**
   * Halt the context and release its keeping resources
   *
   * @throws IllegalStateException if the context has been halted already
   */
  public void halt() {
    if (halted) {
      throw new IllegalStateException("Context already halted");
    }
    else {
      halted = true;
    }

    // stop all async threads
    executorAndlockTableLocker.lock();
    try {
      if (executorService != null) {
        executorService.shutdownNow();
      }
    }
    finally {
      executorAndlockTableLocker.unlock();
    }

    // notify all triggers that the context is halting
    triggerLocker.lock();
    try {
      final IdentityHashMap<ProlTrigger, Set<?>> notifiedTriggers = new IdentityHashMap<ProlTrigger, Set<?>>();// it is being used to save the set of already notified triggers, we should notify each object only one time
      for (final Entry<String, List<ProlTrigger>> mapentry : triggersOnAssert.entrySet()) {
        for (final ProlTrigger trigger : mapentry.getValue()) {
          try {
            if (!notifiedTriggers.containsKey(trigger)) {
              trigger.onContextHalting(this);
            }
          }
          catch (Throwable ex) {
            LOG.log(Level.SEVERE, "Exception during a context halting notification", ex);
          }
          finally {
            notifiedTriggers.put(trigger, Collections.emptySet());
          }
        }
      }
      for (final Entry<String, List<ProlTrigger>> mapentry : triggersOnRetract.entrySet()) {
        for (final ProlTrigger trigger : mapentry.getValue()) {
          try {
            if (!notifiedTriggers.containsKey(trigger)) {
              trigger.onContextHalting(this);
            }
          }
          catch (Throwable ex) {
            LOG.log(Level.SEVERE, "Exception during a context halting notification", ex);
          }
          finally {
            notifiedTriggers.put(trigger, Collections.emptySet());
          }
        }
      }

      notifiedTriggers.clear();

      triggersOnAssert.clear();
      triggersOnRetract.clear();
    }
    finally {
      triggerLocker.unlock();
    }

    ioLocker.lock();
    libLocker.lock();
    try {
      try {
        final Iterator<ProlMemoryPipe> memPipes = pipes.values().iterator();
        while (memPipes.hasNext()) {
          try {
            memPipes.next().close();
          }
          catch (Throwable thr) {
          }
        }

        final Iterator<ProlTextInputStream> inStreams = inputStreams.values().iterator();
        while (inStreams.hasNext()) {
          try {
            inStreams.next().close();
          }
          catch (Throwable thr) {
          }
        }
        inputStreams.clear();

        final Iterator<ProlTextOutputStream> outStreams = outputStreams.values().iterator();
        while (outStreams.hasNext()) {
          try {
            outStreams.next().close();
          }
          catch (Throwable thr) {
          }
        }
        outputStreams.clear();

        getContextExecutorService().shutdownNow();
        currentInputStream = null;
        currentOutputStream = null;
      }
      finally {
        // notify all libraries that the context is halted
        for (ProlAbstractLibrary library : libraries) {
          try {
            library.contextHasBeenHalted(this);
          }
          catch (Exception ex) {
            LOG.log(Level.SEVERE, "library.contextHasBeenHalted();", ex);
          }
        }
      }
    }
    finally {
      libLocker.unlock();
      ioLocker.unlock();
    }

  }

  /**
   * Check that the context has been halted
   *
   * @return true if the context has been halted, else false
   */
  public boolean isHalted() {
    return halted;
  }

  /**
   * Allows to convert a java object into its Prol representation. null will be
   * converted as TermList.NULLLIST Term will be just returned as th result
   * ConvertableToTerm will be asked for Term and the term will be returned as
   * the result String will be converted as Term Number (integer or float only)
   * will be converted as NumericTerm Collection will be converted as TermList
   * Object[] will be converted as TermStruct, the first element will be used as
   * the functor (it's toString() value), empty array will be converted ass
   * TermList.NULLLIST
   *
   * @param object the object to be converted into a Prol representation, can be
   * null
   * @return a Term object which is a Prol compatible representation of the Java
   * object
   * @throws IllegalArgumentException it will be thrown if the object has an
   * unsupported type
   * @throws NullPointerException it will be thrown if any successor of
   * ConvertableToTerm will return null as result
   */
  public Term objectAsTerm(final Object object) {
    Term result = null;

    if (object == null) {
      // return NULLLIST
      result = TermList.NULLLIST;
    }
    else if (object instanceof Term) {
      result = (Term) object;
    }
    else if (object instanceof ConvertableToTerm) {
      final ConvertableToTerm cterm = (ConvertableToTerm) object;
      result = cterm.asProlTerm();
      if (result == null) {
        throw new NullPointerException("asProlTerm() returned null [" + object.toString() + ']');
      }
    }
    else if (object instanceof String) {
      // atom or mapped object
      result = new Term((String) object);
    }
    else if (object instanceof Number) {
      if (object instanceof Integer) {
        result = new TermInteger(((Integer) object).intValue());
      }
      else if (object instanceof Float) {
        result = new TermFloat(((Float) object).floatValue());
      }
      else {
        throw new IllegalArgumentException("Unsupported number format.");
      }
    }
    else if (object instanceof Collection) {
      // list
      final Collection<?> lst = (Collection) object;

      if (lst.isEmpty()) {
        result = TermList.NULLLIST;
      }
      else {
        TermList accumulator = null;
        // fill the list
        for (Object item : lst) {
          if (accumulator == null) {
            accumulator = new TermList(objectAsTerm(item));
            result = accumulator; // the first list
          }
          else {
            accumulator = TermList.appendItem(accumulator, objectAsTerm(item));
          }
        }
      }
    }
    else if (object instanceof Object[]) {
      // struct
      final Object[] array = (Object[]) object;
      final int arrlen = array.length;
      if (arrlen == 0) {
        // as null list
        result = TermList.NULLLIST;
      }
      else {
        final Term functor = new Term(array[0].toString());
        if (arrlen == 1) {
          result = new TermStruct(functor);
        }
        else {
          final Term[] terms = new Term[arrlen - 1];
          for (int li = 1; li < arrlen; li++) {
            terms[li - 1] = objectAsTerm(array[li]);
          }
          result = new TermStruct(functor, terms);
        }
      }
    }
    else {
      throw new IllegalArgumentException("Unsupported object to be represented as a Term");
    }
    return result;
  }

  /**
   * Convert a Term into its Java representation Term will be converted as
   * java.lang.String or as an mapped Java object if it is found in the context
   * NumericTerm will be converted as java.lang.Number object TermList will be
   * converted as List<Object>
   * TermStruct will be converted as Object[] where the first element if the
   * functor (it will be converted as other Term so you can use mapped objects)
   *
   * @param term the term to be converted, must not be null
   * @return a Java object represents the Term
   * @throws IllegalArgumentException it will be thrown if a non-instantiate
   * variable has been detected
   */
  public Object termAsObject(final Term term) {
    final Term cterm = Utils.getTermFromElement(term);
    Object result = null;
    switch (cterm.getTermType()) {
      case Term.TYPE_ATOM: {
        if (cterm instanceof NumericTerm) {
          // as numeric value
          result = ((TermInteger) cterm).getNumericValue();
        }
        else {
          // find mapped object
          final String termtext = cterm.getText();
          result = findMappedObjectForName(termtext);
          if (result == null) {
            // there is not any mapped object, so return the text
            result = termtext;
          }
        }
      }
      break;
      case Term.TYPE_LIST: {
        // make List<Object>
        final List<Object> list = new ArrayList<Object>();
        TermList tlist = (TermList) cterm;
        while (tlist.isNullList()) {
          list.add(termAsObject(tlist.getHead()));
          final Term tail = tlist.getTail();
          if (tail.getTermType() == Term.TYPE_LIST) {
            tlist = (TermList) tail;
          }
          else {
            list.add(termAsObject(tail));
            break;
          }
        }
        result = list;
      }
      break;
      case Term.TYPE_OPERATORS:
      case Term.TYPE_OPERATOR: {
        // just as text
        result = cterm.getText();
      }
      break;
      case Term.TYPE_STRUCT: {
        // struct
        final TermStruct sterm = (TermStruct) cterm;
        final int size = sterm.getArity() + 1;
        final Object[] array = new Object[size];

        // the first element is the term
        array[0] = termAsObject(sterm.getFunctor());

        // other elements
        for (int li = 1; li < size; li++) {
          array[li] = termAsObject(sterm.getElement(li - 1));
        }

        result = array;
      }
      break;
      case Term.TYPE_VAR: {
        // non instantiate variable
        throw new IllegalArgumentException("It is non instantiate variable \'" + cterm.getText() + "\'");
      }
      default: {
        // new type detected
        throw new ProlCriticalError("Unsupported term type");
      }
    }
    return result;
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
            triggerListAssert = new ArrayList<ProlTrigger>();
            triggersOnAssert.put(signature, triggerListAssert);
          }
        }

        if (triggerType == ProlTriggerType.TRIGGER_RETRACT || triggerType == ProlTriggerType.TRIGGER_ASSERT_RETRACT) {
          triggerListRetract = triggersOnRetract.get(signature);
          if (triggerListRetract == null) {
            triggerListRetract = new ArrayList<ProlTrigger>();
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
    }
    finally {
      triggerLocker.unlock();
    }
  }

  /**
   * Unregister a notification trigger
   *
   * @param trigger the trigger to be removed from the inside trigger list, must
   * not be null
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
    }
    finally {
      triggerLocker.unlock();
    }
  }

  /**
   * Check that there is any registered trigger for the event type+signature
   * pair
   *
   * @param normalizedSignature the normalized signature to be checked, must not
   * be null
   * @param observedEvent the event type to be checked, must not be null
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
    }
    finally {
      triggerLocker.unlock();
    }
  }

  /**
   * Notify all registered triggers which are registered for the signature+event
   * type pair
   *
   * @param normalizedSignature the normalized signature, must not be null
   * @param observedEvent the detected trigger type, must not be null
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
          }
          else if (trigAssert == null && trigRetract != null) {
            listOfTriggers = trigRetract;
          }
          else {
            listOfTriggers = new ArrayList<ProlTrigger>(trigAssert);
            listOfTriggers.addAll(trigRetract);
          }
        }
        break;
        default: {
          throw new IllegalArgumentException("Unsupported trigger event [" + observedEvent.name());
        }
      }

      if (listOfTriggers != null) {
        triggersToProcess = listOfTriggers.toArray(new ProlTrigger[listOfTriggers.size()]);
      }

    }
    finally {
      triggerLocker.unlock();
    }

    if (triggersToProcess != null) {
      final TriggerEvent event = new TriggerEvent(this, normalizedSignature, observedEvent);
      for (int li = 0; li < triggersToProcess.length; li++) {
        final ProlTrigger trigger = triggersToProcess[li];
        try {
          trigger.onTriggerEvent(event);
        }
        catch (Exception ex) {
          LOG.log(Level.SEVERE, "Exception during a trigger processing [" + trigger.toString() + ']', ex);
        }
      }
    }
  }

  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder("ProlContext(").append(contextName).append(')').append('[').append(super.toString()).append(']');
    return builder.toString();
  }

  /**
   * Allows to make copy of the context and its knowledge base
   *
   * @return new context containing snapshot of current knowledge base
   * @throws IOException it will be thrown if there is any exception during
   * initialization of IO streams
   */
  public ProlContext makeCopy() throws IOException {
    final ProlContext newContext = new ProlContext(streamManager, this.contextName + "_copy", knowledgeBaseFactory);

    newContext.libraries.addAll(libraries);
    knowledgeBaseLocker.lock();
    try {
      newContext.knowledgeBase = knowledgeBase == null ? null : knowledgeBase.makeCopy(newContext);
    }
    finally {
      knowledgeBaseLocker.unlock();
    }
    return newContext;
  }
}
