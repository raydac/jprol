package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.annotations.Predicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlForkExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;

import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import static com.igormaznitsa.jprol.data.TermType.*;

public class JProlThreadLibrary extends AbstractJProlLibrary {
  public JProlThreadLibrary() {
    super("jprol-thread-lib");
  }

  private static List<Future<Term>> startListAsFork(final ChoicePoint goal, final TermStruct predicate, final TermList termlist) {
    List<AuxForkTask> goalList = goal.getPayload();

    if (goalList == null) {
      Set<Integer> varFlagTable = null;

      // the first call
      goalList = new ArrayList<>();
      TermList tlist = termlist;
      final JProlContext context = goal.getContext();
      while (!tlist.isNullList()) {
        final Term term = tlist.getHead();

        if (term.getTermType() != ATOM) {
          // find vars
          final Map<String, TermVar> varTable = term.allNamedVarsAsMap();
          if (!varTable.isEmpty()) {
            if (varFlagTable == null) {
              varFlagTable = new HashSet<>();
            }
            for (final Map.Entry<String, TermVar> pair : varTable.entrySet()) {
              final TermVar variable = pair.getValue();
              if (!variable.isAnonymous() && !variable.isGround()) {
                final Integer varUID = variable.getVarUid();
                if (varFlagTable.contains(varUID)) {
                  throw new ProlInstantiationErrorException("Variable \'" + variable.getText() + "\' is being shared between one or more parallel solving goals but not instantiated.", predicate);
                } else {
                  varFlagTable.add(varUID);
                }
              }
            }
          }
        }
        //----------------------------------------------------

        goalList.add(new AuxForkTask(term, context));

        final Term tail = tlist.getTail();
        if (tail.getTermType() == LIST) {
          tlist = (TermList) tail;
        } else {
          break;
        }
      }
      goal.setPayload(goalList);
    }

    List<Future<Term>> resultList = new ArrayList<>();

    final ExecutorService executor = goal.getContext().getContextExecutorService();

    for (final AuxForkTask task : goalList) {
      resultList.add(executor.submit(task));
    }

    return resultList;
  }

  @Predicate(signature = "fork/1", template = {"+list"}, reference = "Allows to prove a few goals (non linked between each other) in separated threads simultaneously, it is blocking the calling thread until all threads (started by the predicate) are completed. The fork implements AND operation (i.e. all goals have to be true else the predicate will fail).You must not have the same noninstantiated variables in terms that will be executed in different threads. The fork_error/1 will be thrown if any thread will throw an exception.")
  public static boolean predicateFORK1(final ChoicePoint goal, final TermStruct predicate) throws InterruptedException {
    TermList termlist = predicate.getElement(0).findNonVarOrSame();

    // invoke all taska and wait for them all
    final List<Future<Term>> taskList = startListAsFork(goal, predicate, termlist);

    boolean result = true;
    int taskindex = 0;

    List<Throwable> forkExceptions = null; // lazy initialization

    while (!termlist.isNullList()) {
      final Term head = termlist.getHead();

      try {
        final Future<Term> task = taskList.get(taskindex);
        final Term resultOfTask = task.get();
        if (resultOfTask == null) {
          // we have not result of the task
          result = false;
          break;
        } else {
          assertUnify(head, resultOfTask);
        }
      } catch (ExecutionException ex) {
        if (forkExceptions == null) {
          forkExceptions = new ArrayList<>();
        }

        forkExceptions.add(ex);
      }

      final Term tail = termlist.getTail();
      if (tail.getTermType() == LIST) {
        termlist = (TermList) tail;
      } else {
        break;
      }

      taskindex++;
    }

    if (forkExceptions != null) {

      throw new ProlForkExecutionException(predicate, forkExceptions.toArray(new Throwable[0]));
    }

    if (!result) {
      goal.cutVariants();
    }

    return result;
  }

  @Predicate(signature = "ifork/1", template = {"+list"}, reference = "It works like fork/1 but it will interrupt all non-completed threads of the fork if any of completed fails.")
  public static boolean predicateIFORK1(final ChoicePoint goal, final TermStruct predicate) throws InterruptedException {
    final TermList termlist = predicate.getElement(0).findNonVarOrSame();

    // invoke all taska and wait for them all
    final List<Future<Term>> taskList = startListAsFork(goal, predicate, termlist);

    boolean result = true;

    Exception forkException = null;
    Term termThrowsException = null;

    // parse the list for terms
    final Term[] parsedgoal = termlist.toArray();

    final Map<Integer, Future<Term>> workingThreads = new HashMap<>();
    int index = 0;
    for (final Future<Term> task : taskList) {
      workingThreads.put(index++, task);
    }

    while (!workingThreads.isEmpty()) {
      if (Thread.currentThread().isInterrupted()) {
        // stop all fork tasks
        for (final Future<Term> task : workingThreads.values()) {
          task.cancel(true);
        }
        workingThreads.clear();
        throw new InterruptedException();
      }

      boolean stopAllWorkingThreads = false;
      Integer completedIndex = null;

      for (final Map.Entry<Integer, Future<Term>> checkingtask : workingThreads.entrySet()) {
        final Future<Term> term = checkingtask.getValue();
        if (term.isDone()) {
          completedIndex = checkingtask.getKey();

          if (term.isCancelled()) {
            stopAllWorkingThreads = true;
            break;
          }
          try {
            final Term resultTerm = term.get();
            if (resultTerm == null) {
              // fail
              result = false;
              stopAllWorkingThreads = true;
              break;
            } else {
              // check the result
              final int threadindex = completedIndex;
              final Term originalGoal = parsedgoal[threadindex];
              assertUnify(originalGoal, resultTerm);
            }
          } catch (final Exception ex) {
            final int threadindex = completedIndex;
            termThrowsException = parsedgoal[threadindex];
            forkException = ex;
            stopAllWorkingThreads = true;
            result = false;
          }
          break;
        }
      }

      // remove the completed task
      if (completedIndex != null) {
        workingThreads.remove(completedIndex);
      }

      if (stopAllWorkingThreads) {
        workingThreads.values().forEach((task) -> task.cancel(true));
      }
    }

    if (forkException != null) {

      throw new ProlForkExecutionException(termThrowsException, new Throwable[] {forkException});
    }

    if (!result) {
      goal.cutVariants();
    }

    return result;
  }


  @Predicate(signature = "lock/1", template = {"+atom"}, reference = "Block current thread until it will be possible to lock an atom, don't forget unlock.")
  @Determined
  public static void predicateLOCK1(final ChoicePoint goal, final TermStruct predicate) {
    final String atomName = predicate.getElement(0).getText();
    goal.getContext().lockLockerForName(atomName);
  }

  @Predicate(signature = "unlock/1", template = {"+atom"}, reference = "Unlock a locker for its name and allow to continue work of waiting threads. If any other thread is the owner for the locker then permission_error/3 will be thrown.")
  @Determined
  public static void predicateUNLOCK1(final ChoicePoint goal, final TermStruct predicate) {
    final String atomName = predicate.getElement(0).getText();

    try {
      goal.getContext().unlockLockerForName(atomName);
    } catch (IllegalArgumentException ex) {
      throw new ProlExistenceErrorException("locker", "unlock", predicate, ex);
    } catch (IllegalMonitorStateException ex) {
      throw new ProlPermissionErrorException("locker", "unlock", predicate, ex);
    }
  }

  @Predicate(signature = "trylock/1", template = {"+atom"}, reference = "Try make lock for a named locker, if it is being locked already then fail else success.")
  @Determined
  public static boolean predicateTRYLOCK1(final ChoicePoint goal, final TermStruct predicate) {
    final String atomName = predicate.getElement(0).getText();
    return goal.getContext().trylockLockerForName(atomName);
  }

  @Predicate(signature = "async/1", template = {"+callable_term"}, reference = "Allows to next a goal asynchronously, it will be started as a daemon so it will be stopped when the main goal will be solved or failed. If there will be uncatched exception it will be just out at the log.")
  @Determined
  public static void predicateASYNC1(final ChoicePoint goal, final TermStruct predicate) {
    final Term goalToSolve = predicate.getElement(0).findNonVarOrSame();
    final JProlContext context = goal.getContext();

    // we have to check the goal because it must not have any noninstantiated variable!!!!
    final Map<String, TermVar> vars = goalToSolve.allNamedVarsAsMap();
    for (TermVar var : vars.values()) {
      if (!var.isGround()) {
        throw new ProlInstantiationErrorException("Variable \'" + var.getText() + "\' is not instantiated, you must have all variables instantiated for async/1 .", predicate);
      }
    }

    context.submitAsync(goalToSolve);
  }

  @Predicate(determined = true, signature = "waitasync/0", reference = "Blocking waiting until all daemon threads (started with either fork/1 or async/1) of the context will be done.")
  public static void predicateWAITASYNC0(final ChoicePoint goal, final TermStruct predicate) {
    final ExecutorService service = goal.getContext().getContextExecutorService();

    goal.getContext().waitForAllAsyncDone();

    if (Thread.currentThread().isInterrupted()) {
      service.shutdown();
      throw new ProlForkExecutionException("Execution interrupted", predicate, null);
    }
  }

  private static final class AuxForkTask implements Callable<Term> {

    private final Term term;
    private final ChoicePoint goal;

    AuxForkTask(final Term termToSolve, final JProlContext context) {
      this.term = termToSolve.makeClone().findNonVarOrDefault(null);

      if (termToSolve.getTermType() == VAR) {
        this.goal = null;
      } else {
        this.goal = new ChoicePoint(this.term, context, null);
      }
    }

    ChoicePoint getGoal() {
      return this.goal;
    }

    Term getTerm() {
      return this.term;
    }

    @Override
    public Term call() {
      return this.goal == null ? null : this.goal.next();
    }

    @Override
    public String toString() {
      return this.term.toString();
    }
  }
}
