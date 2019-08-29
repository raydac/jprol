package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlForkExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.utils.ProlAssertions;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class JProlThreadLibrary extends AbstractJProlLibrary {
  public JProlThreadLibrary() {
    super("jprol-thread-lib");
  }

  private static List<CompletableFuture<Term>> asyncProveOnce(final ChoicePoint cpoint, final TermList list) {
    final Term[] terms = list.toArray();
    Arrays.stream(terms).forEach(x -> ProlAssertions.assertCallable(x.findNonVarOrSame()));
    return Arrays.stream(terms)
        .map(x -> cpoint.getContext().proveOnceAsync(x.makeClone()))
        .collect(Collectors.toList());
  }

  @JProlPredicate(determined = true, signature = "fork/1", args = {"+list"}, reference = "Allows to prove a few goals (non linked between each other) in separated threads simultaneously, it is blocking the calling thread until all threads (started by the predicate) are completed. The fork implements AND operation (i.e. all goals have to be true else the predicate will fail).You must not have the same noninstantiated variables in terms that will be executed in different threads. The fork_error/1 will be thrown if any thread will throw an exception.")
  public static boolean predicateFORK1(final ChoicePoint cpoint, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (cpoint.isArgsValidate()) {
      ProlAssertions.assertList(arg);
    }
    TermList taskTerms = (TermList) arg;

    final CompletableFuture<Term>[] startedTasks = (CompletableFuture<Term>[]) asyncProveOnce(cpoint, taskTerms).toArray(new CompletableFuture[0]);

    try {
      CompletableFuture.allOf(startedTasks).join();
    } catch (Throwable ex) {
      throw new ProlForkExecutionException(predicate, Arrays.stream(startedTasks)
          .filter(x -> x.isCompletedExceptionally())
          .map(x -> {
            try {
              x.get();
              return null;
            } catch (Throwable e) {
              return e;
            }
          }).filter(Objects::nonNull).toArray(Throwable[]::new));
    }
    return Arrays.stream(startedTasks).map(CompletableFuture::join).allMatch(Objects::nonNull);
  }

  @JProlPredicate(determined = true, signature = "ifork/1", args = {"+list"}, reference = "It works like fork/1 but it will interrupt all non-completed threads of the fork if any of completed fails.")
  public static boolean predicateIFORK1(final ChoicePoint cpoint, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (cpoint.isArgsValidate()) {
      ProlAssertions.assertList(arg);
    }
    TermList taskTerms = (TermList) arg;

    final CompletableFuture<Term>[] startedTasks = (CompletableFuture<Term>[]) asyncProveOnce(cpoint, taskTerms).toArray(new CompletableFuture[0]);

    try {
      CompletableFuture.anyOf(startedTasks).join();
    } catch (Throwable ex) {
      throw new ProlForkExecutionException(predicate, Arrays.stream(startedTasks)
          .filter(x -> x.isCompletedExceptionally())
          .map(x -> {
            try {
              x.get();
              return null;
            } catch (Throwable e) {
              return e;
            }
          }).filter(Objects::nonNull).toArray(Throwable[]::new));
    }
    Arrays.stream(startedTasks).filter(x -> !x.isDone()).forEach(x -> x.cancel(true));
    return Arrays.stream(startedTasks)
        .filter(x -> !x.isCancelled())
        .map(x -> x.join())
        .allMatch(Objects::nonNull);
  }

  @JProlPredicate(determined = true, signature = "async/1", args = {"+callable"}, reference = "Allows to next a goal asynchronously, it will be started as a daemon so it will be stopped when the main goal will be solved or failed. If there will be uncatched exception it will be just out at the log.")
  public static void predicateASYNC1(final ChoicePoint cpoint, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (cpoint.isArgsValidate()) {
      ProlAssertions.assertCallable(term);
    }

    if (!term.isGround()) {
      throw new ProlInstantiationErrorException("Callable term must be bounded", predicate);
    }

    cpoint.getContext().proveAllAsync(term);
  }

  @JProlPredicate(determined = true, signature = "unlock/1", args = {"+atom"}, reference = "Unlock a locker for its name and allow to continue work of waiting threads. If any other thread is the owner for the locker then permission_error/3 will be thrown.")
  public static void predicateUNLOCK1(final ChoicePoint cpoint, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (cpoint.isArgsValidate()) {
      ProlAssertions.assertAtom(term);
    }

    try {
      cpoint.getContext().unlockLockerForName(term.getText());
    } catch (IllegalArgumentException ex) {
      throw new ProlExistenceErrorException("locker", "unlock", predicate, ex);
    } catch (IllegalMonitorStateException ex) {
      throw new ProlPermissionErrorException("locker", "unlock", predicate, ex);
    }
  }

  @JProlPredicate(determined = true, signature = "trylock/1", args = {"+atom"}, reference = "Try make lock for a named locker, if it is being locked already then fail else success.")
  public static boolean predicateTRYLOCK1(final ChoicePoint cpoint, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (cpoint.isArgsValidate()) {
      ProlAssertions.assertAtom(term);
    }
    return cpoint.getContext().trylockLockerForName(term.getText());
  }

  @JProlPredicate(determined = true, signature = "lock/1", args = {"+atom"}, reference = "Lock named locker, if it is being locked already then fail else success.")
  public static void predicateLOCK1(final ChoicePoint cpoint, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (cpoint.isArgsValidate()) {
      ProlAssertions.assertAtom(term);
    }
    cpoint.getContext().lockLockerForName(term.getText());
  }

  @JProlPredicate(determined = true, signature = "waitasync/0", reference = "Blocking waiting until all daemon threads (started with either fork/1 or async/1) of the context will be done.")
  public static void predicateWAITASYNC0(final ChoicePoint cpoint, final TermStruct predicate) {
    cpoint.getContext().waitAllAsyncTasks();
    if (Thread.currentThread().isInterrupted()) {
      cpoint.getContext().getContextExecutorService().shutdown();
      throw new ProlForkExecutionException("Execution interrupted", predicate, null);
    }
  }
}
