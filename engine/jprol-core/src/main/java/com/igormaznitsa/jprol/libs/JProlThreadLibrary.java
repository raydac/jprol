package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.utils.ProlUtils.extractErrors;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlForkExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
public class JProlThreadLibrary extends AbstractJProlLibrary {
  public JProlThreadLibrary() {
    super("jprol-thread-lib");
  }

  private static List<CompletableFuture<Term>> asyncProveOnce(final JProlChoicePoint choicePoint,
                                                              final TermList list,
                                                              final boolean shareKnowledgeBase) {
    final Term[] terms = list.toArray(false);
    Arrays.stream(terms).forEach(x -> ProlAssertions.assertCallable(x.findNonVarOrSame()));
    return Arrays.stream(terms)
        .map(x -> choicePoint.getContext().proveOnceAsync(x.makeClone(), shareKnowledgeBase))
        .collect(Collectors.toList());
  }

  @JProlPredicate(determined = true, signature = "fork/1", args = {
      "+list"}, critical = true, reference = "Allows to prove a few goals (non linked between each other) in separated threads simultaneously, it is blocking the calling thread until all threads (started by the predicate) are completed. The fork implements AND operation (i.e. all goals have to be true else the predicate will fail).You must not have the same non-instantiated variables in terms that will be executed in different threads. The fork_error/1 will be thrown if any thread will throw an exception.")
  public static boolean predicateFORK1(final JProlChoicePoint choicePoint,
                                       final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertList(arg);
    }
    TermList taskTerms = (TermList) arg;

    final List<CompletableFuture<Term>> startedTasks = asyncProveOnce(choicePoint, taskTerms,
        choicePoint.getContext().isShareKnowledgeBaseBetweenThreads());
    CompletableFuture.allOf(startedTasks.toArray(new CompletableFuture<?>[0])).join();
    final Throwable[] errors = extractErrors(startedTasks);
    if (errors.length != 0) {
      throw new ProlForkExecutionException("Detected exception during fork/1", predicate, errors);
    }
    return startedTasks.stream().map(CompletableFuture::join).allMatch(Objects::nonNull);
  }

  @JProlPredicate(determined = true, signature = "ifork/1", args = {
      "+list"}, critical = true, reference = "It works like fork/1 but it will interrupt all non-completed threads of the fork if any of completed fails.")
  public static boolean predicateIFORK1(final JProlChoicePoint choicePoint,
                                        final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertList(arg);
    }
    TermList taskTerms = (TermList) arg;

    final List<CompletableFuture<Term>> startedTasks = asyncProveOnce(choicePoint, taskTerms,
        choicePoint.getContext().isShareKnowledgeBaseBetweenThreads());

    CompletableFuture.anyOf(startedTasks.toArray(new CompletableFuture<?>[0])).join();
    startedTasks.stream().filter(x -> !x.isDone()).forEach(x -> x.cancel(true));
    final Throwable[] errors = extractErrors(startedTasks);
    if (errors.length != 0) {
      throw new ProlForkExecutionException("Detected exception during ifork/1", predicate, errors);
    }
    return startedTasks.stream()
        .filter(x -> !x.isCancelled())
        .map(CompletableFuture::join)
        .allMatch(Objects::nonNull);
  }

  @JProlPredicate(determined = true, signature = "async/1", args = {
      "+callable"}, critical = true, reference = "Allows to next a goal asynchronously, it will be started as a daemon so it will be stopped when the main goal will be solved or failed. If there will be uncaught exception it will be just out at the log.")
  public static void predicateASYNC1(final JProlChoicePoint choicePoint,
                                     final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertCallable(term);
    }

    if (!term.isGround()) {
      throw new ProlInstantiationErrorException("Callable term must be bounded", predicate);
    }
    choicePoint.getContext()
        .proveAllAsync(term, choicePoint.getContext().isShareKnowledgeBaseBetweenThreads());
  }

  @JProlPredicate(determined = true, signature = "unlock/1", args = {
      "+atom"}, critical = true,
      reference = "Unlock a locker for its name and allow to continue work of waiting threads. If any other thread is the owner for the locker then permission_error/3 will be thrown.")
  public static void predicateUNLOCK1(final JProlChoicePoint choicePoint,
                                      final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertAtom(term);
    }

    try {
      choicePoint.getContext().unlockFor(term.getText());
    } catch (IllegalArgumentException ex) {
      throw new ProlExistenceErrorException("locker", "unlock", predicate, ex);
    } catch (IllegalMonitorStateException ex) {
      throw new ProlPermissionErrorException("locker", "unlock", predicate, ex);
    }
  }

  @JProlPredicate(determined = true, signature = "trylock/1", args = {
      "+atom"}, critical = true, reference = "Try make lock for a named locker, if it is being locked already then fail else success.")
  public static boolean predicateTRYLOCK1(final JProlChoicePoint choicePoint,
                                          final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertAtom(term);
    }
    return choicePoint.getContext().tryLockFor(term.getText());
  }

  @JProlPredicate(determined = true, signature = "lock/1", args = {
      "+atom"}, critical = true, reference = "Lock named locker, if it is being locked already then fail else success.")
  public static void predicateLOCK1(final JProlChoicePoint choicePoint,
                                    final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertAtom(term);
    }
    choicePoint.getContext().lockFor(term.getText());
  }

  @JProlPredicate(critical = true, determined = true, signature = "waitasync/0", reference = "Blocking waiting until all daemon threads (started with either fork/1 or async/1) of the context will be done.")
  public static void predicateWAITASYNC0(final JProlChoicePoint choicePoint,
                                         final TermStruct predicate) {
    choicePoint.getContext().waitAllAsyncTasks();
    if (Thread.currentThread().isInterrupted() || choicePoint.getContext().isDisposed()) {
      choicePoint.getContext().getContextExecutorService().shutdown();
      throw new ProlForkExecutionException("Execution interrupted", predicate, null);
    }
  }
}
