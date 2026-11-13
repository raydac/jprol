package com.igormaznitsa.jprol.libs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlAbortExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlForkExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import com.igormaznitsa.jprol.logic.KeyValueTermStore;
import com.igormaznitsa.jprol.trace.JProlContextListener;
import com.igormaznitsa.jprol.utils.DefaultKeyValueTermStore;
import java.io.StringReader;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

class JProlThreadLibraryTest extends AbstractJProlTest {

  private static final Object CP_ASSOCIATED = new Object();

  private JProlContext prepareAsyncContext(final AsyncTestCounters counters,
                                           final KeyValueTermStore store) {
    final JProlContext context = makeAsyncTestContext(store);
    context.addLibrary(new TestLib());
    context.addContextListener(new JProlContextListener() {
      @Override
      public void onAsyncTaskStarted(JProlContext source, JProlContext taskContext, long taskId,
                                     CompletableFuture<Term> startedTask) {
        counters.startCounter.incrementAndGet();
      }

      @Override
      public void onAsyncTaskAborted(JProlContext source, JProlContext taskContext, long taskId,
                                     ProlAbortExecutionException exception) {
        counters.abortCounter.incrementAndGet();
      }

      @Override
      public void onAsyncUncaughtTaskException(JProlContext source, JProlContext taskContext,
                                               long taskId, Throwable error) {
        error.printStackTrace();
        counters.errors.add(error);
      }
    });
    return context;
  }

  @Test
  void testAsyncWaitasync() throws Exception {
    final long start = System.currentTimeMillis();
    final AsyncTestCounters counters = new AsyncTestCounters();
    try (final JProlContext context = prepareAsyncContext(counters, null)) {
      context.addLibrary(new JProlThreadLibrary());
      assertNotNull(
          context.makeChoicePoint("async(testAsync),waitasync.", CP_ASSOCIATED).prove());
      context.waitAllAsyncTasks(Duration.ofSeconds(3));
    }
    assertTrue(System.currentTimeMillis() - start >= 2000L);
    assertEquals(1, counters.startCounter.get());
    assertEquals(0, counters.abortCounter.get());
    assertTrue(counters.errors.isEmpty());
  }

  @Test
  void testAsyncPauseAbortWaitasync() throws Exception {
    final long start = System.currentTimeMillis();
    final AsyncTestCounters counters = new AsyncTestCounters();
    final KeyValueTermStore termStore = new DefaultKeyValueTermStore("test");

    try (final JProlContext context = prepareAsyncContext(counters, termStore)) {
      context.consult(
          "hello:-pause(2_000),nb_setval('hello', 'World'),nb_getval('hello', 'World'),nb_delete('hello'),nb_setval('hello22', 'World'),abort.");
      context.addLibrary(new JProlThreadLibrary());
      assertNotNull(
          context.makeChoicePoint("nb_setval('hello666', 'World'),async(hello),waitasync(10_000).",
              CP_ASSOCIATED).prove());
      context.waitAllAsyncTasks(null);
    }
    assertTrue(System.currentTimeMillis() - start >= 2000L);
    assertEquals(1, counters.startCounter.get());
    assertEquals(1, counters.abortCounter.get());
    assertTrue(counters.errors.isEmpty());

    Thread.sleep(100);
    assertTrue(termStore.isEmpty());
  }

  @Test
  void testLockUnlockRoundTripAndErrors() {
    try (final JProlContext context = makeAsyncTestContext(null)) {
      assertNotNull(context.makeChoicePoint(
              "lock(gate), lock(gate), trylock(gate), unlock(gate), unlock(gate), unlock(gate).")
          .prove());
      assertThrows(ProlPermissionErrorException.class,
          () -> context.makeChoicePoint("lock(gate), unlock(gate), unlock(gate).").prove());
      assertThrows(ProlExistenceErrorException.class,
          () -> context.makeChoicePoint("unlock(missing_gate).").prove());
    }
  }

  @Test
  @Timeout(5)
  void testReentrantLockKeepsOuterHold() throws Exception {
    try (final JProlContext context = makeAsyncTestContext(null)) {
      context.lockFor("gate");
      context.lockFor("gate");
      context.unlockFor("gate");

      final AtomicBoolean acquired = new AtomicBoolean(true);
      final Thread other = new Thread(() -> acquired.set(context.tryLockFor("gate")));
      other.start();
      other.join();

      assertFalse(acquired.get());
      context.unlockFor("gate");
      assertTrue(context.tryLockFor("gate"));
      context.unlockFor("gate");
    }
  }

  @Test
  @Timeout(5)
  void testDisposeReleasesLockWaiter() throws Exception {
    final JProlContext context = makeAsyncTestContext(null);
    context.lockFor("gate");
    final AtomicBoolean interrupted = new AtomicBoolean();
    final Thread waiter = new Thread(() -> {
      try {
        context.lockFor("gate");
      } catch (InterruptedException | RuntimeException ex) {
        interrupted.set(true);
      }
    });
    waiter.start();
    Thread.sleep(100L);
    context.dispose();
    waiter.join(2000L);

    assertFalse(waiter.isAlive());
    assertTrue(interrupted.get());
  }

  @Test
  @Timeout(20)
  void testSharedKnowledgeBaseStaysStableUnderFork() {
    final String facts = IntStream.rangeClosed(1, 30)
        .mapToObj(index -> "p(" + index + ").")
        .collect(Collectors.joining(" "));
    try (final JProlContext context = makeAsyncTestContext(null)) {
      context.setSystemFlag(JProlSystemFlag.SHARE_KNOWLEDGE_BASE, Terms.TRUE);
      context.consult(new StringReader(":- dynamic([p/1, q/1]). " + facts));
      for (int attempt = 0; attempt < 12; attempt++) {
        assertNotNull(context.makeChoicePoint(
                "fork([(findall(X,p(1),[1])), (findall(Y,p(30),[30])), (assertz(p(99)), retract(p(99)))]).")
            .prove());
      }
    }
  }

  @Test
  @Timeout(10)
  void testForkFailureAndExceptionPropagationAndIforkFastFail() {
    try (final JProlContext context = this.makeAsyncTestContext(null)) {
      assertNotNull(context.makeChoicePoint("fork([true, (X is 2 + 2, X =:= 4)]).").prove());
      assertFalse(context.makeChoicePoint("fork([true, fail, true]).").prove() != null);
      assertThrows(
          ProlForkExecutionException.class,
          () -> context.makeChoicePoint("fork([true, (_ is 1 // 0)]).").prove()
      );

      final long startMs = System.currentTimeMillis();
      assertFalse(context.makeChoicePoint("ifork([fail, pause(5000)]).").prove() != null);
      assertTrue(System.currentTimeMillis() - startMs < 3000L);
    }
  }

  @Test
  @Timeout(10)
  void testWaitasyncTimeoutRaisesForkExecutionException() {
    try (final JProlContext context = this.makeAsyncTestContext(null)) {
      assertThrows(
          ProlForkExecutionException.class,
          () -> context.makeChoicePoint("async(pause(3000)), waitasync(50).").prove()
      );
      context.cancelAllAsyncTasks();
    }
  }

  public static final class TestLib extends AbstractJProlLibrary {
    public TestLib() {
      super("test");
    }

    @JProlPredicate(signature = "testAsync/0", determined = true)
    public static void predicateTestAsync(final JProlChoicePoint cp, final TermStruct struct)
        throws Exception {
      assertSame(CP_ASSOCIATED, cp.getAssociatedObject());
      Thread.sleep(2000L);
    }
  }

  private static final class AsyncTestCounters {
    final AtomicInteger startCounter = new AtomicInteger();
    final AtomicInteger abortCounter = new AtomicInteger();
    final List<Throwable> errors = new CopyOnWriteArrayList<>();

    void reset() {
      this.startCounter.set(0);
      this.abortCounter.set(0);
      errors.clear();
    }
  }


}