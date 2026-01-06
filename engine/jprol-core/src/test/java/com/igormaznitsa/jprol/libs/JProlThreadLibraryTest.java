package com.igormaznitsa.jprol.libs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.exceptions.ProlAbortExecutionException;
import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.KeyValueTermStore;
import com.igormaznitsa.jprol.trace.JProlContextListener;
import com.igormaznitsa.jprol.utils.DefaultKeyValueTermStore;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;

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