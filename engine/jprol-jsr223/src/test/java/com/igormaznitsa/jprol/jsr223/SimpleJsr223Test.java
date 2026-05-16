package com.igormaznitsa.jprol.jsr223;


import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.libs.JProlCoreGuardedLibrary;
import com.igormaznitsa.jprol.libs.JProlIoLibrary;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import com.igormaznitsa.prologparser.ParserContext;
import java.io.Writer;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import javax.script.SimpleBindings;
import org.junit.jupiter.api.Test;

public class SimpleJsr223Test {

  private ScriptEngine findScriptEngine() {
    return this.findScriptEngine(List.of());
  }

  private ScriptEngine findScriptEngine(final List<AbstractJProlLibrary> additionalLibraries) {
    final JProlScriptEngineFactory factory =
        (JProlScriptEngineFactory) new ScriptEngineManager().getEngineFactories()
            .stream().filter(x -> x.getNames().contains("jprol.prolog")).findFirst().orElseThrow();
    final ScriptEngine engine = factory.createScriptEngine(additionalLibraries);
    engine.getBindings(ScriptContext.ENGINE_SCOPE)
        .put(JProlScriptEngine.JPROL_CONTEXT_FLAGS, Map.of(
            JProlSystemFlag.VERIFY.asText(), false));
    return engine;
  }

  @Test
  void testDisableCriticalPredicateClause() {
    final ScriptEngine engine =
        findScriptEngine(List.of(new JProlCoreGuardedLibrary(), new JProlIoLibrary()));
    final Bindings globalBindings = new SimpleBindings();
    engine.setBindings(globalBindings, ScriptContext.GLOBAL_SCOPE);

    engine.getBindings(ScriptContext.GLOBAL_SCOPE).put(
        JProlScriptEngine.JPROL_GLOBAL_GUARD_PREDICATE,
        (JProlGuardPredicate) (sourceLibrary, choicePoint, predicateIndicator) -> !"clause/2".equals(
            predicateIndicator));

    assertDoesNotThrow(() -> engine.eval("?-write('hello')."));
    assertThrowsExactly(ScriptException.class, () ->
        engine.eval("?-clause(a(X),(X = 10))."));

    final ScriptEngine engineWithoutRestriction =
        findScriptEngine(List.of(new JProlCoreGuardedLibrary(), new JProlIoLibrary()));
    assertDoesNotThrow(() ->
        engineWithoutRestriction.eval("?-clause(a(X),(X = 10))."));

  }

  @Test
  void testFactoryGetMethodCallSyntax() {
    ScriptEngine engine = findScriptEngine();
    assertEquals("?- '0print_hello'(123, 44.34, '\\'world\\'').",
        engine.getFactory().getMethodCallSyntax("?-", "0print_hello", "123", "44.34", "'world'"));
  }

  @Test
  void testFactoryGetProgram() {
    ScriptEngine engine = findScriptEngine();
    assertEquals("?- write('hello world'), nl, repeat.",
        engine.getFactory().getProgram("write('hello world').", "nl  ", "  repeat   "));
  }


  @Test
  public void testSimpleEngineCall() throws Exception {
    ScriptEngine engine = new ScriptEngineManager().getEngineByName("jprol");
    engine.put("List", List.of(1, 2, 3, 4, 5));
    assertTrue((boolean) engine.eval(
        "list_length([],0). list_length([_],1). list_length([_|Tail],X):-list_length(Tail,Y), X is Y + 1.\n"
            + "?- list_length(List,Length)."));
    assertEquals(5L, engine.get("Length"));
  }

  @Test
  public void testSimpleEngineCallWithCustomBindings() throws Exception {
    ScriptEngine engine = new ScriptEngineManager().getEngineByName("jprol");
    Bindings bindings = new SimpleBindings();
    bindings.put("List", List.of(1, 2, 3, 4, 5));
    assertTrue((boolean) engine.eval(
        "list_length([],0). list_length([_],1). list_length([_|Tail],X):-list_length(Tail,Y), X is Y + 1.\n"
            + "?- list_length(List,Length).", bindings));
    assertNull(engine.get("List"));
    assertNull(engine.get("Length"));
    assertEquals(5L, bindings.get("Length"));
  }

  @Test
  public void testSimpleEngineInvocableCall() throws Exception {
    ScriptEngine engine = new ScriptEngineManager().getEngineByName("jprol");
    ((JProlScriptEngine) engine).consult(
        "list_length([],0). list_length([_],1). list_length([_|Tail],X):-list_length(Tail,Y), X is Y + 1.");
    Invocable invocable = (Invocable) engine;
    assertTrue((boolean) invocable.invokeFunction("list_length", List.of(1, 2, 3, 4, 5),
        Terms.newVar("Length")));
    assertEquals(5L, engine.get("Length"));
    assertTrue((boolean) invocable.invokeMethod(null, "list_length", List.of(1, 2, 3, 4, 5, 6),
        Terms.newVar("Length")));
    assertEquals(6L, engine.get("Length"));

    assertNotNull(invocable.getInterface(JProlScriptEngine.class));
    assertNotNull(invocable.getInterface(ScriptContext.class));
    assertNotNull(invocable.getInterface(JProlScriptEngineContext.class).getKnowledgeBase());
    assertNotNull(invocable.getInterface(KnowledgeBase.class));
    assertNotNull(invocable.getInterface(JProlContext.class));
    assertNotNull(invocable.getInterface(ParserContext.class));
    assertNull(invocable.getInterface(Writer.class));

    invocable.getInterface(JProlScriptEngineContext.class).clear(true);
    invocable.getInterface(JProlScriptEngine.class).close();
    assertThrowsExactly(IllegalStateException.class,
        () -> invocable.invokeMethod(null, "list_length", List.of(1, 2, 3, 4, 5, 6),
            Terms.newVar("Length")));
  }

  @Test
  public void testBindings() throws Exception {
    var engine = findScriptEngine();
    var bindings = engine.getBindings(ScriptContext.ENGINE_SCOPE);
    bindings.put("X", 3);
    assertTrue((Boolean) engine.eval(
        "cube(X,Y):-Y is X * X * X. ?-cube(X,Result). ?- this_line_must_be_ignored('lalala')."));
    assertEquals(27L, bindings.get("Result"));
  }

  @Test
  public void testMultithread() throws Exception {
    var engine = findScriptEngine();
    final AtomicInteger errorCounter = new AtomicInteger();
    final AtomicInteger completionCounter = new AtomicInteger();
    final int threads = 5000;
    final CountDownLatch start = new CountDownLatch(threads);
    final CountDownLatch end = new CountDownLatch(threads);
    for (int i = 0; i < threads; i++) {
      final long arg = i;
      final long expected = i * i;
      final Thread thread = new Thread(() -> {
        try {
          start.await();
          var bindings = engine.getBindings(ScriptContext.ENGINE_SCOPE);
          bindings.put("X", arg);
          final boolean completed =
              (Boolean) engine.eval("square(X,Y):-Y is X * X. ?-square(X,Result).");
          if (!completed) {
            throw new IllegalStateException("Goal is false");
          }
          final Long result = (Long) bindings.get("Result");
          if (result != expected) {
            throw new IllegalStateException(
                "Unexpected result, expected " + expected + " but calculated " + result);
          }
        } catch (InterruptedException ex) {
          Thread.currentThread().interrupt();
        } catch (Throwable ex) {
          errorCounter.incrementAndGet();
          System.err.println("Error: " + ex.getMessage());
        } finally {
          completionCounter.incrementAndGet();
          end.countDown();
        }
      }, "thread-test-" + i);
      thread.start();
      start.countDown();
    }
    end.await();
    assertEquals(threads, completionCounter.get());
    assertEquals(0, errorCounter.get());
    ((JProlScriptEngine) engine).gc();
    assertEquals(1, ((JProlScriptEngine) engine).size(),
        "Expected only thread alive in engine stores");
  }

  @Test
  public void testMultithreadInvocable() throws Exception {
    var engine = findScriptEngine();
    final AtomicInteger errorCounter = new AtomicInteger();
    final AtomicInteger completionCounter = new AtomicInteger();
    final int threads = 5000;
    final CountDownLatch start = new CountDownLatch(threads);
    final CountDownLatch end = new CountDownLatch(threads);

    assertTrue((boolean) engine.eval("square(X,Y):-Y is X * X."));

    for (int i = 0; i < threads; i++) {
      final long arg = i;
      final long expected = i * i;
      final Thread thread = new Thread(() -> {
        try {
          start.await();
          var bindings = engine.getBindings(ScriptContext.ENGINE_SCOPE);
          assertTrue(
              (boolean) ((Invocable) engine).invokeFunction("square", arg, Terms.newVar("Result")));
          final Long result = (Long) bindings.get("Result");
          if (result != expected) {
            throw new IllegalStateException(
                "Unexpected result, expected " + expected + " but calculated " + result);
          }
        } catch (InterruptedException ex) {
          Thread.currentThread().interrupt();
        } catch (Throwable ex) {
          ex.printStackTrace();
          errorCounter.incrementAndGet();
        } finally {
          completionCounter.incrementAndGet();
          end.countDown();
        }
      }, "thread-test-" + i);
      thread.start();
      start.countDown();
    }
    end.await();
    assertEquals(threads, completionCounter.get());
    assertEquals(0, errorCounter.get());
  }

  @Test
  public void testMultithreadCompiled() throws Exception {
    var engine = findScriptEngine();
    final AtomicInteger errorCounter = new AtomicInteger();
    final AtomicInteger completionCounter = new AtomicInteger();
    final int threads = 5000;
    final CountDownLatch start = new CountDownLatch(threads);
    final CountDownLatch end = new CountDownLatch(threads);

    final CompiledScript compiled =
        ((Compilable) engine).compile("square(X,Y):-Y is X * X. ?-square(X,Result).");

    for (int i = 0; i < threads; i++) {
      final long arg = i;
      final long expected = i * i;
      final Thread thread = new Thread(() -> {
        try {
          start.await();
          var bindings = compiled.getEngine().getBindings(ScriptContext.ENGINE_SCOPE);
          bindings.put("X", arg);
          assertTrue((boolean) compiled.eval());
          final Long result = (Long) bindings.get("Result");
          if (result != expected) {
            throw new IllegalStateException(
                "Unexpected result, expected " + expected + " but calculated " + result);
          }
        } catch (InterruptedException ex) {
          Thread.currentThread().interrupt();
        } catch (Throwable ex) {
          errorCounter.incrementAndGet();
          System.err.println("Error: " + ex.getMessage());
        } finally {
          completionCounter.incrementAndGet();
          end.countDown();
        }
      }, "thread-test-" + i);
      thread.start();
      start.countDown();
    }
    end.await();
    assertEquals(threads, completionCounter.get());
    assertEquals(0, errorCounter.get());
  }

  @Test
  public void testMultithreadCompiledInvocable() throws Exception {
    var engine = findScriptEngine();
    final AtomicInteger errorCounter = new AtomicInteger();
    final AtomicInteger completionCounter = new AtomicInteger();
    final int threads = 5000;
    final CountDownLatch start = new CountDownLatch(threads);
    final CountDownLatch end = new CountDownLatch(threads);

    final CompiledScript compiled =
        ((Compilable) engine).compile("square(X,Y):-Y is X * X.");

    for (int i = 0; i < threads; i++) {
      final long arg = i;
      final long expected = i * i;
      final Thread thread = new Thread(() -> {
        try {
          start.await();
          assertTrue((boolean) ((Invocable) compiled).invokeFunction("square", arg,
              Terms.newVar("Result")));
          final Long result = (Long) engine.get("Result");
          if (result != expected) {
            throw new IllegalStateException(
                "Unexpected result, expected " + expected + " but calculated " + result);
          }
        } catch (InterruptedException ex) {
          Thread.currentThread().interrupt();
        } catch (Throwable ex) {
          errorCounter.incrementAndGet();
          System.err.println("Error: " + ex.getMessage());
        } finally {
          completionCounter.incrementAndGet();
          end.countDown();
        }
      }, "thread-test-" + i);
      thread.start();
      start.countDown();
    }
    end.await();
    assertEquals(threads, completionCounter.get());
    assertEquals(0, errorCounter.get());
  }

  @Test
  void testSudoku() throws Exception {
    var engine = new ScriptEngineManager().getEngineByName("jprol");
    assertTrue((boolean) engine.eval(
        "num(1). num(2). num(3). num(4). num(5). num(6). num(7). num(8). num(9)."
            + "diff(A,B,C,D,E,F,G,H,I) :-"
            + "num(A), num(B), num(C), num(D), num(E), num(F), num(G), num(H), num(I),"
            + "A\\=B, A\\=C, A\\=D, A\\=E, A\\=F, A\\=G, A\\=H, A\\=I,"
            + "B\\=C, B\\=D, B\\=E, B\\=F, B\\=G, B\\=H, B\\=I,"
            + "C\\=D, C\\=E, C\\=F, C\\=G, C\\=H, C\\=I,"
            + "D\\=E, D\\=F, D\\=G, D\\=H, D\\=I,"
            + "E\\=F, E\\=G, E\\=H, E\\=I,"
            + "F\\=G, F\\=H, F\\=I,"
            + "G\\=H, G\\=I,"
            + "H\\=I."

            + "sudoku("
            + "A1,A2,A3,A4,A5,A6,A7,A8,A9,"
            + "B1,B2,B3,B4,B5,B6,B7,B8,B9,"
            + "C1,C2,C3,C4,C5,C6,C7,C8,C9,"
            + "D1,D2,D3,D4,D5,D6,D7,D8,D9,"
            + "E1,E2,E3,E4,E5,E6,E7,E8,E9,"
            + "F1,F2,F3,F4,F5,F6,F7,F8,F9,"
            + "G1,G2,G3,G4,G5,G6,G7,G8,G9,"
            + "H1,H2,H3,H4,H5,H6,H7,H8,H9,"
            + "I1,I2,I3,I4,I5,I6,I7,I8,I9) :-"

            + "diff(A1,A2,A3,A4,A5,A6,A7,A8,A9),"
            + "diff(B1,B2,B3,B4,B5,B6,B7,B8,B9),"
            + "diff(C1,C2,C3,C4,C5,C6,C7,C8,C9),"
            + "diff(D1,D2,D3,D4,D5,D6,D7,D8,D9),"
            + "diff(E1,E2,E3,E4,E5,E6,E7,E8,E9),"
            + "diff(F1,F2,F3,F4,F5,F6,F7,F8,F9),"
            + "diff(G1,G2,G3,G4,G5,G6,G7,G8,G9),"
            + "diff(H1,H2,H3,H4,H5,H6,H7,H8,H9),"
            + "diff(I1,I2,I3,I4,I5,I6,I7,I8,I9),"

            + "diff(A1,B1,C1,D1,E1,F1,G1,H1,I1),"
            + "diff(A2,B2,C2,D2,E2,F2,G2,H2,I2),"
            + "diff(A3,B3,C3,D3,E3,F3,G3,H3,I3),"
            + "diff(A4,B4,C4,D4,E4,F4,G4,H4,I4),"
            + "diff(A5,B5,C5,D5,E5,F5,G5,H5,I5),"
            + "diff(A6,B6,C6,D6,E6,F6,G6,H6,I6),"
            + "diff(A7,B7,C7,D7,E7,F7,G7,H7,I7),"
            + "diff(A8,B8,C8,D8,E8,F8,G8,H8,I8),"
            + "diff(A9,B9,C9,D9,E9,F9,G9,H9,I9),"

            + "diff(A1,A2,A3,B1,B2,B3,C1,C2,C3),"
            + "diff(A4,A5,A6,B4,B5,B6,C4,C5,C6),"
            + "diff(A7,A8,A9,B7,B8,B9,C7,C8,C9),"
            + "diff(D1,D2,D3,E1,E2,E3,F1,F2,F3),"
            + "diff(D4,D5,D6,E4,E5,E6,F4,F5,F6),"
            + "diff(D7,D8,D9,E7,E8,E9,F7,F8,F9),"
            + "diff(G1,G2,G3,H1,H2,H3,I1,I2,I3),"
            + "diff(G4,G5,G6,H4,H5,H6,I4,I5,I6),"
            + "diff(G7,G8,G9,H7,H8,H9,I7,I8,I9)."
    ));

    var b = engine.createBindings();
    assertTrue(
        (boolean) engine.eval("?-sudoku(7, 3, 6,  8, 4, 9,  1, 5, A,"
                + "1, B, 4,  2, 7, C,  6, 8, 9,"
                + "D, 8, 9,  1, 5, 6,  3, 4, 7,"

                + "9, 4, E,  7, 6, F,  2, G, 5,"
                + "3, 6, 1,  H, 2, 5,  4, 7, I,"
                + "5, J, 2,  4, K, 8,  9, 1, 6,"

                + "6, 2, L,  5, 8, M,  7, 9, N,"
                + "O, 9, 7,  P, 1, 2,  5, Q, 4,"
                + "4, R, 5,  6, S, 7,  8, 2, 3).",
            b
        )
    );
    var arr = IntStream.range(0, 'T' - 'A').map(x -> 'A' + x)
        .map(x -> ((Long) b.get(String.valueOf((char) x))).intValue()).toArray();
    assertArrayEquals(new int[] {2, 5, 3, 2, 8, 1, 3, 9, 8, 7, 3, 3, 4, 1, 8, 3, 6, 1, 9}, arr);
  }
}
