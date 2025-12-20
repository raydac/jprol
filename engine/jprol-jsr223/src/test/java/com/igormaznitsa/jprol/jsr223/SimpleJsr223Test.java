package com.igormaznitsa.jprol.jsr223;


import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.Terms;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
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
    var manager = new ScriptEngineManager();
    var engine = manager.getEngineByName("jprol.prolog");
    assertNotNull(engine);
    return engine;
  }

  @Test
  void testDisableCriticalPredicateClause() throws Exception {
    final ScriptEngine engine = findScriptEngine();
    engine.getBindings(ScriptContext.GLOBAL_SCOPE).put(
        JProlScriptEngine.JPROL_GLOBAL_CRITICAL_PREDICATE_GUARD,
        (JProlCriticalPredicateGuard) (sourceLibrary, choicePoint, predicateIndicator) -> !"clause/2".equals(
            predicateIndicator));

    assertThrowsExactly(ScriptException.class, () ->
        engine.eval("?-clause(a(X),(X = 10))."));

    final ScriptEngine engineWithoutRestriction = findScriptEngine();
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

}
