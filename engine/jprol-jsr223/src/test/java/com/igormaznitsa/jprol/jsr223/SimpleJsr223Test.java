package com.igormaznitsa.jprol.jsr223;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.Terms;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import org.junit.jupiter.api.Test;

public class SimpleJsr223Test {

  private ScriptEngine findScriptEngine() {
    var manager = new ScriptEngineManager();
    var engine = manager.getEngineByName("jprol");
    assertNotNull(engine);
    return engine;
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
    for (int i = 0; i < threads; i++) {
      final long arg = i;
      final long expected = i * i;
      final Thread thread = new Thread(() -> {
        try {
          start.await();
          var bindings = engine.getBindings(ScriptContext.ENGINE_SCOPE);
          assertTrue((boolean) engine.eval("square(X,Y):-Y is X * X."));
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
