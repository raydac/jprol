package com.igormaznitsa.jprol.jsr223;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;
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
  public void testSimpleHelloWorld() throws Exception {
    assertTrue((Boolean) findScriptEngine().eval("?-writeln('Hello world')."));
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
    final int threads = 1000;
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
        } catch (Exception ex) {
          errorCounter.incrementAndGet();
          System.err.println("Error: " + ex.getMessage());
        } finally {
          end.countDown();
        }
      }, "thread-test-" + i);
      thread.start();
      start.countDown();
    }
    end.await();
    assertEquals(0, errorCounter.get());
  }
}
