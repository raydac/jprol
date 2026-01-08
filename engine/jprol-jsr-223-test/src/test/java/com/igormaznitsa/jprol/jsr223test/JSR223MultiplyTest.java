package com.igormaznitsa.jprol.jsr223test;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import org.junit.jupiter.api.Test;

class JSR223MultiplyTest {

  @Test
  void testMultiply() throws Exception {
    final JSR223Multiply instance = new JSR223Multiply();
    assertEquals(45L, instance.mul(5, 9));
    assertEquals(50L, instance.mul(5, 10));
    assertEquals(-16L, instance.mul(-2, 8));
  }

  @Test
  void testMultiplyMultithread() throws Exception {
    final JSR223Multiply instance = new JSR223Multiply();
    final ExecutorService executor = Executors.newFixedThreadPool(5);
    final List<Future<Long>> futures = new ArrayList<>();

    final int threads = 1000_000;

    for (int i = 0; i < threads; i++) {
      final long value = i;
      futures.add(executor.submit(() -> instance.mul(value, 11)));
    }
    for (int i = 0; i < threads; i++) {
      final Long result = futures.get(i).get();
      assertEquals((long) i * 11L, result);
    }
    System.out.println("Internally buffered resources: " + instance.internalSize());
  }

}