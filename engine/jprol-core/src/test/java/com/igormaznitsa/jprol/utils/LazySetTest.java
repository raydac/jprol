package com.igormaznitsa.jprol.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class LazySetTest {

  @Test
  void testEmptyArrayOfCustomType() {
    final LazySet<Long> longSet = new LazySet<>();
    assertEquals(0, longSet.toArray().length);
    assertEquals(0, longSet.toArray(Long[]::new).length);
    longSet.add(777L);
    assertEquals(1, longSet.toArray().length);
    assertEquals(1, longSet.toArray(Long[]::new).length);
  }
}