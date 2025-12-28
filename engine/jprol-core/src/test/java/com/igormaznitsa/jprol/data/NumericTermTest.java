package com.igormaznitsa.jprol.data;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class NumericTermTest {

  @Test
  void testUnifiableWith() {
    final TermVar var1 = Terms.newVar("A");
    final TermVar var2 = Terms.newVar("B");
    assertTrue(var1.unifyWith(var2));
    final TermLong long1 = Terms.newLong(1L);
    assertTrue(var1.isUnifiableWith(long1));
    assertTrue(var1.isUnground());
    assertTrue(long1.isUnifiableWith(var1));
  }
}