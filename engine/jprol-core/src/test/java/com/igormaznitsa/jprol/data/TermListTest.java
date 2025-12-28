package com.igormaznitsa.jprol.data;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class TermListTest {

  @Test
  void testUnifyWith() {
    final TermVar var1 = Terms.newVar("A");
    final TermVar var2 = Terms.newVar("B");

    assertTrue(var1.unifyWith(var2));
    final TermList long1 = Terms.newList();
    assertTrue(var1.unifyWith(long1));
    assertTrue(var1.isGround());
    assertTrue(long1.unifyWith(var1));
  }


}