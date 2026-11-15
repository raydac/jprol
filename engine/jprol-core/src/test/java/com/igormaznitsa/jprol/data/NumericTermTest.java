package com.igormaznitsa.jprol.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
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

  @Test
  void testUnifyWith() {
    final TermVar var1 = Terms.newVar("A");
    final TermVar var2 = Terms.newVar("B");

    assertTrue(var1.unifyWith(var2));
    final TermLong long1 = Terms.newLong(1L);
    assertTrue(var1.unifyWith(long1));
    assertTrue(var1.isGround());
    assertTrue(long1.unifyWith(var1));
  }

  @Test
  void testLongCacheBoundariesAndOverflowProtection() {
    assertSame(Terms.newLong(-128L), Terms.newLong(-128L));
    assertSame(Terms.newLong(0L), Terms.newLong(0L));
    assertSame(Terms.newLong(1024L), Terms.newLong(1024L));
    assertNotSame(Terms.newLong(-129L), Terms.newLong(-129L));
    assertNotSame(Terms.newLong(1025L), Terms.newLong(1025L));

    final TermLong max = Terms.newLong(Long.MAX_VALUE);
    final TermLong min = Terms.newLong(Long.MIN_VALUE);
    final TermLong one = Terms.newLong(1L);
    final TermLong negOne = Terms.newLong(-1L);

    assertThrows(ProlEvaluationErrorException.class, () -> max.add(one));
    assertThrows(ProlEvaluationErrorException.class, () -> min.sub(one));
    assertThrows(ProlEvaluationErrorException.class, () -> max.mul(Terms.newLong(2L)));
    assertThrows(ProlEvaluationErrorException.class, min::neg);
    assertThrows(ProlEvaluationErrorException.class, min::abs);
    assertThrows(ProlEvaluationErrorException.class, () -> min.div(negOne));
  }

  @Test
  void testMixedLongAndDoubleArithmeticAndComparison() {
    final TermLong five = Terms.newLong(5L);
    final TermDouble fiveDouble = Terms.newDouble(5.0d);
    final TermDouble half = Terms.newDouble(0.5d);

    assertEquals(0, five.compare(fiveDouble));
    assertEquals(0, fiveDouble.compare(five));
    assertFalse(five.equals(fiveDouble));
    assertTrue(five.add(half).isDouble());
    assertEquals(5.5d, five.add(half).toNumber().doubleValue());
    assertEquals(2.5d, five.mul(half).toNumber().doubleValue());
    assertEquals(10.0d, five.div(half).toNumber().doubleValue());
    assertEquals(-1L, Terms.newDouble(-0.25d).sign().toNumber().longValue());
    assertEquals(0L, Terms.newDouble(0.0d).sign().toNumber().longValue());
    assertEquals(1L, Terms.newDouble(0.25d).sign().toNumber().longValue());
  }
}
