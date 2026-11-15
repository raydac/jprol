package com.igormaznitsa.jprol.data;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class TermListTest {

  @Test
  void testUnifyWith() {
    final TermVar var1 = Terms.newVar("A");
    final TermVar var2 = Terms.newVar("B");

    assertTrue(var1.unifyWith(var2));
    final TermList empty = Terms.newList();
    assertTrue(var1.unifyWith(empty));
    assertTrue(var1.isGround());
    assertTrue(empty.unifyWith(var1));
  }

  @Test
  void testPartialListUnificationAndLengthAndReverse() {
    final TermVar tail = Terms.newVar("Tail");
    final TermList open = Terms.newList(
        Terms.newLong(1L),
        Terms.newList(Terms.newLong(2L), tail)
    );
    assertFalse(open.isGround());
    assertTrue(open.isUnground());
    assertTrue(open.containsNamedVariable("Tail"));
    assertFalse(open.containsNamedVariable("Missing"));

    final TermList closed = TermList.listOf(
        Terms.newLong(1L),
        Terms.newLong(2L),
        Terms.newLong(3L),
        Terms.newLong(4L)
    );
    assertTrue(open.isUnifiableWith(closed));
    assertFalse(open.isGround());

    assertTrue(open.unifyWith(closed));
    assertTrue(open.isGround());
    assertEquals("[3,4]", tail.toSrcString());
    assertEquals(4, open.calculateLength());
    assertEquals("[4,3,2,1]", closed.reverse().toSrcString());
    assertEquals("[]", NULL_LIST.reverse().toSrcString());
  }

  @Test
  void testImproperDottedPairUnificationAndFormatting() {
    final TermList pairLeft = Terms.newList(Terms.newAtom("head"), Terms.newVar("Rest"));
    final TermList pairRight = Terms.newList(Terms.newVar("H"), Terms.newAtom("tail_atom"));
    assertTrue(pairLeft.unifyWith(pairRight));
    assertEquals("['head'|'tail_atom']", pairLeft.toSrcString());
    assertEquals("['head'|'tail_atom']", pairRight.toSrcString());
  }
}
