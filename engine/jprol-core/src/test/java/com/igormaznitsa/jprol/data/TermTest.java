package com.igormaznitsa.jprol.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import org.junit.jupiter.api.Test;

class TermTest {

  @Test
  void testEqualsContract_Term() {
    final Term one1 = Terms.newAtom("one");
    final Term one2 = Terms.newAtom("one");
    final Term two = Terms.newAtom("two");
    assertEquals(one1, one2);
    assertEquals(one2, one1);
    assertNotEquals(one1, two);
    assertNotEquals(two, one1);
  }

  @Test
  void testEqualsContract_TermOperator() {
    final Term one1 = Terms.newAtom("one");
    final TermOperator op1 = new TermOperator(1000, OpAssoc.FX, "one", SourcePosition.UNKNOWN);
    assertEquals(op1, one1);
    assertEquals(one1, op1);
  }
}