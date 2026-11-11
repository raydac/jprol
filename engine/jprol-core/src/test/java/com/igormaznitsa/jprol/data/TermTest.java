package com.igormaznitsa.jprol.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import org.junit.jupiter.api.Test;

class TermTest {

  @Test
  void testEqualsContract_Term() {
    final Term one1 = Terms.newAtom("one");
    final Term one2 = Terms.newAtom("one");
    final Term two = Terms.newAtom("two");
    assertEquals(one1.hashCode(), one2.hashCode());
    assertEquals(one1, one2);
    assertEquals(one2, one1);
    assertNotEquals(one1, two);
    assertNotEquals(two, one1);
  }

  @Test
  void testEqualsContract_TermOperator() {
    final Term term = Terms.newAtom("one");
    final TermOperator operator = new TermOperator(1000, OpAssoc.FX, "one", SourcePosition.UNKNOWN);
    assertEquals(term.hashCode(), operator.hashCode());
    assertEquals(operator, term);
    assertEquals(term, operator);
    assertTrue(operator.isUnifiableWith(term));
    assertTrue(operator.unifyWith(term));
    assertTrue(term.unifyWith(operator));
    assertTrue(term.isUnifiableWith(operator));
  }

  @Test
  void testEqualsContract_TermOperatorContainer() {
    final TermOperator operator =
        new TermOperator(1000, OpAssoc.FX, "one", SourcePosition.UNKNOWN);
    final TermOperatorContainer first =
        TermOperatorContainer.makeFor(operator, SourcePosition.UNKNOWN);
    final TermOperatorContainer second =
        TermOperatorContainer.makeFor(
            new TermOperator(1000, OpAssoc.FX, "one", SourcePosition.UNKNOWN),
            SourcePosition.UNKNOWN);
    assertEquals(first, second);
    assertEquals(first.hashCode(), second.hashCode());
    assertNotEquals(first, Terms.newAtom("one"));
    assertNotEquals(Terms.newAtom("one"), first);
  }

  @Test
  void testVariableChainWalksPastTheFirstLink() {
    final TermVar head = Terms.newVar("Head");
    final TermVar middle = Terms.newVar("Middle");
    final TermVar tail = Terms.newVar("Tail");
    head.setImmediateValue(middle);
    middle.setImmediateValue(tail);

    assertTrue(head.isPresentedInVarChain(tail));
    assertTrue(head.isPresentedInVarChain(Terms.newVar("Tail")));
    assertFalse(head.isPresentedInVarChain(Terms.newVar("Other")));
  }
}