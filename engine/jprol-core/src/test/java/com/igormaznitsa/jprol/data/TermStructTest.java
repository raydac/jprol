package com.igormaznitsa.jprol.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.io.StringReader;
import org.junit.jupiter.api.Test;

class TermStructTest extends AbstractJProlTest {

  @Test
  void testTransferPayload() {
    try (final JProlContext context = this.makeTestContext()) {
      context.consult(new StringReader(
          "c(A,Y) :- s(_,_,Y) = A. b([X],Y) :- Z = [X], [L] = Z, c(s(1,2,L),Y).  a(X,Y) :- b([X],Z), Z = Y."));
      final Object payload = new Object();
      final Term valX = Terms.newAtom("ABC", payload);
      final TermVar result = Terms.newVar("Y");
      final TermStruct goal = Terms.newStruct(Terms.newAtom("a"), valX, result);

      final JProlChoicePoint choicePoint = context.makeChoicePoint(goal);

      assertNotNull(choicePoint.prove());

      assertEquals("ABC", result.tryGround().getText());
      assertEquals(payload, result.tryGround().getPayload());
    }
  }
}