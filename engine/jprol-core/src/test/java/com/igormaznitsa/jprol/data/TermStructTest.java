package com.igormaznitsa.jprol.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.io.StringReader;
import org.junit.jupiter.api.Test;

class TermStructTest extends AbstractJProlTest {

  @Test
  void testTermContextPayload() {
    try (final JProlContext context = this.makeTestContext()) {
      context.consult(new StringReader(
          "c(A,Y) :- s(_,_,Y) = A. b([X],Y) :- Z = [X], [L] = Z, c(s(1,2,L),Y).  a(X,Y) :- b([X],Z), Z = Y."));
      final Object payload = new Object();
      final Term valX = Terms.newAtom("ABC");
      context.setPayload(valX, payload);
      final TermVar result = Terms.newVar("Y");
      final TermStruct goal = Terms.newStruct(Terms.newAtom("a"), valX, result);

      final JProlChoicePoint choicePoint = context.makeChoicePoint(goal);

      assertNotNull(choicePoint.prove());

      assertEquals("ABC", result.tryGround().getText());
      assertEquals(payload, result.tryGround().findPayload(context).orElseThrow());
    }
  }

  @Test
  void testOperatorSourceFormattingRespectsPrecedenceThroughBoundVariables() {
    assertEquals(
        "(1 + 2) * 4",
        this.proveSingle("", "A = 1 + 2, T = A * 4.", "T")
    );
    assertEquals(
        "4 * (2 * 3)",
        this.proveSingle("", "B = 2 * 3, T = 4 * B.", "T")
    );
    assertEquals(
        "1 - (2 - 3)",
        this.proveSingle("", "B = 2 - 3, T = 1 - B.", "T")
    );
    assertEquals(
        "1 - 2 - 3",
        this.proveSingle("", "A = 1 - 2, T = A - 3.", "T")
    );
  }

  @Test
  void testDefensiveCopyOfArgumentsArray() {
    final TermStruct struct = Terms.newStruct(
        Terms.newAtom("pair"),
        Terms.newLong(1L),
        Terms.newLong(2L)
    );
    final Term[] argsCopy = struct.getArguments();
    assertNotSame(argsCopy, struct.getArguments());
    argsCopy[0] = Terms.newLong(999L);
    assertEquals("pair(1,2)", struct.toSrcString());
  }
}
