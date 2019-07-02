package com.igormaznitsa.jprol.test;

import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.ProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import org.junit.jupiter.api.Test;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

import static org.junit.jupiter.api.Assertions.*;

class HanoiTowersTest extends AbstractProlTest {

  @Test
  void testHanoiTowers() {
    final StringWriter data = new StringWriter();

    final ProlContext context = makeTestContext().addIoResourceProvider(new IoResourceProvider() {
      @Override
      public Writer findWriter(ProlContext context, String writerId, boolean append) {
        if ("+hanoi".equals(writerId)) {
          return data;
        }
        return null;
      }
    });
    context.consult(new StringReader("move(1,X,Y,_):-write('Move top disk from '),write(X),write(' to '),write(Y),nl." +
        "move(N,X,Y,Z):-N>1,M is N-1,move(M,X,Z,Y),move(1,X,Y,_),move(M,Z,Y,X)."));
    final ChoicePoint goal = new ChoicePoint("tell(\'+hanoi\'),move(3,left,right,center).", context);
    assertNotNull(goal.next());
    assertNull(goal.next());

    assertEquals("Move top disk from left to right\n" +
        "Move top disk from left to center\n" +
        "Move top disk from right to center\n" +
        "Move top disk from left to right\n" +
        "Move top disk from center to left\n" +
        "Move top disk from center to right\n" +
        "Move top disk from left to right\n", data.toString());
  }
}
