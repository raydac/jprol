package com.igormaznitsa.jprol.it;

import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import org.junit.jupiter.api.Test;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

import static org.junit.jupiter.api.Assertions.*;

class HanoiTowersTest extends AbstractJProlTest {

  @Test
  void testHanoiTowers() {
    final StringWriter data = new StringWriter();

    final IoResourceProvider testIoProvider = new IoResourceProvider() {
      @Override
      public Writer findWriter(JProlContext context, String writerId, boolean append) {
        if ("+hanoi".equals(writerId)) {
          return data;
        }
        return null;
      }
    };


    final JProlContext context = makeTestContext(testIoProvider);
    context.consult(new StringReader("move(1,X,Y,_):-write('Move top disk from '),write(X),write(' to '),write(Y),nl." +
        "move(N,X,Y,Z):-N>1,M is N-1,move(M,X,Z,Y),move(1,X,Y,_),move(M,Z,Y,X)."));
    final JProlChoicePoint goal = new JProlChoicePoint("tell(\'+hanoi\'),move(3,left,right,center).", context);
    assertNotNull(goal.prove());
    assertNull(goal.prove());

    assertEquals("Move top disk from left to right\n" +
        "Move top disk from left to center\n" +
        "Move top disk from right to center\n" +
        "Move top disk from left to right\n" +
        "Move top disk from center to left\n" +
        "Move top disk from center to right\n" +
        "Move top disk from left to right\n", data.toString());
  }
}
