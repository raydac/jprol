package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.io.ProlMemoryPipe;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import java.nio.charset.Charset;
import junit.framework.TestCase;
import org.junit.Test;

public class HanoiTowers extends TestCase {

  @Test
  public void testHanoiTowers() {
    try {
      final ProlContext context = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
      final ProlConsult consult = new ProlConsult("move(1,X,Y,_):-write('Move top disk from '),write(X),write(' to '),write(Y),nl.move(N,X,Y,Z):-N>1,M is N-1,move(M,X,Z,Y),move(1,X,Y,_),move(M,Z,Y,X).", context);
      consult.consult();

      final Goal goal = new Goal("tell(\'+hanoi\'),move(3,left,right,center).", context);
      if (goal.solve() != null && goal.solve() == null) {
        assertTrue(true);
      }
      else {
        fail();
      }

      final ProlMemoryPipe pipe = context.getMemoryPipeForName("+hanoi");
      pipe.closeForWriteOnly();
      final String data = pipe.getAllDataAsString(Charset.defaultCharset());

      assertEquals(data, "Move top disk from left to right\nMove top disk from left to center\nMove top disk from right to center\nMove top disk from left to right\nMove top disk from center to left\nMove top disk from center to right\nMove top disk from left to right\n");

    }
    catch (Throwable thr) {
      thr.printStackTrace();
      fail("Exception during operation");
    }
  }
}
