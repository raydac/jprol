package com.igormaznitsa.jprol.test;

import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PuzzleTest extends AbstractProlTest {

  @Test
  void testPuzzle() throws Exception {
    final JProlContext context = makeContextAndConsult("sum(N1,N2,N):-sum1(N1,N2,N,0,0,[0,1,2,3,4,5,6,7,8,9],_)."
        + "sum1([],[],[],C,C,Digits,Digits)."
        + "sum1([D1|N1],[D2|N2],[D|N],C1,C,Digs1,Digs):-sum1(N1,N2,N,C1,C2,Digs1,Digs2),digitsum(D1,D2,C2,D,C,Digs2,Digs)."
        + "digitsum(D1,D2,C1,D,C,Digs1,Digs):-del_var(D1,Digs1,Digs2),del_var(D2,Digs2,Digs3),del_var(D,Digs3,Digs),S is D1+D2+C1,D is S mod 10,C is S//10."
        + "del_var(A,L,L):-nonvar(A),!."
        + "del_var(A,[A|L],L)."
        + "del_var(A,[B|L],[B|L1]):-del_var(A,L,L1)."
        + "puzzle1([D,O,N,A,L,D],[G,E,R,A,L,D],[R,O,B,E,R,T])."
        + "puzzle2([0,S,E,N,D],[0,M,O,R,E],[M,O,N,E,Y]).");
    final ChoicePoint goal = new ChoicePoint("puzzle1(X,Y,Z),sum(X,Y,Z).", context);

    final String[] puzzle1solXYZ = new String[] {"[5,2,6,4,8,5]", "[1,9,7,4,8,5]", "[7,2,3,9,7,0]"};

    final String[] puzzle2solZ = new String[] {"[0,8,3,5,6]", "[0,6,3,7,8]", "[0,4,2,8,9]", "[0,7,5,8,9]", "[0,9,3,4,6]", "[0,9,4,5,7]", "[0,4,1,7,9]", "[0,6,3,7,9]", "[0,8,4,6,9]", "[0,7,5,8,1]", "[0,9,2,3,7]", "[0,7,2,5,9]", "[0,8,3,5,9]", "[0,7,1,4,9]", "[0,8,1,3,9]", "[1,0,6,5,2]", "[0,3,1,8,5]", "[0,7,1,4,3]", "[0,8,2,4,3]", "[0,8,3,5,4]", "[0,8,4,6,5]", "[0,4,1,7,6]", "[0,3,1,8,7]", "[0,4,2,8,7]", "[0,6,4,8,7]"
    };

    assertNotNull(goal.next());
    assertEquals(goal.getVarAsText("X"), puzzle1solXYZ[0]);
    assertEquals(goal.getVarAsText("Y"), puzzle1solXYZ[1]);
    assertEquals(goal.getVarAsText("Z"), puzzle1solXYZ[2]);
    assertNull(goal.next());
    final ChoicePoint goal2 = new ChoicePoint("puzzle2(X,Y,Z),sum(X,Y,Z).", context);

    for (final String aPuzzle2solZ : puzzle2solZ) {
      assertNotNull(goal2.next());
      assertEquals(aPuzzle2solZ, goal2.getVarAsText("Z"));
    }

    assertNull(goal2.next());
  }
}
