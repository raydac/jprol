package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

import static org.junit.jupiter.api.Assertions.*;


class CryptoTest extends AbstractProlTest {

  @Test
  void testCrypto() throws Exception {

    // check the knowledge base export data process
    //--
    final ProlContext context0 = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult0 = new ProlConsult("sum(N1,N2,N):-\n"
        + "     sum1(N1,N2,N,\n"
        + "     0,0,\n"
        + "     [0,1,2,3,4,5,6,7,8,9],_).\n"
        + "sum1([],[],[],C,C,Digits,Digits).\n"
        + "sum1([D1|N1],[D2|N2],[D|N],C1,C,Digs1, Digs):-\n"
        + "     sum1(N1,N2,N,C1,C2,Digs1,Digs2),\n"
        + "     digitsum(D1,D2,C2,D,C,Digs2,Digs).\n"
        + "\n"
        + "digitsum(D1,D2,C1,D,C,Digs1,Digs):-\n"
        + "     del_var(D1,Digs1,Digs2),\n"
        + "     del_var(D2,Digs2,Digs3),\n"
        + "     del_var(D,Digs3,Digs),\n"
        + "     S is D1+D2+C1,\n"
        + "     D is S mod 10,\n"
        + "     C is S // 10.\n"
        + "\n"
        + "del_var(A,L,L):-nonvar(A),!.\n"
        + "del_var(A,[A|L],L).\n"
        + "del_var(A,[B|L],[B|L1]):-del_var(A,L,L1).\n"
        + "\n"
        + "puzzle1([D,O,N,A,L,D],[G,E,R,A,L,D],[R,O,B,E,R,T]).\n"
        + "puzzle2([0,S,E,N,D],[0,M,O,R,E],[M,O,N,E,Y]).\n", context0);
    consult0.consult();
    ByteArrayOutputStream buffer = new ByteArrayOutputStream(1024);
    PrintWriter src = new PrintWriter(buffer);
    context0.getKnowledgeBase().write(src);
    src.flush();
    src.close();
    //--

    final ProlContext context = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult(new ByteArrayInputStream(buffer.toByteArray()), context);

    consult.consult();

    final ChoicePoint goal = new ChoicePoint("puzzle1(N1,N2,N), sum(N1,N2,N).", context);
    assertNotNull(goal.next());
    assertEquals("[7,2,3,9,7,0]", goal.getVarAsText("N"));
    assertEquals("[5,2,6,4,8,5]", goal.getVarAsText("N1"));
    assertEquals("[1,9,7,4,8,5]", goal.getVarAsText("N2"));
    assertNull(goal.next());

    final ChoicePoint goal2 = new ChoicePoint("puzzle2(N1,N2,N), sum(N1,N2,N).", context);
    int counter = 0;
    while (goal2.next() != null) {
      counter++;
    }
    assertEquals(25, counter);
  }
}
