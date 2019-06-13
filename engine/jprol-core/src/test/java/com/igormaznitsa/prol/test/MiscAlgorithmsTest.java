package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class MiscAlgorithmsTest extends AbstractProlTest {

  private void calcAkkerman(int m, int n, int a) throws Exception {
    final ProlContext context = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult("akkerman(0,N,X):- X is N+1,!. akkerman(M,0,X):- Mn is M-1, !, akkerman(Mn,1,X). akkerman(M,N,X):- Mn is M-1, Nn is N-1, !, akkerman(M,Nn,Y), !, akkerman(Mn,Y,X).", context);
    consult.consult();
    final String goalText = "akkerman(" + m + ',' + n + ",A).";
    final ChoicePoint goal = new ChoicePoint(goalText, context);
    final Term resultterm = goal.next();
    assertNotNull(resultterm);
    final Term result = goal.getVarForName("A").getValue();
    assertNotNull(result);
    assertEquals(a, result.toNumber().intValue());
    assertNull(goal.next());
  }

  @Test
  void testAkkerman() throws Exception {
    calcAkkerman(0, 0, 1);
    calcAkkerman(1, 0, 2);
    calcAkkerman(2, 0, 3);
    calcAkkerman(3, 0, 5);
    calcAkkerman(4, 0, 13);

    calcAkkerman(0, 1, 2);
    calcAkkerman(1, 1, 3);
    calcAkkerman(2, 1, 5);
    calcAkkerman(3, 1, 13);

    calcAkkerman(0, 5, 6);
    calcAkkerman(1, 5, 7);
    calcAkkerman(2, 5, 13);
    calcAkkerman(3, 5, 253);

    //calcAkkerman(4, 1, 65533); // too deep stack
  }

  @Test
  void testFibbonachi() throws Exception {
    final ProlContext context = makeContext("fib(1,1):-!. fib(0,0):-!. fib(N,Result):-Npp is N-2, Np is N-1, fib(Npp,Resultpp), fib(Np,Resultp), Result is Resultpp+Resultp.");

    final ChoicePoint goal = new ChoicePoint("fib(22,Result).", context);

    assertNotNull(goal.next());
    assertEquals(17711L, goal.getVarAsNumber("Result").intValue());
    assertNull(goal.next());
  }

  @Test
  void testObject() throws Exception {
    final ProlContext context = makeContext("object(rectangle(Length,Width),[(area(A):-A is Length * Width),(describe :- write('Rectangle of size'), write(Length * Width))]).send(Object,Message):-get_methods(Object,Methods),process(Message,Methods)."
        + "get_methods(Object,Methods):-object(Object,Methods)."
        + "process(Message,[Message|_])."
        + "process(Message,[(Message :- Body)|_]):-call(Body)."
        + "process(Message,[_|Methods]):-process(Message,Methods).");

    final ChoicePoint goal = new ChoicePoint("Rec1=rectangle(4,3),send(Rec1,area(Area)).", context);

    assertNotNull(goal.next());
    assertEquals(12L, goal.getVarAsNumber("Area").longValue());
    assertNull(goal.next());
  }

  private ProlContext makeContext(final String knowledgeBase) throws Exception {
    final ProlContext context = new ProlContext("PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult(knowledgeBase, context);
    consult.consult();
    return context;
  }
}
