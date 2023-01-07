package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

class MiscAlgorithmsTest extends AbstractJProlTest {

  private void calcAkkerman(int m, int n, int a) {
    final JProlContext context = makeContextAndConsult("akkerman(0,N,X):- X is N+1,!. "
        + "akkerman(M,0,X):- Mn is M-1, !, akkerman(Mn,1,X). "
        + "akkerman(M,N,X):- Mn is M-1, Nn is N-1, !, akkerman(M,Nn,Y), !, akkerman(Mn,Y,X).");
    final String goalText = "akkerman(" + m + ',' + n + ",A).";
    final JProlChoicePoint goal = new JProlChoicePoint(goalText, context);
    final Term resultterm = goal.prove();
    assertNotNull(resultterm);
    final Term result = goal.findVar("A").get().getValue();
    assertNotNull(result);
    assertEquals(a, result.toNumber().intValue());
    assertNull(goal.prove());
  }

  @Test
  void testAkkerman() {
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
  void testFibonacci() {
    final JProlContext context = makeContextAndConsult("fib(1,1):-!. fib(0,0):-!. fib(N,Result):-Npp is N-2, Np is N-1, fib(Npp,Resultpp), fib(Np,Resultp), Result is Resultpp+Resultp.");

    final JProlChoicePoint goal = new JProlChoicePoint("fib(22,Result).", context);

    assertNotNull(goal.prove());
    assertEquals(17711L, getVarAsNumber(goal, "Result").intValue());
    assertNull(goal.prove());
  }

  @Test
  void testQuasiObjectMethods() {
    final JProlContext context = makeContextAndConsult("object(rectangle(Length,Width),[(area(A):-A is Length * Width),(describe :- write('Rectangle of size'), write(Length * Width))])."
        + "send(Object,Message):-get_methods(Object,Methods),process(Message,Methods)."
        + "get_methods(Object,Methods):-object(Object,Methods)."
        + "process(Message,[Message|_])."
        + "process(Message,[(Message :- Body)|_]):-call(Body)."
        + "process(Message,[_|Methods]):-process(Message,Methods).");

    final JProlChoicePoint goal = new JProlChoicePoint("Rec1=rectangle(4,3),send(Rec1,area(Area)).", context);

    assertNotNull(goal.prove());
    assertEquals(12L, getVarAsNumber(goal, "Area").longValue());
    assertNull(goal.prove());
  }
}
