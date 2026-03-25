package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

class MiscAlgorithmsTest extends AbstractJProlTest {

  private void calcAckermann(int n, int m, int a) {
    try (final JProlContext context = makeContextAndConsult(
        "ackermann(0, M, R) :- R is M + 1."
            + "ackermann(N, 0, R) :- N > 0, N1 is N - 1, ackermann(N1, 1, R)."
            +
            "ackermann(N, M, R) :- N > 0, M > 0, M1 is M - 1, ackermann(N, M1, R1), N1 is N - 1, ackermann(N1, R1, R)."
    )) {
      final String goalText = "ackermann(" + n + ',' + m + ",A).";
      final JProlChoicePoint goal = context.makeChoicePoint(goalText);
      final Term resultterm = goal.prove();
      assertNotNull(resultterm);
      final Term result = goal.findVar("A").get().getValue();
      assertNotNull(result);
      assertEquals(a, result.toNumber().intValue());
      assertNull(goal.prove());
    }
  }

  @Test
  void testAckerman() {
    calcAckermann(0, 0, 1);
    calcAckermann(0, 1, 2);
    calcAckermann(0, 2, 3);
    calcAckermann(0, 3, 4);
    calcAckermann(0, 4, 5);
    calcAckermann(1, 0, 2);
    calcAckermann(1, 1, 3);
    calcAckermann(1, 2, 4);
    calcAckermann(1, 3, 5);
    calcAckermann(1, 4, 6);
    calcAckermann(2, 0, 3);
    calcAckermann(2, 1, 5);
    calcAckermann(2, 2, 7);
    calcAckermann(2, 3, 9);
    calcAckermann(3, 0, 5);
    calcAckermann(3, 5, 253);
    calcAckermann(3, 6, 509);

    //calcAckermann(4, 0, 13);
    //calcAckermann(3, 9, 4093);
    //calcAckermann(3, 10, 8189);
    //calcAckermann(3, 16, 524285);
  }

  @Test
  void testFibonacci() {
    try (final JProlContext context = makeContextAndConsult(
        "fib(1,1):-!. fib(0,0):-!. fib(N,Result):-Npp is N-2, Np is N-1, fib(Npp,Resultpp), fib(Np,Resultp), Result is Resultpp+Resultp.")) {
      final JProlChoicePoint goal = context.makeChoicePoint("fib(22,Result).");

      assertNotNull(goal.prove());
      assertEquals(17711L, getVarAsNumber(goal, "Result").intValue());
      assertNull(goal.prove());
    }
  }

  @Test
  void testQuasiObjectMethods() {
    try (final JProlContext context = makeContextAndConsult(
        "object(rectangle(Length,Width),[(area(A):-A is Length * Width),(describe :- write('Rectangle of size'), write(Length * Width))])."
        + "send(Object,Message):-get_methods(Object,Methods),process(Message,Methods)."
        + "get_methods(Object,Methods):-object(Object,Methods)."
        + "process(Message,[Message|_])."
        + "process(Message,[(Message :- Body)|_]):-call(Body)."
            + "process(Message,[_|Methods]):-process(Message,Methods).")) {

      final JProlChoicePoint goal =
          context.makeChoicePoint("Rec1=rectangle(4,3),send(Rec1,area(Area)).");

      assertNotNull(goal.prove());
      assertEquals(12L, getVarAsNumber(goal, "Area").longValue());
      assertNull(goal.prove());
    }
  }
}
