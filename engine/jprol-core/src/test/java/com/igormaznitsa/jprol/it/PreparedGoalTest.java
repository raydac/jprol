package com.igormaznitsa.jprol.it;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.PreparedGoal;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PreparedGoalTest extends AbstractJProlTest {

  @Test
  void testPreparedGoal() throws Exception {
    final JProlContext context = makeContextAndConsult("test(A,B,C,D,E):-E is A+B/C*D. randomval(A):-rnd([1,2,3,4,5],A).");

    final PreparedGoal goal = new PreparedGoal("test({?},{?},{?},{?},Result).", context);

    for (long a = 1; a < 100; a += 4) {
      for (long b = 1; b < 60; b += 3) {
        for (long c = 2; c < 8; c++) {
          for (long d = 1; d < 5; d++) {
            final long precalculatedResult = a + b / c * d;

            final JProlChoicePoint workGoal = goal.makeChoicePoint(context, a, b, c, d);
            assertNotNull(workGoal.prove());

            assertEquals(precalculatedResult, getVarAsNumber(workGoal, "Result").longValue());
          }
        }
      }
    }

    final PreparedGoal oncegoal = new PreparedGoal("randomval(Result).", context);

    for (int li = 0; li < 1000; li++) {
      final Term result = oncegoal.once(context);
      final int goalResult = result.variables().filter(x -> "Result".equals(x.getText())).findFirst().orElse(null).getValue().toNumber().intValue();
      if (goalResult < 1 || goalResult > 5) {
        fail("Wrong result, returned value is outbound");
      }
    }
  }
}
