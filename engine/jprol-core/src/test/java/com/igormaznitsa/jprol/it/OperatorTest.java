package com.igormaznitsa.jprol.it;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import com.igormaznitsa.jprol.logic.PreparedGoal;
import com.igormaznitsa.jprol.logic.UndefinedPredicateBehavior;
import com.igormaznitsa.jprol.trace.JProlContextListener;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class OperatorTest extends AbstractJProlTest {

  @ParameterizedTest
  @ValueSource(strings = {"FAIL", "ERROR", "WARNING"})
  void testCustomOperatorUserForMiscUnknownPredicateStrategies(
      final String unknownPredicateStrategy) throws Exception {
    final UndefinedPredicateBehavior strategy =
        UndefinedPredicateBehavior.valueOf(unknownPredicateStrategy);

    final JProlContext context = makeContextAndConsult(
        ctx -> {
          ctx.addContextListener(new JProlContextListener() {
            @Override
            public void onUndefinedPredicateWarning(JProlContext source,
                                                    JProlChoicePoint choicePoint,
                                                    String undefinedPredicateSignature) {
              if (source.getUndefinedPredicateBehavior() != UndefinedPredicateBehavior.FAIL) {
                throw new Error("Undefined predicate: " + choicePoint.getGoalTerm());
              }
            }
          });
          ctx.setSystemFlag(JProlSystemFlag.UNKNOWN, strategy.getTerm());
        },
        ":-op(800,xfx,'<===>'). :-op(700,xfy,'v'). :-op(600,xfy,'&'). :-op(500,fy,'~').  :-op(900,yfx,'::'). moon <===> earth."
    );

    final JProlChoicePoint prepGoal =
        context.makeChoicePoint("~(xxx & yyy) <===> ~xxx v ~yyy.");
    TermStruct root = (TermStruct) prepGoal.getGoalTerm();

    assertEquals("<===>", root.getText());
    assertEquals(2, root.getArity());

    TermStruct lefttree = root.getArgumentAt(0);
    TermStruct righttree = root.getArgumentAt(1);

    assertEquals("~", lefttree.getText());
    assertEquals(1, lefttree.getArity());
    assertEquals("v", righttree.getText());
    assertEquals(2, righttree.getArity());

    final TermStruct prevrighttree = righttree;

    lefttree = lefttree.getArgumentAt(0);
    righttree = righttree.getArgumentAt(0);

    assertEquals("&", lefttree.getText());
    assertEquals(2, lefttree.getArity());
    assertEquals(ATOM, lefttree.getArgumentAt(0).getTermType());
    assertEquals(ATOM, lefttree.getArgumentAt(1).getTermType());
    assertEquals("xxx", lefttree.getArgumentAt(0).getText());
    assertEquals("yyy", lefttree.getArgumentAt(1).getText());

    lefttree = prevrighttree.getArgumentAt(0);
    righttree = prevrighttree.getArgumentAt(1);

    assertEquals(1, lefttree.getArity(), 1);
    assertEquals("~", lefttree.getText(), "~");
    assertEquals(1, righttree.getArity(), 1);
    assertEquals("~", righttree.getText(), "~");
    assertEquals(ATOM, lefttree.getArgumentAt(0).getTermType());
    assertEquals("xxx", lefttree.getArgumentAt(0).getText());
    assertEquals(ATOM, righttree.getArgumentAt(0).getTermType());
    assertEquals("yyy", righttree.getArgumentAt(0).getText());

    final PreparedGoal prepGoal2 = new PreparedGoal("moon <===> X.", context);
    int solvecounter = 0;
    final JProlChoicePoint gl =
        context.makeChoicePoint(prepGoal2.getPreparedGoalTerm().makeClone());
    String text = null;
    Term curresult;
    while ((curresult = gl.prove()) != null) {
      text = gl.findVar("X").get().getValue().getText();
      solvecounter++;
    }
    assertEquals(1, solvecounter);
    assertEquals("earth", text);

    final PreparedGoal prepGoal3 = new PreparedGoal("hello::world.", context);
    assertNull(context.makeChoicePoint(prepGoal3.getPreparedGoalTerm().makeClone()).prove(),
        "Use of operator without any definition in knowledge base must not be recognized as error, as fail only");
  }
}
