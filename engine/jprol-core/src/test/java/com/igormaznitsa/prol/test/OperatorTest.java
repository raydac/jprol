package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.DeferredGoal;
import com.igormaznitsa.prol.logic.ProlConsult;
import com.igormaznitsa.prol.logic.ProlContext;
import org.junit.jupiter.api.Test;

import java.io.StringReader;

import static com.igormaznitsa.prol.data.TermType.ATOM;
import static org.junit.jupiter.api.Assertions.assertEquals;

class OperatorTest extends AbstractProlTest {

  @Test
  void testOperatorDefs() throws Exception {
    final ProlContext context = new ProlContext("Operator_test");
    final ProlConsult consult = new ProlConsult(new StringReader(":-op(800,xfx,'<===>'). :-op(700,xfy,'v'). :-op(600,xfy,'&'). :-op(500,fy,'~'). moon <===> earth."), context);
    consult.consult();

    final ChoicePoint prepGoal = new ChoicePoint("~(xxx & yyy) <===> ~xxx v ~yyy.", context);
    TermStruct root = (TermStruct) prepGoal.getGoalTerm();

    assertEquals("<===>", root.getText());
    assertEquals(2, root.getArity());

    TermStruct lefttree = root.getElement(0);
    TermStruct righttree = root.getElement(1);

    assertEquals("~", lefttree.getText());
    assertEquals(1, lefttree.getArity());
    assertEquals("v", righttree.getText());
    assertEquals(2, righttree.getArity());

    final TermStruct prevrighttree = righttree;

    lefttree = (TermStruct) lefttree.getElement(0);
    righttree = (TermStruct) righttree.getElement(0);

    assertEquals("&", lefttree.getText());
    assertEquals(2, lefttree.getArity());
    assertEquals(ATOM, lefttree.getElement(0).getTermType());
    assertEquals(ATOM, lefttree.getElement(1).getTermType());
    assertEquals("xxx", lefttree.getElement(0).getText());
    assertEquals("yyy", lefttree.getElement(1).getText());

    lefttree = prevrighttree.getElement(0);
    righttree = prevrighttree.getElement(1);

    assertEquals(1, lefttree.getArity(), 1);
    assertEquals("~", lefttree.getText(), "~");
    assertEquals(1, righttree.getArity(), 1);
    assertEquals("~", righttree.getText(), "~");
    assertEquals(ATOM, lefttree.getElement(0).getTermType());
    assertEquals("xxx", lefttree.getElement(0).getText());
    assertEquals(ATOM, righttree.getElement(0).getTermType());
    assertEquals("yyy", righttree.getElement(0).getText());

    final DeferredGoal prepGoal2 = new DeferredGoal("moon <===> X.", context);
    int solvecounter = 0;
    final ChoicePoint gl = prepGoal2.getNonparametrizedGoalInstance();
    String text = null;
    Term curresult;
    while ((curresult = gl.next()) != null) {
      text = gl.getVarForName("X").getValue().getText();
      solvecounter++;
    }

    assertEquals(1, solvecounter);
    assertEquals("earth", text);
  }
}
