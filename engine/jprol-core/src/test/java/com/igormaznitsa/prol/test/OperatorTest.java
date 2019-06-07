package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.PreparedGoal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;

import static com.igormaznitsa.prol.data.TermType.ATOM;
import static org.junit.Assert.*;
import org.junit.Test;

public class OperatorTest extends AbstractProlTest {

    @Test
    public void testOperatorDefs() {
        try {
            final ProlContext context = new ProlContext("Operator_test");
            final ProlConsult consult = new ProlConsult(":-op(800,xfx,'<===>'). :-op(700,xfy,'v'). :-op(600,xfy,'&'). :-op(500,fy,'~'). moon <===> earth.", context);
            consult.consult();

            PreparedGoal prepGoal = new PreparedGoal("~(xxx & yyy) <===> ~xxx v ~yyy.", context);
            TermStruct root = (TermStruct) prepGoal.getParsedGoal();

            assertEquals(root.getText(), "<===>");
            assertEquals(root.getArity(), 2);

            TermStruct lefttree = (TermStruct) root.getElement(0);
            TermStruct righttree = (TermStruct) root.getElement(1);

            assertEquals(lefttree.getText(), "~");
            assertEquals(lefttree.getArity(), 1);
            assertEquals(righttree.getText(), "v");
            assertEquals(righttree.getArity(), 2);

            final TermStruct prevrighttree = righttree;

            lefttree = (TermStruct) lefttree.getElement(0);
            righttree = (TermStruct) righttree.getElement(0);

            assertEquals(lefttree.getText(), "&");
            assertEquals(lefttree.getArity(), 2);
            assertEquals(lefttree.getElement(0).getTermType(), ATOM);
            assertEquals(lefttree.getElement(1).getTermType(), ATOM);
            assertEquals(lefttree.getElement(0).getText(), "xxx");
            assertEquals(lefttree.getElement(1).getText(), "yyy");

            lefttree = (TermStruct) prevrighttree.getElement(0);
            righttree = (TermStruct) prevrighttree.getElement(1);

            assertEquals(lefttree.getArity(), 1);
            assertEquals(lefttree.getText(), "~");
            assertEquals(righttree.getArity(), 1);
            assertEquals(righttree.getText(), "~");
            assertEquals(lefttree.getElement(0).getTermType(), ATOM);
            assertEquals(lefttree.getElement(0).getText(), "xxx");
            assertEquals(righttree.getElement(0).getTermType(),ATOM);
            assertEquals(righttree.getElement(0).getText(), "yyy");

            final PreparedGoal prepGoal2 = new PreparedGoal("moon <===> X.", context);
            int solvecounter = 0;
            Term result = null;
            final Goal gl = prepGoal2.getNonparametrizedGoalInstance();
            String text = null;
            while (true) {
                final Term curresult = gl.solve();
                if (curresult == null) {
                    break;
                }

                text = gl.getVarForName("X").getValue().getText();

                result = curresult;
                solvecounter++;
            }

            assertEquals(solvecounter, 1);
            assertEquals(text, "earth");

        } catch (Exception ex) {
            ex.printStackTrace();
            fail(ex.toString());
        }
    }
}
