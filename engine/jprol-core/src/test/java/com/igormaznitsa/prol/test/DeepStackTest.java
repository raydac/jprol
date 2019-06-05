package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.utils.Utils;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;
import org.junit.Ignore;
import org.junit.Test;

@Ignore
public class DeepStackTest extends AbstractProlTest {

    private TermList makeList(final int depth) throws Exception {
        final ProlContext context = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult("depth(0,[]):-!.depth(X,R):-X1 is X-1, depth(X1,R2), R=[X|R2].", context);
        consult.consult();
        final String goalText = "depth(" + depth + ", R).";
        final Goal goal = new Goal(goalText, context);
        final Term resultterm = goal.solve();
        assertNotNull(resultterm);
        final TermList result = (TermList) goal.getVarForName("R").getValue();
        return result;
    }

    private boolean checkList(int depth, TermList list) {
        TermList cList = list;
        while (!cList.isNullList()) {
            final TermInteger intterm = (TermInteger) Utils.getTermFromElement(cList.getHead());
            if (depth != intterm.getValue()) {
                return false;
            }
            depth--;
            cList = (TermList) Utils.getTermFromElement(cList.getTail());
        }
        return true;
    }

    @Test
    public void testDeepStack() throws Exception {
        final int DEPTH = 5000;
        final TermList result = makeList(DEPTH);
        assertNotNull(result);
        assertTrue(checkList(DEPTH, result));
    }
}
