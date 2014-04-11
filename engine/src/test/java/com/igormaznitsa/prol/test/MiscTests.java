package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.*;
import com.igormaznitsa.prol.parser.ProlConsult;
import org.junit.Test;
import static org.junit.Assert.*;

public class MiscTests {

    @Test
    public void findAllTest() throws Exception {
        ProlContext context;
        context = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult("powerSet([],[]).powerSet([_|Xt],Y) :- powerSet(Xt,Y).powerSet([Xh|Xt],[Xh|Yt]) :- powerSet(Xt,Yt).", context);
        consult.consult();
        final Goal goal = new Goal("findall(X,powerSet([a,b],X),Y).", context);

        Var result = null;

        while (true) {
            final Term t = goal.solve();
            if (t == null) {
                break;
            }
            final Var valy = goal.getVarForName("Y");
            assertNull(result);
            result =  valy;
        }

        assertNotNull(result);
        assertTrue(result.getValue() instanceof TermList);
        assertEquals("[[],['b'],['a'],['a','b']]",result.getValue().toString());
    }
}
