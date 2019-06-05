package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import static org.junit.Assert.*;
import org.junit.Test;

public class NonDeterministicAutomata extends AbstractProlTest {

    @Test
    public void testNondeterministicAutomata() throws Throwable {
        final ProlContext context = new ProlContext("test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult("final(s3).trans(s1,a,s1).trans(s1,a,s2).trans(s1,b,s1).trans(s2,b,s3).trans(s3,b,s4).silent(s2,s4).silent(s3,s1).accepts(State,[]):-final(State).accepts(State,[X|Rest]):-trans(State,X,State1),accepts(State1,Rest).accepts(State,String):-silent(State,State1),accepts(State1,String).", context);
        consult.consult();

        final String[] etal = new String[]{
                "['a','a','a','a','a','a','b']",
                "['a','a','a','a','b','a','b']",
                "['a','a','a','a','b','a','b']",
                "['a','a','a','b','a','a','b']",
                "['a','a','a','b','b','a','b']",
                "['a','a','a','b','a','a','b']",
                "['a','a','a','b','b','a','b']",
                "['a','a','b','a','a','a','b']",
                "['a','a','b','a','b','a','b']",
                "['a','a','b','a','b','a','b']",
                "['a','a','b','b','a','a','b']",
                "['a','a','b','b','b','a','b']",
                "['a','a','b','a','a','a','b']",
                "['a','a','b','a','b','a','b']",
                "['a','a','b','a','b','a','b']",
                "['a','a','b','b','a','a','b']",
                "['a','a','b','b','b','a','b']",
                "['a','b','a','a','a','a','b']",
                "['a','b','a','a','b','a','b']",
                "['a','b','a','a','b','a','b']",
                "['a','b','a','b','a','a','b']",
                "['a','b','a','b','b','a','b']",
                "['a','b','a','b','a','a','b']",
                "['a','b','a','b','b','a','b']",
                "['a','b','b','a','a','a','b']",
                "['a','b','b','a','b','a','b']",
                "['a','b','b','a','b','a','b']",
                "['a','b','b','b','a','a','b']",
                "['a','b','b','b','b','a','b']",
                "['a','b','a','a','a','a','b']",
                "['a','b','a','a','b','a','b']",
                "['a','b','a','a','b','a','b']",
                "['a','b','a','b','a','a','b']",
                "['a','b','a','b','b','a','b']",
                "['a','b','a','b','a','a','b']",
                "['a','b','a','b','b','a','b']",
                "['a','b','b','a','a','a','b']",
                "['a','b','b','a','b','a','b']",
                "['a','b','b','a','b','a','b']",
                "['a','b','b','b','a','a','b']",
                "['a','b','b','b','b','a','b']",
                "['b','a','a','a','a','a','b']",
                "['b','a','a','a','b','a','b']",
                "['b','a','a','a','b','a','b']",
                "['b','a','a','b','a','a','b']",
                "['b','a','a','b','b','a','b']",
                "['b','a','a','b','a','a','b']",
                "['b','a','a','b','b','a','b']",
                "['b','a','b','a','a','a','b']",
                "['b','a','b','a','b','a','b']",
                "['b','a','b','a','b','a','b']",
                "['b','a','b','b','a','a','b']",
                "['b','a','b','b','b','a','b']",
                "['b','a','b','a','a','a','b']",
                "['b','a','b','a','b','a','b']",
                "['b','a','b','a','b','a','b']",
                "['b','a','b','b','a','a','b']",
                "['b','a','b','b','b','a','b']",
                "['b','b','a','a','a','a','b']",
                "['b','b','a','a','b','a','b']",
                "['b','b','a','a','b','a','b']",
                "['b','b','a','b','a','a','b']",
                "['b','b','a','b','b','a','b']",
                "['b','b','a','b','a','a','b']",
                "['b','b','a','b','b','a','b']",
                "['b','b','b','a','a','a','b']",
                "['b','b','b','a','b','a','b']",
                "['b','b','b','a','b','a','b']",
                "['b','b','b','b','a','a','b']",
                "['b','b','b','b','b','a','b']"
        };

        final Goal goal = new Goal("X=[_,_,_,_,_,_,_],accepts(s1,X).", context);

        for (final String e : etal) {
            assertNotNull(goal.solve());
            assertEquals(goal.getVarAsText("X"), e);
        }
        assertNull(goal.solve());
    }
}
