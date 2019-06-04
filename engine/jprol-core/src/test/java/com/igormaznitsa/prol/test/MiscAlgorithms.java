package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import static org.junit.Assert.*;
import org.junit.Test;

public class MiscAlgorithms extends AbstractProlTest {

    private void akkermanCalculate(int m, int n, int a) throws Throwable {
        final ProlContext context = new ProlContext(this, "test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult("akkerman(0,N,X):- X is N+1,!. akkerman(M,0,X):- Mn is M-1, !, akkerman(Mn,1,X). akkerman(M,N,X):- Mn is M-1, Nn is N-1, !, akkerman(M,Nn,Y), !, akkerman(Mn,Y,X).", context);
        consult.consult();
        final String goalText = "akkerman(" + m + ',' + n + ",A).";
        final Goal goal = new Goal(goalText, context);
        final Term resultterm = goal.solve();
        assertNotNull(resultterm);
        final Term result = goal.getVarForName("A").getValue();
        assertNotNull(result);
        final int intresult = ((TermInteger) result).getNumericValue().intValue();
        assertEquals(a, intresult);
        assertNull(goal.solve());
    }

    @Test
    public void testAkkerman() {
        try {
            akkermanCalculate(0, 0, 1);
            akkermanCalculate(1, 0, 2);
            akkermanCalculate(2, 0, 3);
            akkermanCalculate(3, 0, 5);
            akkermanCalculate(4, 0, 13);

            akkermanCalculate(0, 1, 2);
            akkermanCalculate(1, 1, 3);
            akkermanCalculate(2, 1, 5);
            akkermanCalculate(3, 1, 13);

            akkermanCalculate(0, 5, 6);
            akkermanCalculate(1, 5, 7);
            akkermanCalculate(2, 5, 13);
            akkermanCalculate(3, 5, 253);

//            akkermanCalculate(4, 1, 65533); // in linux it hangs java
        } catch (Throwable thr) {
            thr.printStackTrace();
            fail("Exception during operation");
        }
    }

    @Test
    public void testFibbonachi() throws Exception {
        final ProlContext context = makeContext("fib(1,1):-!. fib(0,0):-!. fib(N,Result):-Npp is N-2, Np is N-1, fib(Npp,Resultpp), fib(Np,Resultp), Result is Resultpp+Resultp.");

        final Goal goal = new Goal("fib(22,Result).", context);

        assertNotNull(goal.solve());
        assertEquals(goal.getVarAsNumber("Result").intValue(), 17711);
        assertNull(goal.solve());
    }

    @Test
    public void testObject() throws Exception {
        final ProlContext context = makeContext("object(rectangle(Length,Width),[(area(A):-A is Length * Width),(describe :- write('Rectangle of size'), write(Length * Width))]).send(Object,Message):-get_methods(Object,Methods),process(Message,Methods)."
                + "get_methods(Object,Methods):-object(Object,Methods)."
                + "process(Message,[Message|_])."
                + "process(Message,[(Message :- Body)|_]):-call(Body)."
                + "process(Message,[_|Methods]):-process(Message,Methods).");

        final Goal goal = new Goal("Rec1=rectangle(4,3),send(Rec1,area(Area)).", context);

        assertNotNull(goal.solve());
        assertEquals(goal.getVarAsNumber("Area").intValue(), 12);
        assertNull(goal.solve());
    }

    private ProlContext makeContext(final String knowledgeBase) throws Exception {
        final ProlContext context = new ProlContext(this, "PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult(knowledgeBase, context);
        consult.consult();
        return context;
    }
}
