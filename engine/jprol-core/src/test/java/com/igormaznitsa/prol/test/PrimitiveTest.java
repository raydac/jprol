package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import org.junit.Test;

import static org.junit.Assert.*;

public class PrimitiveTest extends AbstractProlTest {

    @Test
    public void testAccList() throws Exception {
        final ProlContext context = makeContext("list(X):-X=A,A=B,B=C,C=D,D=E,A=[_,_,_,_],B=[a,_,_,_],C=[_,b,_,_],D=[_,_,c,_],E=[_,_,_,e].");
      final ChoicePoint goal = new ChoicePoint("list(F).", context);

      assertNotNull(goal.next());
        assertEquals(goal.getVarAsText("F"), "['a','b','c','e']");
      assertNull(goal.next());
    }

    @Test
    public void testPermutation() throws Exception {
        final ProlContext context = makeContext("del(X,[X|Tail],Tail).del(X,[Y|Tail],[Y|Tail1]):-del(X,Tail,Tail1).insert(X,List,BiggerList):-del(X,BiggerList,List).permutation([],[]).permutation([X|L],P):-permutation(L,L1),insert(X,L1,P).");
      final ChoicePoint goal = new ChoicePoint("permutation([red,blue,green],X).", context);

        final String[] etalon = new String[]{
                "['red','blue','green']",
                "['blue','red','green']",
                "['blue','green','red']",
                "['red','green','blue']",
                "['green','red','blue']",
                "['green','blue','red']"
        };

        for (String anEtalon : etalon) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), anEtalon);
        }
      assertNull(goal.next());
    }

    @Test
    public void testVarLoop() throws Exception {
        final ProlContext context = makeContext("a(a1).a(X):-X=100. a(2).");
      final ChoicePoint goal = new ChoicePoint("X=Y,Y=X,Y=2.", context);

      assertNotNull(goal.next());
        assertEquals(goal.getVarAsText("Y"), "2");
      assertNull(goal.next());
    }

    @Test
    public void testClause() throws Exception {
        final ProlContext context = makeContext("a(a1).a(X):-X=100. a(2).");
      final ChoicePoint goal = new ChoicePoint("clause(a(X),Y).", context);

      assertNotNull(goal.next());
        assertEquals(goal.getVarAsText("X"), "'a1'");
        assertEquals(goal.getVarAsText("Y"), "'true'");

      assertNotNull(goal.next());
        assertNull(goal.getVarAsText("X"));
        assertEquals(goal.getVarAsText("Y"), "X = 100");

      assertNotNull(goal.next());
        assertEquals(goal.getVarAsText("X"), "2");
        assertEquals(goal.getVarAsText("Y"), "'true'");

      assertNull(goal.next());

    }

    @Test
    public void testThrowCatch() throws Exception {
        final ProlContext context = makeContext("a(X):-X=999;throw(some_exception).");
      final ChoicePoint goal = new ChoicePoint("catch(a(X),error(Err,_),Y='exc').", context);

      assertNotNull(goal.next());
        assertEquals(goal.getVarAsText("X"), "999");
        assertNull(goal.getVarAsText("Err"));
        assertNull(goal.getVarAsText("Y"));

      assertNotNull(goal.next());
        assertNull(goal.getVarAsText("X"));
        assertEquals(goal.getVarAsText("Err"), "some_exception");
        assertEquals(goal.getVarAsText("Y"), "'exc'");

      assertNull(goal.next());
    }

    @Test
    public void testIfThen() throws Exception {
        final ProlContext context = makeContext("b(b1).b(b2).a(a1).a(a2).");

      final ChoicePoint goal = new ChoicePoint("b(B)->a(A);b(C).", context);

        final String[] results = new String[]{"'a1'", "'a2'", "'a3'", "'a4'"};

      assertNotNull(goal.next());
        assertEquals(goal.getVarAsText("B"), "'b1'");
        assertEquals(goal.getVarAsText("A"), "'a1'");
        assertNull(goal.getVarAsText("C"));

      assertNotNull(goal.next());
        assertEquals(goal.getVarAsText("B"), "'b1'");
        assertEquals(goal.getVarAsText("A"), "'a2'");
        assertNull(goal.getVarAsText("C"));

      assertNull(goal.next());
    }

    @Test
    public void testTrue() throws Exception {
        final ProlContext context = makeContext("a(a1).a(a2).a(a3).a(a4).");

      final ChoicePoint goal = new ChoicePoint("(true,true;fail),a(X).", context);

        final String[] results = new String[]{"a1", "a2", "a3", "a4"};

        int index = 0;

        while (true) {
          Term result = goal.next();
            if (result == null) {
                break;
            }
            assertEquals(goal.getVarForName("X").getValue().getText(), results[index++]);
        }

        assertEquals(index, results.length);

    }

    @Test
    public void testOr1() throws Exception {
        final ProlContext context = makeContext("a([]).");

      final ChoicePoint goal = new ChoicePoint("X=a;X=b;X=c;X=d.", context);

        final String[] results = new String[]{"'a'", "'b'", "'c'", "'d'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }

      assertNull(goal.next());
    }

    @Test
    public void testOr2() throws Exception {
        final ProlContext context = makeContext("testor(X,Y):-X='a1',write('x'),Y='b1',write('y');X='a2',write('x'),Y='b2',write('y');X='a3',write('x'),Y='b3',write('y');X='a4',write('x'),Y='b4',write('y');X='a5',write('x'),Y='b5',write('y').");

      final ChoicePoint goal = new ChoicePoint("testor(X,Y).", context);

        final String[] results = new String[]{"a1", "b1", "a2", "b2", "a3", "b3", "a4", "b4", "a5", "b5"};

        int index = 0;
        while (true) {
          final Term term = goal.next();
            if (term == null) {
                break;
            }
            assertEquals(results[index++], goal.getVarForName("X").getValue().getText());
            assertEquals(results[index++], goal.getVarForName("Y").getValue().getText());
        }

        assertEquals(index, results.length);
    }

    @Test
    public void testAnonimousVar() throws Exception {
        final ProlContext context = makeContext("a(a1,b1).a(a2,b2).a(a3,b3).a(a4,b4).");

      final ChoicePoint goal = new ChoicePoint("a(X,_).", context);

        final String[] results = new String[]{"'a1'", "'a2'", "'a3'", "'a4'"};

        int index = 0;
        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), results[index++]);
        }
      assertNull(goal.next());
    }

    @Test
    public void testAtom() throws Exception {
        final ProlContext context = makeContext("a(a1).a(a2).a(a3).a(a4).");

      final ChoicePoint goal = new ChoicePoint("a(X).", context);

        final String[] results = new String[]{"'a1'", "'a2'", "'a3'", "'a4'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }
      assertNull(goal.next());

      final ChoicePoint goal2 = new ChoicePoint("aa(X).", context);
      assertNull(goal2.next());
    }

    @Test
    public void testAtom2() throws Exception {
        final ProlContext context = makeContext("a(a1,b1).a(a2,b2).a(a3,b3).");

      final ChoicePoint goal = new ChoicePoint("a(a3,X).", context);

      assertNotNull(goal.next());
        assertEquals("'b3'", goal.getVarAsText("X"));
      assertNull(goal.next());
    }

    @Test
    public void testSingleRule() throws Exception {
        final ProlContext context = makeContext("a(a1).a(a2).a(a3).a(a4).");

      final ChoicePoint goal = new ChoicePoint("b(X):-a(X).", context);

        final String[] results = new String[]{"'a1'", "'a2'", "'a3'", "'a4'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }
      assertNull(goal.next());
    }

    @Test
    public void testSingleInsideRule() throws Exception {
        final ProlContext context = makeContext("a(a1).a(a2).a(a3).a(a4).b(X):-a(X).");

      final ChoicePoint goal = new ChoicePoint("b(X).", context);

        final String[] results = new String[]{"'a1'", "'a2'", "'a3'", "'a4'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }
      assertNull(goal.next());
    }

    @Test
    public void testSingleInsideRule2() throws Exception {
        final ProlContext context = makeContext("c(c1).c(c2).a(X):-c(X).a(a2).a(a3).a(a4).b(X):-a(X).");

      final ChoicePoint goal = new ChoicePoint("b(X).", context);

        final String[] results = new String[]{"'c1'", "'c2'", "'a2'", "'a3'", "'a4'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }
      assertNull(goal.next());
    }

    @Test
    public void testANDGoal() throws Exception {
        final ProlContext context = makeContext("a(a1).a(a2).b(b1).b(b2).");

      final ChoicePoint goal = new ChoicePoint("a(A),b(B).", context);

        final String[] resultsA = new String[]{"'a1'", "'a2'"};
        final String[] resultsB = new String[]{"'b1'", "'b2'"};

        for (int la = 0; la < 2; la++) {
            for (int lb = 0; lb < 2; lb++) {
              assertNotNull(goal.next());
                assertEquals(goal.getVarAsText("A"), resultsA[la]);
                assertEquals(goal.getVarAsText("B"), resultsB[lb]);
            }
        }

      assertNull(goal.next());
    }

    @Test
    public void testORGoal() throws Exception {
        final ProlContext context = makeContext("a(a1).a(a2).b(b1).b(b2).");

      final ChoicePoint goal = new ChoicePoint("a(A);b(A).", context);
        final String[] results = new String[]{"'a1'", "'a2'", "'b1'", "'b2'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("A"), result);
        }
      assertNull(goal.next());
    }

    @Test
    public void testCutAnd() throws Exception {
        final ProlContext context = makeContext("a(a1).a(a2).b(b1).b(b2).");

      final ChoicePoint goal = new ChoicePoint("a(A),!,b(B).", context);

        final String[] resultsA = new String[]{"'a1'"};
        final String[] resultsB = new String[]{"'b1'", "'b2'"};

        for (String aResultsA : resultsA) {
            for (String aResultsB : resultsB) {
              assertNotNull(goal.next());
                assertEquals(goal.getVarAsText("A"), aResultsA);
                assertEquals(goal.getVarAsText("B"), aResultsB);
            }
        }

      assertNull(goal.next());
    }

    @Test
    public void testCut1() throws Exception {
        final ProlContext context = makeContext("a(a1):-!.a(a2).a(a3).a(a4).");

      final ChoicePoint goal = new ChoicePoint("a(X).", context);
      assertNotNull(goal.next());
        assertEquals(goal.getVarAsText("X"), "'a1'");
      assertNull(goal.next());
    }

    @Test
    public void testCut2() throws Exception {
        final ProlContext context = makeContext("c(c1).c(c2).a(X):-c(X).a(a2):-!.a(a3).a(a4).b(X):-a(X).");

      final ChoicePoint goal = new ChoicePoint("b(X).", context);

        final String[] results = new String[]{"'c1'", "'c2'", "'a2'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }
      assertNull(goal.next());
    }

    @Test
    public void testCut3() throws Exception {
        final ProlContext context = makeContext("c(c1).c(c2).a(X):-!,c(X).a(a2).a(a3).a(a4).b(X):-a(X).");

      final ChoicePoint goal = new ChoicePoint("b(X).", context);

        final String[] results = new String[]{"'c1'", "'c2'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }
      assertNull(goal.next());
    }

    @Test
    public void testCut4() throws Exception {
        final ProlContext context = makeContext("f(f1).f(f2).c(c1).c(c2).c(c3).a(X):-c(X);!,f(X).a(a2).a(a3).a(a4).b(X):-a(X).");

      final ChoicePoint goal = new ChoicePoint("b(X).", context);

        final String[] results = new String[]{"'c1'", "'c2'", "'c3'", "'f1'", "'f2'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }
      assertNull(goal.next());
    }

    @Test
    public void testLocalCut() throws Exception {
        final ProlContext context = makeContext("f(f1).f(f2).c(c1).c(c2).c(c3).a(X):-c(X),!!.a(a2).a(a3).a(a4).b(X):-a(X).");

      final ChoicePoint goal = new ChoicePoint("b(X).", context);
        final String[] results = new String[]{"'c1'", "'a2'", "'a3'", "'a4'"};

        for (String result : results) {
          assertNotNull(goal.next());
            assertEquals(goal.getVarAsText("X"), result);
        }
      assertNull(goal.next());

    }

    private ProlContext makeContext(final String knowledgeBase) throws Exception {
        final ProlContext context = new ProlContext("PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult(knowledgeBase, context);
        consult.consult();
        return context;
    }
}
