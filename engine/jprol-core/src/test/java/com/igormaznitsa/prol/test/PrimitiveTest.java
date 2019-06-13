package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PrimitiveTest extends AbstractProlTest {

  @Test
  void testAccList() throws Exception {
    final ProlContext context = makeContext("list(X):-X=A,A=B,B=C,C=D,D=E,A=[_,_,_,_],B=[a,_,_,_],C=[_,b,_,_],D=[_,_,c,_],E=[_,_,_,e].");
    final ChoicePoint goal = new ChoicePoint("list(F).", context);

    assertNotNull(goal.next());
    assertEquals("['a','b','c','e']", goal.getVarAsText("F"));
    assertNull(goal.next());
  }

  @Test
  void testPermutation() throws Exception {
    final ProlContext context = makeContext("del(X,[X|Tail],Tail).del(X,[Y|Tail],[Y|Tail1]):-del(X,Tail,Tail1).insert(X,List,BiggerList):-del(X,BiggerList,List).permutation([],[]).permutation([X|L],P):-permutation(L,L1),insert(X,L1,P).");
    final ChoicePoint goal = new ChoicePoint("permutation([red,blue,green],X).", context);

    final String[] etalon = new String[] {
        "['red','blue','green']",
        "['blue','red','green']",
        "['blue','green','red']",
        "['red','green','blue']",
        "['green','red','blue']",
        "['green','blue','red']"
    };

    for (final String etal : etalon) {
      assertNotNull(goal.next());
      assertEquals(etal, goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }

  @Test
  void testVarLoop() throws Exception {
    final ProlContext context = makeContext("a(a1).a(X):-X=100. a(2).");
    final ChoicePoint goal = new ChoicePoint("X=Y,Y=X,Y=2.", context);

    assertNotNull(goal.next());
    assertEquals("2", goal.getVarAsText("Y"));
    assertNull(goal.next());
  }

  @Test
  void testClause() throws Exception {
    final ProlContext context = makeContext("a(a1).a(X):-X=100. a(2).");
    final ChoicePoint goal = new ChoicePoint("clause(a(X),Y).", context);

    assertNotNull(goal.next());
    assertEquals("'a1'", goal.getVarAsText("X"));
    assertEquals("'true'", goal.getVarAsText("Y"));

    assertNotNull(goal.next());
    assertNull(goal.getVarAsText("X"));
    assertEquals("X = 100", goal.getVarAsText("Y"));

    assertNotNull(goal.next());
    assertEquals("2", goal.getVarAsText("X"));
    assertEquals("'true'", goal.getVarAsText("Y"));

    assertNull(goal.next());

  }

  @Test
  void testThrowCatch() throws Exception {
    final ProlContext context = makeContext("a(X):-X=999;throw(some_exception).");
    final ChoicePoint goal = new ChoicePoint("catch(a(X),error(Err,_),Y='exc').", context);

    assertNotNull(goal.next());
    assertEquals("999", goal.getVarAsText("X"));
    assertNull(goal.getVarAsText("Err"));
    assertNull(goal.getVarAsText("Y"));

    assertNotNull(goal.next());
    assertNull(goal.getVarAsText("X"));
    assertEquals("some_exception", goal.getVarAsText("Err"));
    assertEquals("'exc'", goal.getVarAsText("Y"));

    assertNull(goal.next());
  }

  @Test
  void testIfThen() throws Exception {
    final ProlContext context = makeContext("b(b1).b(b2).a(a1).a(a2).");

    final ChoicePoint goal = new ChoicePoint("b(B)->a(A);b(C).", context);

    assertNotNull(goal.next());
    assertEquals("'b1'", goal.getVarAsText("B"));
    assertEquals("'a1'", goal.getVarAsText("A"));
    assertNull(goal.getVarAsText("C"));

    assertNotNull(goal.next());
    assertEquals("'b1'", goal.getVarAsText("B"));
    assertEquals("'a2'", goal.getVarAsText("A"));
    assertNull(goal.getVarAsText("C"));

    assertNull(goal.next());
  }

  @Test
  void testTrue() throws Exception {
    final ProlContext context = makeContext("a(a1).a(a2).a(a3).a(a4).");

    final ChoicePoint goal = new ChoicePoint("(true,true;fail),a(X).", context);

    final String[] results = new String[] {"a1", "a2", "a3", "a4"};

    int index = 0;

    while (goal.next() != null) {
      assertEquals(results[index++], goal.getVarForName("X").getValue().getText());
    }

    assertEquals(results.length, index);
  }

  @Test
  void testOr1() throws Exception {
    final ProlContext context = makeContext("a([]).");

    final ChoicePoint goal = new ChoicePoint("X=a;X=b;X=c;X=d.", context);

    final String[] results = new String[] {"'a'", "'b'", "'c'", "'d'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
    }

    assertNull(goal.next());
  }

  @Test
  void testOr2() throws Exception {
    final ProlContext context = makeContext("testor(X,Y):-X='a1',write('x'),Y='b1',write('y');X='a2',write('x'),Y='b2',write('y');X='a3',write('x'),Y='b3',write('y');X='a4',write('x'),Y='b4',write('y');X='a5',write('x'),Y='b5',write('y').");

    final ChoicePoint goal = new ChoicePoint("testor(X,Y).", context);

    final String[] results = new String[] {"a1", "b1", "a2", "b2", "a3", "b3", "a4", "b4", "a5", "b5"};

    int index = 0;
    Term result;
    while ((result = goal.next()) != null) {
      assertEquals(results[index++], goal.getVarForName("X").getValue().getText());
      assertEquals(results[index++], goal.getVarForName("Y").getValue().getText());
    }

    assertEquals(index, results.length);
  }

  @Test
  void testAnonimousVar() throws Exception {
    final ProlContext context = makeContext("a(a1,b1).a(a2,b2).a(a3,b3).a(a4,b4).");

    final ChoicePoint goal = new ChoicePoint("a(X,_).", context);

    final String[] results = new String[] {"'a1'", "'a2'", "'a3'", "'a4'"};

    int index = 0;
    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(results[index++], goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }

  @Test
  void testAtom() throws Exception {
    final ProlContext context = makeContext("a(a1).a(a2).a(a3).a(a4).");

    final ChoicePoint goal = new ChoicePoint("a(X).", context);

    final String[] results = new String[] {"'a1'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
    }
    assertNull(goal.next());

    final ChoicePoint goal2 = new ChoicePoint("aa(X).", context);
    assertNull(goal2.next());
  }

  @Test
  void testAtom2() throws Exception {
    final ProlContext context = makeContext("a(a1,b1).a(a2,b2).a(a3,b3).");

    final ChoicePoint goal = new ChoicePoint("a(a3,X).", context);

    assertNotNull(goal.next());
    assertEquals("'b3'", goal.getVarAsText("X"));
    assertNull(goal.next());
  }

  @Test
  void testSingleRule() throws Exception {
    final ProlContext context = makeContext("a(a1).a(a2).a(a3).a(a4).");

    final ChoicePoint goal = new ChoicePoint("b(X):-a(X).", context);

    final String[] results = new String[] {"'a1'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }

  @Test
  void testSingleInsideRule() throws Exception {
    final ProlContext context = makeContext("a(a1).a(a2).a(a3).a(a4).b(X):-a(X).");

    final ChoicePoint goal = new ChoicePoint("b(X).", context);

    final String[] results = new String[] {"'a1'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }

  @Test
  void testSingleInsideRule2() throws Exception {
    final ProlContext context = makeContext("c(c1).c(c2).a(X):-c(X).a(a2).a(a3).a(a4).b(X):-a(X).");

    final ChoicePoint goal = new ChoicePoint("b(X).", context);

    final String[] results = new String[] {"'c1'", "'c2'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }

  @Test
  void testANDGoal() throws Exception {
    final ProlContext context = makeContext("a(a1).a(a2).b(b1).b(b2).");

    final ChoicePoint goal = new ChoicePoint("a(A),b(B).", context);

    final String[] resultsA = new String[] {"'a1'", "'a2'"};
    final String[] resultsB = new String[] {"'b1'", "'b2'"};

    for (int la = 0; la < 2; la++) {
      for (int lb = 0; lb < 2; lb++) {
        assertNotNull(goal.next());
        assertEquals(resultsA[la], goal.getVarAsText("A"));
        assertEquals(resultsB[lb], goal.getVarAsText("B"));
      }
    }
    assertNull(goal.next());
  }

  @Test
  void testORGoal() throws Exception {
    final ProlContext context = makeContext("a(a1).a(a2).b(b1).b(b2).");

    final ChoicePoint goal = new ChoicePoint("a(A);b(A).", context);
    final String[] results = new String[] {"'a1'", "'a2'", "'b1'", "'b2'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("A"));
    }
    assertNull(goal.next());
  }

  @Test
  void testCutAnd() throws Exception {
    final ProlContext context = makeContext("a(a1).a(a2).b(b1).b(b2).");

    final ChoicePoint goal = new ChoicePoint("a(A),!,b(B).", context);

    final String[] resultsA = new String[] {"'a1'"};
    final String[] resultsB = new String[] {"'b1'", "'b2'"};

    for (final String aResultsA : resultsA) {
      for (final String aResultsB : resultsB) {
        assertNotNull(goal.next());
        assertEquals(aResultsA, goal.getVarAsText("A"));
        assertEquals(aResultsB, goal.getVarAsText("B"));
      }
    }

    assertNull(goal.next());
  }

  @Test
  void testCut1() throws Exception {
    final ProlContext context = makeContext("a(a1):-!.a(a2).a(a3).a(a4).");

    final ChoicePoint goal = new ChoicePoint("a(X).", context);
    assertNotNull(goal.next());
    assertEquals(goal.getVarAsText("X"), "'a1'");
    assertNull(goal.next());
  }

  @Test
  void testCut2() throws Exception {
    final ProlContext context = makeContext("c(c1).c(c2).a(X):-c(X).a(a2):-!.a(a3).a(a4).b(X):-a(X).");

    final ChoicePoint goal = new ChoicePoint("b(X).", context);

    final String[] results = new String[] {"'c1'", "'c2'", "'a2'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }

  @Test
  void testCut3() throws Exception {
    final ProlContext context = makeContext("c(c1).c(c2).a(X):-!,c(X).a(a2).a(a3).a(a4).b(X):-a(X).");

    final ChoicePoint goal = new ChoicePoint("b(X).", context);

    final String[] results = new String[] {"'c1'", "'c2'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }

  @Test
  void testCut4() throws Exception {
    final ProlContext context = makeContext("f(f1).f(f2).c(c1).c(c2).c(c3).a(X):-c(X);!,f(X).a(a2).a(a3).a(a4).b(X):-a(X).");

    final ChoicePoint goal = new ChoicePoint("b(X).", context);

    final String[] results = new String[] {"'c1'", "'c2'", "'c3'", "'f1'", "'f2'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }

  @Test
  void testLocalCut() throws Exception {
    final ProlContext context = makeContext("f(f1).f(f2).c(c1).c(c2).c(c3).a(X):-c(X),!!.a(a2).a(a3).a(a4).b(X):-a(X).");

    final ChoicePoint goal = new ChoicePoint("b(X).", context);
    final String[] results = new String[] {"'c1'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.next());
      assertEquals(result, goal.getVarAsText("X"));
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
