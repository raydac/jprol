package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import java.io.StringWriter;
import java.io.Writer;
import org.junit.jupiter.api.Test;

class PrimitiveTest extends AbstractJProlTest {

  @Test
  void testAccList() {
    final JProlContext context = makeContextAndConsult("list(X):-X=A,A=B,B=C,C=D,D=E,A=[_,_,_,_],B=[a,_,_,_],C=[_,b,_,_],D=[_,_,c,_],E=[_,_,_,e].");
    final JProlChoicePoint goal = new JProlChoicePoint("list(F).", context);

    assertNotNull(goal.prove());
    assertEquals("['a','b','c','e']", getVarAsText(goal, "F"));
    assertNull(goal.prove());
  }

  @Test
  void testPermutation() {
    final JProlContext context = makeContextAndConsult("del(X,[X|Tail],Tail).del(X,[Y|Tail],[Y|Tail1]):-del(X,Tail,Tail1).insert(X,List,BiggerList):-del(X,BiggerList,List).permutation([],[]).permutation([X|L],P):-permutation(L,L1),insert(X,L1,P).");
    final JProlChoicePoint goal = new JProlChoicePoint("permutation([red,blue,green],X).", context);

    final String[] etalon = new String[] {
        "['red','blue','green']",
        "['blue','red','green']",
        "['blue','green','red']",
        "['red','green','blue']",
        "['green','red','blue']",
        "['green','blue','red']"
    };

    for (final String etal : etalon) {
      assertNotNull(goal.prove());
      assertEquals(etal, getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());
  }

  @Test
  void testVarLoop() {
    final JProlContext context = makeContextAndConsult("a(a1).a(X):-X=100. a(2).");
    final JProlChoicePoint goal = new JProlChoicePoint("X=Y,Y=X,Y=2.", context);

    assertNotNull(goal.prove());
    assertEquals("2", getVarAsText(goal, "Y"));
    assertNull(goal.prove());
  }

  @Test
  void testClause() {
    final JProlContext context = makeContextAndConsult("a(a1).a(X):-X=100. a(2).");
    final JProlChoicePoint goal = new JProlChoicePoint("clause(a(X),Y).", context);

    assertNotNull(goal.prove());
    assertEquals("'a1'", getVarAsText(goal, "X"));
    assertEquals("'true'", getVarAsText(goal, "Y"));

    assertNotNull(goal.prove());
    assertEquals("X", getVarAsText(goal, "X"));
    assertEquals("X = 100", getVarAsText(goal, "Y"));

    assertNotNull(goal.prove());
    assertEquals("2", getVarAsText(goal, "X"));
    assertEquals("'true'", getVarAsText(goal, "Y"));

    assertNull(goal.prove());

  }

  @Test
  void testThrowCatch() {
    final JProlContext context = makeContextAndConsult("a(X):-X=999;throw(some_exception).");
    final JProlChoicePoint goal = new JProlChoicePoint("catch(a(X),error(Err,_),Y='exc').", context);

    assertNotNull(goal.prove());
    assertEquals("999", getVarAsText(goal, "X"));
    assertNull(getVarAsText(goal, "Err"));
    assertNull(getVarAsText(goal, "Y"));

    assertNotNull(goal.prove());
    assertNull(getVarAsText(goal, "X"));
    assertEquals("some_exception", getVarAsText(goal, "Err"));
    assertEquals("'exc'", getVarAsText(goal, "Y"));

    assertNull(goal.prove());
  }

  @Test
  void testIfThen() {
    final JProlContext context = makeContextAndConsult("b(b1).b(b2).a(a1).a(a2).");

    final JProlChoicePoint goal = new JProlChoicePoint("b(B)->a(A);b(C).", context);

    assertNotNull(goal.prove());
    assertEquals("'b1'", getVarAsText(goal, "B"));
    assertEquals("'a1'", getVarAsText(goal, "A"));
    assertNull(getVarAsText(goal, "C"));

    assertNotNull(goal.prove());
    assertEquals("'b1'", getVarAsText(goal, "B"));
    assertEquals("'a2'", getVarAsText(goal, "A"));
    assertNull(getVarAsText(goal, "C"));

    assertNull(goal.prove());
  }

  @Test
  void testTrue() {
    final JProlContext context = makeContextAndConsult("a(a1).a(a2).a(a3).a(a4).");

    final JProlChoicePoint goal = new JProlChoicePoint("(true,true;fail),a(X).", context);

    final String[] results = new String[] {"a1", "a2", "a3", "a4"};

    int index = 0;

    while (goal.prove() != null) {
      assertEquals(results[index++], goal.findVar("X").get().getValue().getText());
    }

    assertEquals(results.length, index);
  }

  @Test
  void testOr1() {
    final JProlContext context = makeContextAndConsult("a([]).");

    final JProlChoicePoint goal = new JProlChoicePoint("X=a;X=b;X=c;X=d.", context);

    final String[] results = new String[] {"'a'", "'b'", "'c'", "'d'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "X"));
    }

    assertNull(goal.prove());
  }

  @Test
  void testOr2() {
    final JProlContext context = makeContextAndConsult(
        "testor(X,Y):-" +
            "X='a1'," +
            "write('x')," +
            "Y='b1'," +
            "write('y');X='a2'," +
            "write('x'),Y='b2'," +
            "write('y');X='a3'," +
            "write('x'),Y='b3'," +
            "write('y');X='a4'," +
            "write('x'),Y='b4'," +
            "write('y');X='a5'," +
            "write('x'),Y='b5'," +
            "write('y')."
    ).addIoResourceProvider(new IoResourceProvider() {
      @Override
      public Writer findWriter(JProlContext context, String writerId, boolean append) {
        return new StringWriter();
      }
    });

    final JProlChoicePoint goal = new JProlChoicePoint("testor(X,Y).", context);

    final String[] results = new String[] {"a1", "b1", "a2", "b2", "a3", "b3", "a4", "b4", "a5", "b5"};

    int index = 0;
    while (goal.prove() != null) {
      assertEquals(results[index++], goal.findVar("X").get().getValue().getText());
      assertEquals(results[index++], goal.findVar("Y").get().getValue().getText());
    }

    assertEquals(results.length, index);
  }

  @Test
  void testAnonimousVar() {
    final JProlContext context = makeContextAndConsult("a(a1,b1).a(a2,b2).a(a3,b3).a(a4,b4).");

    final JProlChoicePoint goal = new JProlChoicePoint("a(X,_).", context);

    final String[] results = new String[] {"'a1'", "'a2'", "'a3'", "'a4'"};

    int index = 0;
    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(results[index++], getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());
  }

  @Test
  void testAtom() {
    final JProlContext context = makeContextAndConsult("a(a1).a(a2).a(a3).a(a4).");

    final JProlChoicePoint goal = new JProlChoicePoint("a(X).", context);

    final String[] results = new String[] {"'a1'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());

    final JProlChoicePoint goal2 = new JProlChoicePoint("aa(X).", context);
    assertNull(goal2.proveWithFailForUnknown());
  }

  @Test
  void testAtom2() {
    final JProlContext context = makeContextAndConsult("a(a1,b1).a(a2,b2).a(a3,b3).");

    final JProlChoicePoint goal = new JProlChoicePoint("a(a3,X).", context);

    assertNotNull(goal.prove());
    assertEquals("'b3'", getVarAsText(goal, "X"));
    assertNull(goal.prove());
  }

  @Test
  void testDryUnify() {
    final JProlContext context = makeContextAndConsult("a(a1,b1).a(a2,b2).a(a3,b3).");

    final Term term1 = new JProlChoicePoint("replace(['a','b','c','d','e','f','g'],'e',1,X).", context).getGoalTerm();
    final Term term2 = new JProlChoicePoint("replace(['a','b','c','d','e','f','g'],'a',N,[N|Lt]).", context).getGoalTerm();
    final Term term3 = new JProlChoicePoint("replace(['a','b','c','d','e','f','g'],'e',N,[N|Lt]).", context).getGoalTerm();

    assertFalse(term1.dryUnifyTo(term2));
    assertFalse(term2.dryUnifyTo(term1));
    assertTrue(term1.dryUnifyTo(term3));
    assertTrue(term3.dryUnifyTo(term1));
  }

  @Test
  void testSingleRule() {
    final JProlContext context = makeContextAndConsult("a(a1).a(a2).a(a3).a(a4).");

    final JProlChoicePoint goal = new JProlChoicePoint("b(X):-a(X).", context);

    final String[] results = new String[] {"'a1'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());
  }

  @Test
  void testSingleInsideRule() {
    final JProlContext context = makeContextAndConsult("a(a1).a(a2).a(a3).a(a4).b(X):-a(X).");

    final JProlChoicePoint goal = new JProlChoicePoint("b(X).", context);

    final String[] results = new String[] {"'a1'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());
  }

  @Test
  void testSingleInsideRule2() {
    final JProlContext context = makeContextAndConsult("c(c1).c(c2).a(X):-c(X).a(a2).a(a3).a(a4).b(X):-a(X).");

    final JProlChoicePoint goal = new JProlChoicePoint("b(X).", context);

    final String[] results = new String[] {"'c1'", "'c2'", "'a2'", "'a3'", "'a4'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());
  }

  @Test
  void testANDGoal() {
    final JProlContext context = makeContextAndConsult("a(a1).a(a2).b(b1).b(b2).");

    final JProlChoicePoint goal = new JProlChoicePoint("a(A),b(B).", context);

    final String[] resultsA = new String[] {"'a1'", "'a2'"};
    final String[] resultsB = new String[] {"'b1'", "'b2'"};

    for (int la = 0; la < 2; la++) {
      for (int lb = 0; lb < 2; lb++) {
        assertNotNull(goal.prove());
        assertEquals(resultsA[la], getVarAsText(goal, "A"));
        assertEquals(resultsB[lb], getVarAsText(goal, "B"));
      }
    }
    assertNull(goal.prove());
  }

  @Test
  void testORGoal() {
    final JProlContext context = makeContextAndConsult("a(a1).a(a2).b(b1).b(b2).");

    final JProlChoicePoint goal = new JProlChoicePoint("a(A);b(A).", context);
    final String[] results = new String[] {"'a1'", "'a2'", "'b1'", "'b2'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "A"));
    }
    assertNull(goal.prove());
  }

  @Test
  void testCutAnd() {
    final JProlContext context = makeContextAndConsult("a(a1).a(a2).b(b1).b(b2).");

    final JProlChoicePoint goal = new JProlChoicePoint("a(A),!,b(B).", context);

    final String[] resultsA = new String[] {"'a1'"};
    final String[] resultsB = new String[] {"'b1'", "'b2'"};

    for (final String aResultsA : resultsA) {
      for (final String aResultsB : resultsB) {
        assertNotNull(goal.prove());
        assertEquals(aResultsA, getVarAsText(goal, "A"));
        assertEquals(aResultsB, getVarAsText(goal, "B"));
      }
    }

    assertNull(goal.prove());
  }

  @Test
  void testCut1() {
    final JProlContext context = makeContextAndConsult("a(a1):-!.a(a2).a(a3).a(a4).");

    final JProlChoicePoint goal = new JProlChoicePoint("a(X).", context);
    assertNotNull(goal.prove());
    assertEquals(getVarAsText(goal, "X"), "'a1'");
    assertNull(goal.prove());
  }

  @Test
  void testCut2() {
    final JProlContext context = makeContextAndConsult("c(c1).c(c2).a(X):-c(X).a(a2):-!.a(a3).a(a4).b(X):-a(X).");

    final JProlChoicePoint goal = new JProlChoicePoint("b(X).", context);

    final String[] results = new String[] {"'c1'", "'c2'", "'a2'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());
  }

  @Test
  void testCut3() {
    final JProlContext context = makeContextAndConsult("c(c1).c(c2).a(X):-!,c(X).a(a2).a(a3).a(a4).b(X):-a(X).");

    final JProlChoicePoint goal = new JProlChoicePoint("b(X).", context);

    final String[] results = new String[] {"'c1'", "'c2'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());
  }

  @Test
  void testCut4() {
    final JProlContext context = makeContextAndConsult("f(f1).f(f2).c(c1).c(c2).c(c3).a(X):-c(X);!,f(X).a(a2).a(a3).a(a4).b(X):-a(X).");

    final JProlChoicePoint goal = new JProlChoicePoint("b(X).", context);

    final String[] results = new String[] {"'c1'", "'c2'", "'c3'", "'f1'", "'f2'"};

    for (final String result : results) {
      assertNotNull(goal.prove());
      assertEquals(result, getVarAsText(goal, "X"));
    }
    assertNull(goal.prove());
  }

}
