package com.igormaznitsa.jprol.it;

import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class NonDeterministicAutomataTest extends AbstractJProlTest {

  @Test
  void testNondeterministicAutomata() throws Exception {
    final JProlContext context = makeContextAndConsult("final(s3).trans(s1,a,s1).trans(s1,a,s2).trans(s1,b,s1).trans(s2,b,s3).trans(s3,b,s4).silent(s2,s4).silent(s3,s1).accepts(State,[]):-final(State).accepts(State,[X|Rest]):-trans(State,X,State1),accepts(State1,Rest).accepts(State,String):-silent(State,State1),accepts(State1,String).");

    final String[] etal = new String[] {
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

    final ChoicePoint goal = new ChoicePoint("X=[_,_,_,_,_,_,_],accepts(s1,X).", context);

    for (final String e : etal) {
      assertNotNull(goal.next());
      assertEquals(e, goal.getVarAsText("X"));
    }
    assertNull(goal.next());
  }
}
