package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

class ListTest extends AbstractJProlTest {

  @Test
  void testRunningOne() {
    final JProlContext context = makeContextAndConsult("member2(E,[E|_]). member2(E,[_|L]) :- member2(E,L). runningOne(X):-X=[_,_,_,_],member2(1,X).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("runningOne(X).", context);
    final String[] etalon = new String[] {
        "[1,_,_,_]",
        "[_,1,_,_]",
        "[_,_,1,_]",
        "[_,_,_,1]"
    };

    for (final String anEtalon : etalon) {
      assertNotNull(testgoal.prove());
      assertEquals(anEtalon, getVarAsText(testgoal, "X"));
    }
    assertNull(testgoal.prove());
  }

  @Test
  void testRomeNumbers() {
    final JProlContext context = makeContextAndConsult("rome(1,'I'). rome(2,'II'). rome(3,'III'). rome(4,'IV'). rome(5,'V'). rome(6,'VI'). rome(7,'VII'). rome(8,'VIII'). rome(9,'IX'). rome(_,'_'). romenum([],[]). romenum([X|Xt],[S|St]):-rome(X,S), romenum(Xt,St).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("romenum([1,2,3],X).", context);

    final String[] etalon = new String[] {
        "['I','II','III']",
        "['I','II','_']",
        "['I','_','III']",
        "['I','_','_']",
        "['_','II','III']",
        "['_','II','_']",
        "['_','_','III']",
        "['_','_','_']"
    };

    for (final String anEtalon : etalon) {
      assertNotNull(testgoal.prove());
      assertEquals(anEtalon, getVarAsText(testgoal, "X"));
    }
    assertNull(testgoal.prove());
  }

  @Test
  void testPrime() {
    final JProlContext context = makeContextAndConsult("is_prime2(N,1):-!.is_prime2(N,X) :- N =\\= (N // X)*X, X1 is X-1,is_prime2(N,X1).is_prime(1):-!,fail.is_prime(N):-N1 is N-1, is_prime2(N,N1),!.prime_interval(_,N,[]):-N<2,!.prime_interval(S,N,[]):-S>=N,!.prime_interval(S,N,L):- S<N,S1 is S+1,(is_prime(S),L=[S|Tail],prime_interval(S1,N,Tail);prime_interval(S1,N,L)),!.prime_list(N,L) :- prime_interval(1,N,L).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("prime_list(100,X).", context);

    assertNotNull(testgoal.prove());
    assertEquals("[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]", getVarAsText(testgoal, "X"));
    assertNull(testgoal.prove());
  }

  @Test
  void testFillNatural() {
    final JProlContext context = makeContextAndConsult("fill_list(N,N,[N]).fill_list(S,N,[S|S1t]):-S<N,S1 is S+1,fill_list(S1,N,S1t).fill_natural(N,L):-fill_list(1,N,L).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("fill_natural(10,L).", context);

    assertNotNull(testgoal.prove());
    assertEquals("[1,2,3,4,5,6,7,8,9,10]", getVarAsText(testgoal, "L"));
    assertNull(testgoal.prove());
  }

  @Test
  void testShift() {
    final JProlContext context = makeContextAndConsult("appendlast([],X,[X]):-!.appendlast([X|Xt],X1,[X|Yt]):-appendlast(Xt,X1,Yt).removelast([X],X,[]):-!.removelast([X|Xt],L,[X|Yt]):-removelast(Xt,L,Yt).rcshift([],[]):-!.rcshift(S,[X|D]) :- removelast(S,X,D).lcshift([],[]):-!.lcshift([X|Xt],R):-appendlast(Xt,X,R).");
    JProlChoicePoint testgoal = new JProlChoicePoint("rcshift(X,[1,2,3,4,5,6,7]).", context);

    assertNotNull(testgoal.prove());
    assertEquals("[2,3,4,5,6,7,1]", getVarAsText(testgoal, "X"));
    assertNull(testgoal.prove());

    testgoal = new JProlChoicePoint("lcshift(X,[1,2,3,4,5,6,7]).", context);

    assertNotNull(testgoal.prove());
    assertEquals("[7,1,2,3,4,5,6]", getVarAsText(testgoal, "X"));
    assertNull(testgoal.prove());
  }

  @Test
  void testSublist() {
    final JProlContext context = makeContextAndConsult("sub2([],_,_,[]). sub2([X|Xt],SN,N,Y):-SN<N,SN1 is SN+1,sub2(Xt,SN1,N,Y). sub2([X|Xt],SN,N,[X|Yt]):-SN>=N,SN1 is SN+1,sub2(Xt,SN1,N,Yt). sublist(S,P,R):-sub2(S,0,P,R).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("sublist([a,b,c,d,e],2,X).", context);

    assertNotNull(testgoal.prove());
    assertEquals("['c','d','e']", getVarAsText(testgoal, "X"));
    assertNull(testgoal.prove());
  }

  @Test
  void testReplace() {
    final JProlContext context = makeContextAndConsult("replace([],_,_,[]):-!. replace([X],X,N,[N]):-!. replace([X|St],X,N,[N|Lt]):-replace(St,X,N,Lt),!. replace([Y|St],X,N,[Y|Lt]):-replace(St,X,N,Lt).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("replace([a,b,c,d,e,f,g],e,1,X).", context);

    assertNotNull(testgoal.prove());
    assertEquals("['a','b','c','d',1,'f','g']", getVarAsText(testgoal, "X"));
    assertNull(testgoal.prove());
  }

  @Test
  void testBubble() {
    final JProlContext context = makeContextAndConsult("reverse([X,Y|L],[Y,X|L]):-X>Y, !. reverse([X|L],[X|LRes]):-reverse(L,LRes). bubble(L,Res):-reverse(L,Res1), !, bubble(Res1,Res). bubble(L,L).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("bubble([3,1,99,3,7,5,20],X).", context);

    assertNotNull(testgoal.prove());
    assertEquals("[1,3,3,5,7,20,99]", getVarAsText(testgoal, "X"));
    assertNull(testgoal.prove());
  }

  @Test
  void testReverse() {
    final JProlContext context = makeContextAndConsult(
        "reverse([], []). reverse([A|B], Z) :- reverse(B, Brev), append(Brev, [A], Z).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("reverse([1,2,3,4,5,6],X).", context);

    assertNotNull(testgoal.prove());
    assertEquals("[6,5,4,3,2,1]", getVarAsText(testgoal, "X"));
    assertNull(testgoal.prove());
  }

  @Test
  void testAppend() {
    final JProlContext context = makeContextAndConsult("");
    final JProlChoicePoint testGoal = new JProlChoicePoint("append(X,Y,[1,2,3,4,5,6]).", context);

    final String[] expectedValues = new String[] {
        "[]", "[1,2,3,4,5,6]",
        "[1]", "[2,3,4,5,6]",
        "[1,2]", "[3,4,5,6]",
        "[1,2,3]", "[4,5,6]",
        "[1,2,3,4]", "[5,6]",
        "[1,2,3,4,5]", "[6]",
        "[1,2,3,4,5,6]", "[]"
    };

    for (int li = 0; li < expectedValues.length; li += 2) {
      assertNotNull(testGoal.prove());
      assertEquals(expectedValues[li], getVarAsText(testGoal, "X"));
      assertEquals(expectedValues[li + 1], getVarAsText(testGoal, "Y"));
    }
    assertNull(testGoal.prove());

    JProlChoicePoint cp = new JProlChoicePoint("append([a,b], [c], X).", context);
    assertNotNull(cp.prove());
    assertEquals("['a','b','c']", getVarAsText(cp, "X"));
    assertNull(cp.prove());

    cp = new JProlChoicePoint("append(X, [Last], [a,b,c]).", context);
    assertNotNull(cp.prove());
    assertEquals("['a','b']", getVarAsText(cp, "X"));
    assertEquals("'c'", getVarAsText(cp, "Last"));
    assertNull(cp.prove());

    cp = new JProlChoicePoint("append([a,b], More, List).", context);
    assertNotNull(cp.prove());
    assertEquals("['a','b'|Z]", getVarAsText(cp, "List"));
    assertNull(cp.prove());

  }

  @Test
  void testTakeListFromBase1() {
    final JProlContext context = makeContextAndConsult("a(X):-X=[_,_,_,4].b(X):-X=[1,_,_,_].c(X):-X=[_,2,_,_].d(X):-X=[_,_,3,_].test(X):-X=A,A=B,B=C,C=D,a(A),b(B),c(C),d(D).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("test(R).", context);

    assertNotNull(testgoal.prove());
    assertEquals(getVarAsText(testgoal, "R"), "[1,2,3,4]");
    assertNull(testgoal.prove());
  }

  @Test
  void testTakeListFromBase2() {
    final JProlContext context = makeContextAndConsult("a([1,2,3,4,5]).a([a,b,c,d,e]).a([cafebabe]).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("a(X).", context);

    final String[] etalon = new String[] {"[1,2,3,4,5]", "['a','b','c','d','e']", "['cafebabe']"};
    for (final String anEtalon : etalon) {
      final Term nextTerm = testgoal.prove();
      assertNotNull(nextTerm);
      assertEquals(anEtalon, getVarAsText(testgoal, "X"));
    }

    assertNull(testgoal.prove());
  }

  @Test
  void testAtList() {
    final JProlContext context = makeContextAndConsult("atlist([],X):-fail,!. atlist([X],X):-!. atlist([X|Tail],X). atlist([X|Tail],Y):-atlist(Tail,Y).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("atlist([0,1,2,3,4,5,6,7,8,9,10],X).", context);

    int etalon = 0;
    while (true) {
      final Term result = testgoal.prove();
      if (result == null) {
        break;
      }

      assertEquals(etalon, getVarAsNumber(testgoal, "X").intValue());

      etalon++;
    }
    assertEquals(etalon, 11);
  }

  @Test
  void testAnonymVariables() {
    final JProlContext context = makeContextAndConsult("a([a,_,_,d]). a([_,b,c,_]). test(X):-X=[1,_,_,4],a(X).");
    final JProlChoicePoint testgoal = new JProlChoicePoint("test(X).", context);

    assertNotNull(testgoal.prove());
    assertEquals("[1,'b','c',4]", getVarAsText(testgoal, "X"));
    assertNull(testgoal.prove());
  }

}
