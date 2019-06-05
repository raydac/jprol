package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import static org.junit.Assert.*;
import org.junit.Test;

public class ListTest extends AbstractProlTest {

    @Test
    public void testRunningOne() throws Throwable {
        final ProlContext context = makeContext("member2(E,[E|_]). member2(E,[_|L]) :- member2(E,L). runningOne(X):-X=[_,_,_,_],member2(1,X).");
        final Goal testgoal = new Goal("runningOne(X).", context);
        final String[] etalon = new String[]{
                "[1,_,_,_]",
                "[_,1,_,_]",
                "[_,_,1,_]",
                "[_,_,_,1]"
        };

        for (final String anEtalon : etalon) {
            assertNotNull(testgoal.solve());
            assertEquals(anEtalon, testgoal.getVarAsText("X"));
        }
        assertNull(testgoal.solve());
    }

    @Test
    public void testRomenum() throws Throwable {
        final ProlContext context = makeContext("rome(1,'I'). rome(2,'II'). rome(3,'III'). rome(4,'IV'). rome(5,'V'). rome(6,'VI'). rome(7,'VII'). rome(8,'VIII'). rome(9,'IX'). rome(_,'_'). romenum([],[]). romenum([X|Xt],[S|St]):-rome(X,S), romenum(Xt,St).");
        final Goal testgoal = new Goal("romenum([1,2,3],X).", context);

        final String[] etalon = new String[]{
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
            assertNotNull(testgoal.solve());
            assertEquals(anEtalon, testgoal.getVarAsText("X"));
        }
        assertNull(testgoal.solve());
    }

    @Test
    public void testPrime() throws Throwable {
        final ProlContext context = makeContext("is_prime2(N,1):-!.is_prime2(N,X) :- N =\\= (N // X)*X, X1 is X-1,is_prime2(N,X1).is_prime(1):-!,fail.is_prime(N):-N1 is N-1, is_prime2(N,N1),!.prime_interval(_,N,[]):-N<2,!.prime_interval(S,N,[]):-S>=N,!.prime_interval(S,N,L):- S<N,S1 is S+1,(is_prime(S),L=[S|Tail],prime_interval(S1,N,Tail);prime_interval(S1,N,L)),!.prime_list(N,L) :- prime_interval(1,N,L).");
        final Goal testgoal = new Goal("prime_list(100,X).", context);

        assertNotNull(testgoal.solve());
        assertEquals("[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]", testgoal.getVarAsText("X"));
        assertNull(testgoal.solve());
    }

    @Test
    public void testFillNatural() throws Throwable {
        final ProlContext context = makeContext("fill_list(N,N,[N]).fill_list(S,N,[S|S1t]):-S<N,S1 is S+1,fill_list(S1,N,S1t).fill_natural(N,L):-fill_list(1,N,L).");
        final Goal testgoal = new Goal("fill_natural(10,L).", context);

        assertNotNull(testgoal.solve());
        assertEquals("[1,2,3,4,5,6,7,8,9,10]", testgoal.getVarAsText("L"));
        assertNull(testgoal.solve());
    }

    @Test
    public void testShift() throws Throwable {
        final ProlContext context = makeContext("appendlast([],X,[X]):-!.appendlast([X|Xt],X1,[X|Yt]):-appendlast(Xt,X1,Yt).removelast([X],X,[]):-!.removelast([X|Xt],L,[X|Yt]):-removelast(Xt,L,Yt).rcshift([],[]):-!.rcshift(S,[X|D]) :- removelast(S,X,D).lcshift([],[]):-!.lcshift([X|Xt],R):-appendlast(Xt,X,R).");
        Goal testgoal = new Goal("rcshift(X,[1,2,3,4,5,6,7]).", context);

        assertNotNull(testgoal.solve());
        assertEquals("[2,3,4,5,6,7,1]", testgoal.getVarAsText("X"));
        assertNull(testgoal.solve());

        testgoal = new Goal("lcshift(X,[1,2,3,4,5,6,7]).", context);

        assertNotNull(testgoal.solve());
        assertEquals("[7,1,2,3,4,5,6]", testgoal.getVarAsText("X"));
        assertNull(testgoal.solve());
    }

    @Test
    public void testSublist() throws Throwable {
        final ProlContext context = makeContext("sub2([],_,_,[]). sub2([X|Xt],SN,N,Y):-SN<N,SN1 is SN+1,sub2(Xt,SN1,N,Y). sub2([X|Xt],SN,N,[X|Yt]):-SN>=N,SN1 is SN+1,sub2(Xt,SN1,N,Yt). sublist(S,P,R):-sub2(S,0,P,R).");
        final Goal testgoal = new Goal("sublist([a,b,c,d,e],2,X).", context);

        assertNotNull(testgoal.solve());
        assertEquals("['c','d','e']", testgoal.getVarAsText("X"));
        assertNull(testgoal.solve());
    }

    @Test
    public void testReplace() throws Throwable {
        final ProlContext context = makeContext("replace([],_,_,[]):-!. replace([X],X,N,[N]):-!. replace([X|St],X,N,[N|Lt]):-replace(St,X,N,Lt),!. replace([Y|St],X,N,[Y|Lt]):-replace(St,X,N,Lt).");
        final Goal testgoal = new Goal("replace([a,b,c,d,e,f,g],e,1,X).", context);

        assertNotNull(testgoal.solve());
        assertEquals("['a','b','c','d',1,'f','g']", testgoal.getVarAsText("X"));
        assertNull(testgoal.solve());
    }

    @Test
    public void testBubble() throws Throwable {
        final ProlContext context = makeContext("reverse([X,Y|L],[Y,X|L]):-X>Y, !. reverse([X|L],[X|LRes]):-reverse(L,LRes). bubble(L,Res):-reverse(L,Res1), !, bubble(Res1,Res). bubble(L,L).");
        final Goal testgoal = new Goal("bubble([3,1,99,3,7,5,20],X).", context);

        assertNotNull(testgoal.solve());
        assertEquals("[1,3,3,5,7,20,99]", testgoal.getVarAsText("X"));
        assertNull(testgoal.solve());
    }

    @Test
    public void testReverse() throws Throwable {
        final ProlContext context = makeContext("reverse([], []). reverse([A|B], Z) :- reverse(B, Brev), append(Brev, [A], Z). append([], Z, Z).append([A|B], Z, [A|Z2]) :- append(B, Z, Z2).");
        final Goal testgoal = new Goal("reverse([1,2,3,4,5,6],X).", context);

        assertNotNull(testgoal.solve());
        assertEquals("[6,5,4,3,2,1]", testgoal.getVarAsText("X"));
        assertNull(testgoal.solve());
    }

    @Test
    public void testAppend() throws Throwable {
        final ProlContext context = makeContext("append([], Z, Z).append([A|B], Z, [A|Z2]) :- append(B, Z, Z2).");
        final Goal testgoal = new Goal("append(X,Y,[1,2,3,4,5,6]).", context);

        final String[] etalon = new String[]{
                "[]", "[1,2,3,4,5,6]",
                "[1]", "[2,3,4,5,6]",
                "[1,2]", "[3,4,5,6]",
                "[1,2,3]", "[4,5,6]",
                "[1,2,3,4]", "[5,6]",
                "[1,2,3,4,5]", "[6]",
                "[1,2,3,4,5,6]", "[]"
        };

        for (int li = 0; li < etalon.length; li += 2) {
            assertNotNull(testgoal.solve());
            assertEquals(etalon[li], testgoal.getVarAsText("X"));
            assertEquals(etalon[li + 1], testgoal.getVarAsText("Y"));
        }
        assertNull(testgoal.solve());

    }

    @Test
    public void testTakeListFromBase1() throws Throwable {
        final ProlContext context = makeContext("a(X):-X=[_,_,_,4].b(X):-X=[1,_,_,_].c(X):-X=[_,2,_,_].d(X):-X=[_,_,3,_].test(X):-X=A,A=B,B=C,C=D,a(A),b(B),c(C),d(D).");
        final Goal testgoal = new Goal("test(R).", context);

        assertNotNull(testgoal.solve());
        assertEquals(testgoal.getVarAsText("R"), "[1,2,3,4]");
        assertNull(testgoal.solve());
    }

    @Test
    public void testTakeListFromBase2() throws Throwable {
        final ProlContext context = makeContext("a([1,2,3,4,5]).a([a,b,c,d,e]).a([cafebabe]).");
        final Goal testgoal = new Goal("a(X).", context);

        final String[] etalon = new String[]{"[1,2,3,4,5]", "['a','b','c','d','e']", "['cafebabe']"};
        for (final String anEtalon : etalon) {
            final Term nextTerm = testgoal.solve();
            assertNotNull(nextTerm);
            assertEquals(anEtalon, testgoal.getVarAsText("X"));
        }

        assertNull(testgoal.solve());
    }

    @Test
    public void testAtList() throws Throwable {
        final ProlContext context = makeContext("atlist([],X):-fail,!. atlist([X],X):-!. atlist([X|Tail],X). atlist([X|Tail],Y):-atlist(Tail,Y).");
        final Goal testgoal = new Goal("atlist([0,1,2,3,4,5,6,7,8,9,10],X).", context);

        int etalon = 0;
        while (true) {
            final Term result = testgoal.solve();
            if (result == null) {
                break;
            }

            assertEquals(etalon, testgoal.getVarAsNumber("X").intValue());

            etalon++;
        }
        assertEquals(etalon, 11);
    }

    @Test
    public void testAnonymVariables() throws Throwable {
        final ProlContext context = makeContext("a([a,_,_,d]). a([_,b,c,_]). test(X):-X=[1,_,_,4],a(X).");
        final Goal testgoal = new Goal("test(X).", context);

        assertNotNull(testgoal.solve());
        assertEquals(testgoal.getVarAsText("X"), "[1,'b','c',4]");
        assertNull(testgoal.solve());
    }

    private ProlContext makeContext(final String knowledgeBase) throws Exception {
        final ProlContext context = new ProlContext("PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult(knowledgeBase, context);
        consult.consult();
        return context;
    }
}
