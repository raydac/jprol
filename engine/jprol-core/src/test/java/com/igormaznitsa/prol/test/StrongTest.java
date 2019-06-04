package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import static org.junit.Assert.*;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

public class StrongTest extends AbstractProlTest {

    @Test
    public void testFlightRoutePlanner() throws Throwable {
        final ProlContext context = makeContext(":-op(50,xfy,:)."
                + "route(P1,P2,Day,[P1/P2/Fnum/Deptime]):-flight(P1,P2,Day,Fnum,Deptime,_)."
                + "route(P1,P2,Day,[(P1/P3/Fnum1/Dep1)|RestRoute]):-route(P3,P2,Day,RestRoute),flight(P1,P3,Day,Fnum1,Dep1,Arr1),deptime(RestRoute,Dep2),transfer(Arr1,Dep2)."
                + "flight(Place1,Place2,Day,Fnum,Deptime,Arrtime):-timetable(Place1,Place2,Flightlist),member(Deptime/Arrtime/Fnum/Daylist,Flightlist),flyday(Day,Daylist)."
                + "flyday(Day,Daylist):-member(Day,Daylist).flyday(Day,alldays):-member(Day,[mo,tu,we,th,fr,sa,su])."
                + "deptime([_/_/_/Dep|_],Dep)."
                + "transfer(Hours1:Mins1,Hours2:Mins2):-60*(Hours2-Hours1)+Mins2-Mins1>=40."
                + "member(X,[X|_]).member(X,[_|L]):-member(X,L)."
                + "conc([],L,L).conc([X|L1],L2,[X|L3]):-conc(L1,L2,L3)."
                + "permutation([],[]).permutation(L,[X|P]):-del( X,L,L1),permutation(L1,P)."
                + "del(X,[X|L],L).del(X,[Y|L],[Y|L1]):-del(X,L,L1)."
                + "timetable(edinburgh,london,[9:40/10:50/ba4733/alldays,13:40/14:50/ba4773/alldays,19:40/20:50/ba4833/[mo,tu,we,th,fr,su]])."
                + "timetable(london,edinburgh,[9:40/10:50/ba4732/alldays,11:40/12:50/ba4752/alldays,18:40/19:50/ba4822 /[mo,tu,we,th,fr]])."
                + "timetable(london,ljubljana,[13:20/16:20/jp212/[mo,tu,we,fr,su],16:30/19:30/ba473/[mo,we,th,sa]])."
                + "timetable(london,zurich,[9:10/11:45/ba614/alldays,14:45/17:20/sr805/alldays])."
                + "timetable(london,milan,[8:30/11:20/ba510/alldays,11:00/13:50/az459/alldays])."
                + "timetable(ljubljana,zurich,[11:30/12:40/jp322/[tu,th]])."
                + "timetable(ljubljana,london,[11:10/12:20/jp211/[mo,tu,we,fr,su],20:30/21:30/ba472/[mo,we,th,sa] ])."
                + "timetable(milan,london,[9:10/10:00/az458/alldays,12:20/13:10/ba511/alldays])."
                + "timetable(milan,zurich,[9:25/10:15/sr621/alldays,12:45/13:35/sr623/alldays])."
                + "timetable( zurich,ljubljana,[13:30/14:40/jp323/[tu,th]])."
                + "timetable( zurich,london,[9:00/9:40/ba613/[mo,tu,we,th,fr,sa],16:10/16:55/sr806/[mo,tu,we,th,fr,su]])."
                + "timetable(zurich,milan,[7:55/8:45/sr620/alldays])."
                + "query3(City1,City2,City3,FN1,FN2,FN3,FN4):-permutation([milan,ljubljana,zurich],[City1,City2,City3]),flight(london,City1,tu,FN1,_,_),flight(City1,City2,we,FN2,_,_),flight(City2,City3,th,FN3,_,_),flight(City3,london,fr,FN4,_,_).");

        final String[] test1etal = new String[]{
                "['milan','zurich','ljubljana','ba510','sr621','jp323','jp211']",
                "['milan','zurich','ljubljana','ba510','sr623','jp323','jp211']",
                "['milan','zurich','ljubljana','az459','sr621','jp323','jp211']",
                "['milan','zurich','ljubljana','az459','sr623','jp323','jp211']"
        };

        Goal goal = new Goal("M=[A,B,C,D,E,F,G],query3(A,B,C,D,E,F,G).", context);

        for (final String c : test1etal) {
            assertNotNull(goal.solve());
            assertEquals(goal.getVarAsText("M"), c);
        }
        assertNull(goal.solve());

        goal = new Goal("route(ljubljana,edinburgh,th,R).", context);
        assertNotNull(goal.solve());
        assertEquals(goal.getVarAsText("R"), "['ljubljana' / 'zurich' / 'jp322' / 11 : 30,'zurich' / 'london' / 'sr806' / 16 : 10,'london' / 'edinburgh' / 'ba4822' / 18 : 40]");

    }

    @Test
    public void testColoring() throws Throwable {
        final ProlContext context = makeContext("member(X,[X|L]).member(X,[Y|L]):-member(X,L).adjacent(X,Y,Map):-member([X,Y],Map);member([Y,X],Map)."
                + "find_regions([],R,R).find_regions([[X,Y]|S],R,A):-(member(X,R)->(member(Y,R)->find_regions(S,R,A);find_regions(S,[Y|R],A));(member(Y,R)->find_regions(S,[X|R],A);find_regions(S,[X,Y|R],A)))."
                + "color(Map,Colors,Coloring) :- find_regions(Map,[],Regions), color_all(Regions,Colors,Coloring), \\+ conflict(Map,Coloring)."
                + "color_all([R|Rs],Colors,[[R,C]|A]):-member(C,Colors),color_all(Rs,Colors,A).color_all([],_,[]).conflict(Map,Coloring):-member([R1,C],Coloring), member([R2,C],Coloring), adjacent(R1,R2,Map).");

        final String[] etal = new String[]{
                "[[5,'red'],[4,'green'],[3,'red'],[1,'blue'],[2,'yellow']]",
                "[[5,'red'],[4,'green'],[3,'red'],[1,'yellow'],[2,'blue']]",
                "[[5,'red'],[4,'green'],[3,'blue'],[1,'yellow'],[2,'red']]",
                "[[5,'red'],[4,'green'],[3,'yellow'],[1,'blue'],[2,'red']]",
                "[[5,'red'],[4,'blue'],[3,'red'],[1,'green'],[2,'yellow']]",
                "[[5,'red'],[4,'blue'],[3,'red'],[1,'yellow'],[2,'green']]",
                "[[5,'red'],[4,'blue'],[3,'green'],[1,'yellow'],[2,'red']]",
                "[[5,'red'],[4,'blue'],[3,'yellow'],[1,'green'],[2,'red']]",
                "[[5,'red'],[4,'yellow'],[3,'red'],[1,'green'],[2,'blue']]",
                "[[5,'red'],[4,'yellow'],[3,'red'],[1,'blue'],[2,'green']]",
                "[[5,'red'],[4,'yellow'],[3,'green'],[1,'blue'],[2,'red']]",
                "[[5,'red'],[4,'yellow'],[3,'blue'],[1,'green'],[2,'red']]",
                "[[5,'green'],[4,'red'],[3,'green'],[1,'blue'],[2,'yellow']]",
                "[[5,'green'],[4,'red'],[3,'green'],[1,'yellow'],[2,'blue']]",
                "[[5,'green'],[4,'red'],[3,'blue'],[1,'yellow'],[2,'green']]",
                "[[5,'green'],[4,'red'],[3,'yellow'],[1,'blue'],[2,'green']]",
                "[[5,'green'],[4,'blue'],[3,'red'],[1,'yellow'],[2,'green']]",
                "[[5,'green'],[4,'blue'],[3,'green'],[1,'red'],[2,'yellow']]",
                "[[5,'green'],[4,'blue'],[3,'green'],[1,'yellow'],[2,'red']]",
                "[[5,'green'],[4,'blue'],[3,'yellow'],[1,'red'],[2,'green']]",
                "[[5,'green'],[4,'yellow'],[3,'red'],[1,'blue'],[2,'green']]",
                "[[5,'green'],[4,'yellow'],[3,'green'],[1,'red'],[2,'blue']]",
                "[[5,'green'],[4,'yellow'],[3,'green'],[1,'blue'],[2,'red']]",
                "[[5,'green'],[4,'yellow'],[3,'blue'],[1,'red'],[2,'green']]",
                "[[5,'blue'],[4,'red'],[3,'green'],[1,'yellow'],[2,'blue']]",
                "[[5,'blue'],[4,'red'],[3,'blue'],[1,'green'],[2,'yellow']]",
                "[[5,'blue'],[4,'red'],[3,'blue'],[1,'yellow'],[2,'green']]",
                "[[5,'blue'],[4,'red'],[3,'yellow'],[1,'green'],[2,'blue']]",
                "[[5,'blue'],[4,'green'],[3,'red'],[1,'yellow'],[2,'blue']]",
                "[[5,'blue'],[4,'green'],[3,'blue'],[1,'red'],[2,'yellow']]",
                "[[5,'blue'],[4,'green'],[3,'blue'],[1,'yellow'],[2,'red']]",
                "[[5,'blue'],[4,'green'],[3,'yellow'],[1,'red'],[2,'blue']]",
                "[[5,'blue'],[4,'yellow'],[3,'red'],[1,'green'],[2,'blue']]",
                "[[5,'blue'],[4,'yellow'],[3,'green'],[1,'red'],[2,'blue']]",
                "[[5,'blue'],[4,'yellow'],[3,'blue'],[1,'red'],[2,'green']]",
                "[[5,'blue'],[4,'yellow'],[3,'blue'],[1,'green'],[2,'red']]",
                "[[5,'yellow'],[4,'red'],[3,'green'],[1,'blue'],[2,'yellow']]",
                "[[5,'yellow'],[4,'red'],[3,'blue'],[1,'green'],[2,'yellow']]",
                "[[5,'yellow'],[4,'red'],[3,'yellow'],[1,'green'],[2,'blue']]",
                "[[5,'yellow'],[4,'red'],[3,'yellow'],[1,'blue'],[2,'green']]",
                "[[5,'yellow'],[4,'green'],[3,'red'],[1,'blue'],[2,'yellow']]",
                "[[5,'yellow'],[4,'green'],[3,'blue'],[1,'red'],[2,'yellow']]",
                "[[5,'yellow'],[4,'green'],[3,'yellow'],[1,'red'],[2,'blue']]",
                "[[5,'yellow'],[4,'green'],[3,'yellow'],[1,'blue'],[2,'red']]",
                "[[5,'yellow'],[4,'blue'],[3,'red'],[1,'green'],[2,'yellow']]",
                "[[5,'yellow'],[4,'blue'],[3,'green'],[1,'red'],[2,'yellow']]",
                "[[5,'yellow'],[4,'blue'],[3,'yellow'],[1,'red'],[2,'green']]",
                "[[5,'yellow'],[4,'blue'],[3,'yellow'],[1,'green'],[2,'red']]"};

        final Goal goal = new Goal("color([[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[3,4],[4,5]],[red,green,blue,yellow],Coloring).", context);
        for (final String e : etal) {
            assertNotNull(goal.solve());
            assertEquals(goal.getVarAsText("Coloring"), e);
        }
        assertNull(goal.solve());

    }

    private ProlContext makeContext(final String knowledgeBase) throws Exception {
        final ProlContext context = new ProlContext(this, "PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult = new ProlConsult(knowledgeBase, context);
        consult.consult();

        // check that kb is right in its data export
        final ByteArrayOutputStream baos = new ByteArrayOutputStream(1024);
        final PrintWriter out = new PrintWriter(baos);
        context.getKnowledgeBase().write(out);
        out.flush();
        out.close();

        //System.out.println(new String(baos.toByteArray()));
        final ProlContext context1 = new ProlContext(this, "PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
        final ProlConsult consult1 = new ProlConsult(new ByteArrayInputStream(baos.toByteArray()), context1);
        consult1.consult();

        return context1.makeCopy(); // and check makeCopy() for the context

    }
}
