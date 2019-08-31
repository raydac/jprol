package com.igormaznitsa.jprol.it;

import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class EinsteinTest extends AbstractJProlTest {

  @Test
  void testEinstein() {
    final JProlContext context = makeContextAndConsult("next_to(X,Y,List) :- iright(X,Y,List)."
        + "next_to(X,Y,List) :- iright(Y,X,List)."
        + "einstein(Houses,Fish_Owner) :-"
        + "Houses=[ [house,norwegian,_,_,_,_] , _ , [house,_,_,_,milk,_] , _ , _ ],"
        + "member2([house,brit,_,_,_,red] , Houses),"
        + "member2([house,swede,dog,_,_,_] , Houses),"
        + "member2([house,dane,_,_,tea,_] , Houses),"
        + "iright([house,_,_,_,_,green] , [house,_,_,_,_,white], Houses),"
        + "member2([house,_,_,_,coffee,green] , Houses),"
        + "member2([house,_,bird,pallmall,_,_] , Houses),"
        + "member2([house,_,_,dunhill,_,yellow] , Houses),"
        + "next_to([house,_,_,dunhill,_,_] , [house,_,horse,_,_,_], Houses),"
        + "member2([house,_,_,_,milk,_] , Houses),"
        + "next_to([house,_,_,marlboro,_,_] , [house,_,cat,_,_,_], Houses),"
        + "next_to([house,_,_,marlboro,_,_] , [house,_,_,_,water,_], Houses),"
        + "member2([house,_,_,winfield,beer,_] , Houses),"
        + "member2([house,german,_,rothmans,_,_] , Houses),"
        + "next_to([house,norwegian,_,_,_,_] , [house,_,_,_,_,blue] , Houses),"
        + "member2([house, Fish_Owner , fish , _ , _ , _ ] , Houses)."
        + "iright(L,R,[L,R|_]). iright(L,R,[_|Rest]) :- iright(L,R,Rest)."
        + "member2(E,[E|_]). member2(E,[_|L]) :- member2(E,L).");

    final JProlChoicePoint goal = new JProlChoicePoint("einstein(_,X).", context);
    assertNotNull(goal.prove());
    assertEquals("'german'", getVarAsText(goal, "X"));
    assertNull(goal.prove());
  }

}
