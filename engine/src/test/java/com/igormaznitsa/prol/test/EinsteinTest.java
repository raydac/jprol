package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.trace.TraceListener;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import junit.framework.TestCase;
import org.junit.Test;

public class EinsteinTest extends TestCase implements TraceListener {

  @Test
  public void testEinstein() throws Throwable {
    final ProlContext context = makeContext("next_to(X,Y,List) :- iright(X,Y,List)."
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

    final Goal goal = new Goal("einstein(_,X).", context);
    assertNotNull(goal.solve());
    assertEquals(goal.getVarAsText("X"), "'german'");
    assertNull(goal.solve());
  }

  private ProlContext makeContext(final String knowledgeBase) throws Exception {
    final ProlContext context = new ProlContext("PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult(knowledgeBase, context);
    consult.consult();

    // check that kb is right in its data export
    final ByteArrayOutputStream baos = new ByteArrayOutputStream(1024);
    final PrintWriter out = new PrintWriter(baos);
    context.getKnowledgeBase().write(out);
    out.flush();
    out.close();

    //System.out.println(new String(baos.toByteArray()));
    final ProlContext context1 = new ProlContext("PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult1 = new ProlConsult(new ByteArrayInputStream(baos.toByteArray()), context1);
    consult1.consult();

    return context1;
  }

  @Override
  public boolean onProlGoalCall(Goal goal) {
    if (allowedPredicate(goal)) {
      System.out.println("Call: " + goal.getGoalTerm());
    }
    return true;
  }

  @Override
  public boolean onProlGoalRedo(Goal goal) {
    if (allowedPredicate(goal)) {
      System.out.println("Redo: " + goal.getGoalTerm());
    }
    return true;
  }

  @Override
  public void onProlGoalFail(Goal goal) {
    if (allowedPredicate(goal)) {
      System.out.println("Fail: " + goal.getGoalTerm());
    }
  }

  @Override
  public void onProlGoalExit(Goal goal) {
    if (allowedPredicate(goal)) {
      System.out.println("Exit: " + goal.getGoalTerm());
    }
  }

  private boolean allowedPredicate(Goal goal) {
    final String signature = goal.getGoalTerm().getSignature();
    return !(signature.equals(",/2") || signature.equals(";/2"));
  }
}
