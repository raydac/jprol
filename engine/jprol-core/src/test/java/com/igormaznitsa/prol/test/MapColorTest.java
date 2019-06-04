package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.trace.TraceListener;
import static org.junit.Assert.*;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

public class MapColorTest extends AbstractProlTest implements TraceListener {

  @Test
  public void testMapColor() throws Throwable {
    final ProlContext context = makeContext("colour_countries(Colours):-\n"
            + "        setof(Country/_, X^ngb(Country,X), Colours),\n"
            + "        colours(Colours).\n"
            + "colours([]).\n"
            + "colours([Country/Colour|Rest]):-\n"
            + "        colours(Rest),\n"
            + "        member(Colour, [yellow,blue,red,green]),\n"
            + "        \\+ (member(Country1/Colour, Rest), neighbour(Country, Country1)).\n"
            + "neighbour(Country, Country1):-\n"
            + "        ngb(Country, Neighbours),\n"
            + "        member(Country1, Neighbours).\n"
            + "ngb(portugal, [spain]).\n"
            + "ngb(spain, [portugal,france]).\n"
            + "ngb(france, [spain,belgium,switzerland,w_germany,italy]).\n"
            + "ngb(belgium, [france,w_germany,netherlands]).\n"
            + "ngb(netherlands, [belgium,w_germany]).\n"
            + "ngb(w_germany, [netherlands,belgium,france,switzerland,austria,denmark]).\n"
            + "ngb(switzerland, [france,w_germany,austria,italy]).\n"
            + "ngb(austria, [w_germany,switzerland,italy]).\n"
            + "ngb(italy, [france,switzerland,austria]).\n"
            + "ngb(denmark, [w_germany]).\n"
            + "member(X, [X|_]).\n"
            + "member(X, [_|T]):-\n"
            + "        member(X, T).");

    final Goal goal = new Goal("colour_countries(X).", context);
    int counter = 0;
    Term result0 = null;
    Term result7775 = null;
    while (goal.solve() != null) {
      if (counter == 0) {
        result0 = goal.getVarForName("X").getValue().makeClone();
      } else if (counter == 7775) {
        result7775 = goal.getVarForName("X").getValue().makeClone();
      }
      counter++;
    }
    assertEquals(7776, counter);
    assertEquals("[austria / red,belgium / green,denmark / blue,france / red,italy / yellow,netherlands / blue,portugal / blue,spain / yellow,switzerland / blue,w_germany / yellow]", result0.forWrite());
    assertEquals("[austria / blue,belgium / yellow,denmark / red,france / blue,italy / green,netherlands / red,portugal / red,spain / green,switzerland / red,w_germany / green]", result7775.forWrite());
  }

  private ProlContext makeContext(final String knowledgeBase) throws Exception {
    final ProlContext context = new ProlContext(this, "PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult(knowledgeBase, context);
    consult.consult();

    // check that kb is right in its data export
    final ByteArrayOutputStream baos = new ByteArrayOutputStream(1024);
      try (PrintWriter out = new PrintWriter(baos)) {
          context.getKnowledgeBase().write(out);
          out.flush();
      }

    //System.out.println(new String(baos.toByteArray()));
    final ProlContext context1 = new ProlContext(this, "PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult1 = new ProlConsult(new ByteArrayInputStream(baos.toByteArray()), context1);
    consult1.consult();

    return context1;
  }

  @Override
  public boolean onProlGoalCall(final Goal goal) {
    if (allowedPredicateInGoal(goal)) {
      System.out.println("Call: " + goal.getGoalTerm());
    }
    return true;
  }

  @Override
  public boolean onProlGoalRedo(final Goal goal) {
    if (allowedPredicateInGoal(goal)) {
      System.out.println("Redo: " + goal.getGoalTerm());
    }
    return true;
  }

  @Override
  public void onProlGoalFail(final Goal goal) {
    if (allowedPredicateInGoal(goal)) {
      System.out.println("Fail: " + goal.getGoalTerm());
    }
  }

  @Override
  public void onProlGoalExit(final Goal goal) {
    if (allowedPredicateInGoal(goal)) {
      System.out.println("Exit: " + goal.getGoalTerm());
    }
  }

  private boolean allowedPredicateInGoal(final Goal goal) {
    final String signature = goal.getGoalTerm().getSignature();
    return !(signature.equals(",/2") || signature.equals(";/2"));
  }
}
