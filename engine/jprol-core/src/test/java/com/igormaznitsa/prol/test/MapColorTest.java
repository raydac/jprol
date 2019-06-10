package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

import static org.junit.Assert.assertEquals;

public class MapColorTest extends AbstractProlTest {

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

    final ChoicePoint goal = new ChoicePoint("colour_countries(X).", context);
    int counter = 0;
    Term result0 = null;
    Term result7775 = null;
    while (goal.next() != null) {
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
    final ProlContext context = new ProlContext("PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult(knowledgeBase, context);
    consult.consult();

    // check that kb is right in its data export
    final ByteArrayOutputStream baos = new ByteArrayOutputStream(1024);
      try (PrintWriter out = new PrintWriter(baos)) {
          context.getKnowledgeBase().write(out);
          out.flush();
      }

    //System.out.println(new String(baos.toByteArray()));
    final ProlContext context1 = new ProlContext("PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult1 = new ProlConsult(new ByteArrayInputStream(baos.toByteArray()), context1);
    consult1.consult();

    return context1;
  }

  private boolean allowedPredicateInGoal(final ChoicePoint goal) {
    final String signature = goal.getGoalTerm().getSignature();
    return !(signature.equals(",/2") || signature.equals(";/2"));
  }
}
