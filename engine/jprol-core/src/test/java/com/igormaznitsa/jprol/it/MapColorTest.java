package com.igormaznitsa.jprol.it;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class MapColorTest extends AbstractJProlTest {

  @Test
  void testMapColor() {
    final JProlContext context = makeContextAndConsult("colour_countries(Colours):-\n"
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

    final JProlChoicePoint goal = new JProlChoicePoint("colour_countries(X).", context);
    int counter = 0;
    Term result0 = null;
    Term result7775 = null;
    while (goal.prove() != null) {
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
}
