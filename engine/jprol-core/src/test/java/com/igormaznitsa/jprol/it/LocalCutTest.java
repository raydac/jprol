package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;

public class LocalCutTest extends AbstractJProlTest {
  @Test
  void testLocalCut() {
    try (final JProlContext context = makeTestContext()) {

      context.consult(new StringReader(
          "a(1).a(3).a(5). c(7). c(11). b(X):- a(Y), !!, c(Z), X is Y * Z. b(777)."));
      final JProlChoicePoint choicePoint = context.makeChoicePoint("b(R).");
      final List<Long> results = new ArrayList<>();
      while (choicePoint.prove() != null) {
        final Term value = choicePoint.findVar("R").orElseThrow().tryGround();
        results.add(value.toNumber().longValue());
      }
      assertEquals(List.of(7L, 11L, 777L), results);
    }
  }

  @Test
  void testFullCut() {
    try (final JProlContext context = makeTestContext()) {

      context.consult(new StringReader(
          "a(1).a(3).a(5). c(7). c(11). b(X):- a(Y), !, c(Z), X is Y * Z. b(777)."));
      final JProlChoicePoint choicePoint = context.makeChoicePoint("b(R).");
      final List<Long> results = new ArrayList<>();
      while (choicePoint.prove() != null) {
        final Term value = choicePoint.findVar("R").orElseThrow().tryGround();
        results.add(value.toNumber().longValue());
      }
      assertEquals(List.of(7L, 11L), results);
    }
  }

}
