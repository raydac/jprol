package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

class SudokuTest extends AbstractJProlTest {

  @Test
  void testSudoku() throws Exception {
    try (final JProlContext context = this.makeContextAndConsultFromResource("sudoku9x9.pl")) {
      final JProlChoicePoint choicePoint = new JProlChoicePoint("sudoku(" +
          "7, 3, 6,  8, 4, 9,  1, 5, A,"
          + "1, B, 4,  2, 7, C,  6, 8, 9,"
          + "D, 8, 9,  1, 5, 6,  3, 4, 7,"

          + "9, 4, E,  7, 6, F,  2, G, 5,"
          + "3, 6, 1,  H, 2, 5,  4, 7, I,"
          + "5, J, 2,  4, K, 8,  9, 1, 6,"

          + "6, 2, L,  5, 8, M,  7, 9, N,"
          + "O, 9, 7,  P, 1, 2,  5, Q, 4,"
          + "4, R, 5,  6, S, 7,  8, 2, 3"
          + ").", context);

      final Term result = choicePoint.prove();
      assertNotNull(result);

      assertEquals("2", choicePoint.findVar("A").get().findNonVarOrSame().getText());
      assertEquals("5", choicePoint.findVar("B").get().findNonVarOrSame().getText());
      assertEquals("3", choicePoint.findVar("C").get().findNonVarOrSame().getText());
      assertEquals("2", choicePoint.findVar("D").get().findNonVarOrSame().getText());
      assertEquals("8", choicePoint.findVar("E").get().findNonVarOrSame().getText());
      assertEquals("1", choicePoint.findVar("F").get().findNonVarOrSame().getText());
      assertEquals("3", choicePoint.findVar("G").get().findNonVarOrSame().getText());
      assertEquals("9", choicePoint.findVar("H").get().findNonVarOrSame().getText());
      assertEquals("8", choicePoint.findVar("I").get().findNonVarOrSame().getText());
      assertEquals("7", choicePoint.findVar("J").get().findNonVarOrSame().getText());
      assertEquals("3", choicePoint.findVar("K").get().findNonVarOrSame().getText());
      assertEquals("3", choicePoint.findVar("L").get().findNonVarOrSame().getText());
      assertEquals("4", choicePoint.findVar("M").get().findNonVarOrSame().getText());
      assertEquals("1", choicePoint.findVar("N").get().findNonVarOrSame().getText());
      assertEquals("8", choicePoint.findVar("O").get().findNonVarOrSame().getText());
      assertEquals("3", choicePoint.findVar("P").get().findNonVarOrSame().getText());
      assertEquals("6", choicePoint.findVar("Q").get().findNonVarOrSame().getText());
      assertEquals("1", choicePoint.findVar("R").get().findNonVarOrSame().getText());
      assertEquals("9", choicePoint.findVar("S").get().findNonVarOrSame().getText());

      assertNull(choicePoint.prove());
    }
  }
}
