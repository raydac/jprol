package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.igormaznitsa.jprol.exceptions.ProlArgumentValidationException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.io.IOException;
import org.junit.jupiter.api.Test;

class ArgumentValidationScriptTest extends AbstractJProlTest {

  @Test
  void verifiedScriptAcceptsGoodArgumentsAndRejectsBadOnes() throws IOException {
    try (JProlContext context = this.makeContextAndConsultFromResource(
        "argument_validation_test.prl")) {
      final JProlChoicePoint goal = context.makeChoicePoint("run_tests(Failures).");
      assertNotNull(goal.prove());
      assertEquals("[]", getVarAsText(goal, "Failures"));
      assertNull(goal.prove());
    }
  }

  @Test
  void validationIsAppliedOnlyWhileVerifyIsOn() {
    try (JProlContext context = this.makeTestContext()) {
      assertNotNull(context.makeChoicePoint("set_prolog_flag(verify, true).").prove());
      assertThrows(
          ProlArgumentValidationException.class,
          () -> context.makeChoicePoint("atom_chars(12, L).").prove());
      assertThrows(
          ProlArgumentValidationException.class,
          () -> context.makeChoicePoint("char_code(ab, C).").prove());
      assertThrows(
          ProlArgumentValidationException.class,
          () -> context.makeChoicePoint("char_code(2, C).").prove());
      assertThrows(
          ProlArgumentValidationException.class,
          () -> context.makeChoicePoint("rnd(foo, X).").prove());
      assertThrows(
          ProlTypeErrorException.class,
          () -> context.makeChoicePoint("length(foo, 3).").prove());

      assertNotNull(context.makeChoicePoint("set_prolog_flag(verify, false).").prove());

      final JProlChoicePoint chars = context.makeChoicePoint("atom_chars(12, L).");
      assertNotNull(chars.prove());
      assertEquals("['1','2']", getVarAsText(chars, "L"));

      final JProlChoicePoint longChar = context.makeChoicePoint("char_code(ab, C).");
      assertNotNull(longChar.prove());
      assertEquals("97", getVarAsText(longChar, "C"));

      final JProlChoicePoint digit = context.makeChoicePoint("char_code(2, C).");
      assertNotNull(digit.prove());
      assertEquals("50", getVarAsText(digit, "C"));

      assertNull(context.makeChoicePoint("length(foo, 3).").prove());
      assertThrows(
          ProlTypeErrorException.class,
          () -> context.makeChoicePoint("rnd(foo, X).").prove());
    }
  }
}
