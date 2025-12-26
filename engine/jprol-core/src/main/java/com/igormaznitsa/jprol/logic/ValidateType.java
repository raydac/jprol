package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Allowed types for validating parameters.
 *
 * @see JProlPredicate#validate()
 * @since 3.0.0
 */
public enum ValidateType {
  TERM("term", s -> true, s -> null),
  ATOM("atom", s -> ProlAssertions.checkAtom(s) == null, ProlAssertions::checkAtom),
  NAME("name", s -> ProlAssertions.checkName(s) == null, ProlAssertions::checkName),
  ARITY("arity", s -> ProlAssertions.checkArity(s) == null, ProlAssertions::checkArity),
  ATOMIC("atomic", s -> ProlAssertions.assertAtomic(s) == null, ProlAssertions::checkAtomic),
  CHAR_LIST("char_list", s -> ProlAssertions.checkCharList(s) == null,
      ProlAssertions::checkCharList),
  CODE_LIST("code_list", s -> ProlAssertions.checkCharCodeList(s) == null,
      ProlAssertions::checkCharCodeList),
  STRING("string", s -> ProlAssertions.assertString(s) == null, ProlAssertions::checkString),
  FLOAT("float", s -> ProlAssertions.checkFloat(s) == null, ProlAssertions::checkFloat),
  INTEGER("integer", s -> ProlAssertions.checkInteger(s) == null, ProlAssertions::checkInteger),
  NUMBER("number", s -> ProlAssertions.checkNumber(s) == null, ProlAssertions::checkNumber),
  GOAL("goal", s -> ProlAssertions.checkGoal(s) == null, ProlAssertions::checkGoal),
  CALLABLE("callable", s -> ProlAssertions.checkCallable(s) == null,
      ProlAssertions::checkCallable),
  HEAD("head", s -> ProlAssertions.checkHead(s) == null, ProlAssertions::checkHead),
  BODY("body", s -> ProlAssertions.checkBody(s) == null, ProlAssertions::checkBody),
  EVALUABLE("evaluable", s -> ProlAssertions.checkEvaluable(s) == null,
      ProlAssertions::checkEvaluable),
  LIST("list", s -> ProlAssertions.checkList(s) == null, ProlAssertions::checkList),
  NONEMPTY_LIST("nonempty_list", s -> ProlAssertions.checkNonEmptyList(s) == null,
      ProlAssertions::checkNonEmptyList),
  CHAR("char", s -> ProlAssertions.checkChar(s) == null, ProlAssertions::checkChar),
  TRIGGER_EVENT("trigger_event", s -> ProlAssertions.checkTriggerEvent(s) == null,
      ProlAssertions::checkTriggerEvent),
  PREDICATE_INDICATOR("predicate_indicator", s -> ProlAssertions.checkIndicator(s) == null,
      ProlAssertions::checkIndicator),
  OPERATOR_SPECIFIER("operator_specifier", s -> ProlAssertions.checkOperatorSpecifier(s) == null,
      ProlAssertions::checkOperatorSpecifier),
  IO_MODE("io_mode", s -> ProlAssertions.checkIoMode(s) == null, ProlAssertions::checkIoMode),
  COMPOUND_TERM("compound_term", s -> ProlAssertions.checkCompound(s) == null,
      ProlAssertions::checkCompound),
  COMPOUND("compound", s -> ProlAssertions.checkCompound(s) == null,
      ProlAssertions::checkCompound),
  VAR("var", s -> ProlAssertions.checkVar(s) == null, ProlAssertions::checkVar);

  private static final List<ValidateType> VALUES = Arrays.asList(ValidateType.values());
  private final String text;
  private final Predicate<Term> checker;
  private final Function<Term, ProlException> exceptionFunction;

  ValidateType(final String text, final Predicate<Term> checker,
               final Function<Term, ProlException> exceptionFunction) {
    this.text = text;
    this.checker = checker;
    this.exceptionFunction = exceptionFunction;
  }

  public static Optional<ValidateType> find(final String text) {
    final String normalized = text.trim().toLowerCase(Locale.ROOT);
    return VALUES.stream().filter(x -> x.text.equals(normalized)).findFirst();
  }

  public boolean test(final Term term) {
    return this.checker.test(term);
  }

  public String getText() {
    return this.text;
  }

  public ProlException findException(final Term term) {
    return this.exceptionFunction.apply(term);
  }
}
