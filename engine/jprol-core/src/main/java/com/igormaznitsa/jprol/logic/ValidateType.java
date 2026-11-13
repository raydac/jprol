package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.function.BiPredicate;
import java.util.function.Function;

/**
 * Allowed types for validating parameters.
 *
 * @see JProlPredicate#validate()
 * @since 3.0.0
 */
public enum ValidateType {
  TERM("term", (raw, grounded) -> true, s -> null),
  ATOM("atom", (raw, grounded) -> ProlAssertions.isAtomGrounded(grounded),
      ProlAssertions::checkAtom),
  FORMAT("format", (raw, grounded) -> ProlAssertions.isFormatGrounded(grounded),
      ProlAssertions::checkFormat),
  NAME("name", (raw, grounded) -> ProlAssertions.isNameGrounded(grounded),
      ProlAssertions::checkName),
  ARITY("arity", (raw, grounded) -> ProlAssertions.isArityGrounded(grounded),
      ProlAssertions::checkArity),
  ATOMIC("atomic", (raw, grounded) -> ProlAssertions.isAtomicGrounded(grounded),
      ProlAssertions::checkAtomic),
  CHAR_LIST("char_list", (raw, grounded) -> ProlAssertions.isCharListGrounded(grounded),
      ProlAssertions::checkCharList),
  CODE_LIST("code_list", (raw, grounded) -> ProlAssertions.isCharCodeListGrounded(grounded),
      ProlAssertions::checkCharCodeList),
  STRING("string", (raw, grounded) -> ProlAssertions.isPrologStringGrounded(grounded),
      ProlAssertions::checkString),
  FLOAT("float", (raw, grounded) -> grounded instanceof TermDouble, ProlAssertions::checkFloat),
  INTEGER("integer", (raw, grounded) -> grounded instanceof TermLong, ProlAssertions::checkInteger),
  NUMBER("number", (raw, grounded) -> grounded instanceof NumericTerm, ProlAssertions::checkNumber),
  GOAL("goal", (raw, grounded) -> ProlAssertions.isCallableGrounded(grounded),
      ProlAssertions::checkGoal),
  CALLABLE("callable", (raw, grounded) -> ProlAssertions.isCallableGrounded(grounded),
      ProlAssertions::checkCallable),
  HEAD("head", (raw, grounded) -> ProlAssertions.isHeadGrounded(grounded),
      ProlAssertions::checkHead),
  BODY("body", (raw, grounded) -> ProlAssertions.isCallableGrounded(grounded),
      ProlAssertions::checkBody),
  EVALUABLE("evaluable", (raw, grounded) -> ProlAssertions.isEvaluableGrounded(grounded),
      ProlAssertions::checkEvaluable),
  LIST("list", (raw, grounded) -> grounded.getTermType() == TermType.LIST,
      ProlAssertions::checkList),
  NONEMPTY_LIST("nonempty_list",
      (raw, grounded) -> grounded.getTermType() == TermType.LIST && !grounded.isNullList(),
      ProlAssertions::checkNonEmptyList),
  CHAR("char",
      (raw, grounded) -> grounded.getTermType() == TermType.ATOM &&
          grounded.getText().length() == 1,
      ProlAssertions::checkChar),
  TRIGGER_EVENT("trigger_event",
      (raw, grounded) -> ProlAssertions.isTriggerEventGrounded(grounded),
      ProlAssertions::checkTriggerEvent),
  PREDICATE_INDICATOR("predicate_indicator", ProlAssertions::isIndicatorGrounded,
      ProlAssertions::checkIndicator),
  OPERATOR_SPECIFIER("operator_specifier",
      (raw, grounded) -> ProlAssertions.isOperatorSpecifierGrounded(grounded),
      ProlAssertions::checkOperatorSpecifier),
  IO_MODE("io_mode", (raw, grounded) -> ProlAssertions.isIoModeGrounded(grounded),
      ProlAssertions::checkIoMode),
  COMPOUND_TERM("compound_term",
      (raw, grounded) ->
          grounded.getTermType() == TermType.LIST || grounded.getTermType() == TermType.STRUCT,
      ProlAssertions::checkCompound),
  COMPOUND("compound",
      (raw, grounded) ->
          grounded.getTermType() == TermType.LIST || grounded.getTermType() == TermType.STRUCT,
      ProlAssertions::checkCompound),
  VAR("var", (raw, grounded) -> !(raw.isUnground() && raw.getTermType() != TermType.VAR),
      ProlAssertions::checkVar);

  private static final List<ValidateType> VALUES = Arrays.asList(ValidateType.values());
  private final String text;
  private final BiPredicate<Term, Term> groundedChecker;
  private final Function<Term, ProlException> exceptionFunction;

  ValidateType(
      final String text,
      final BiPredicate<Term, Term> groundedChecker,
      final Function<Term, ProlException> exceptionFunction
  ) {
    this.text = text;
    this.groundedChecker = groundedChecker;
    this.exceptionFunction = exceptionFunction;
  }

  public static Optional<ValidateType> find(final String text) {
    final String normalized = text.trim().toLowerCase(Locale.ROOT);
    return VALUES.stream().filter(x -> x.text.equals(normalized)).findFirst();
  }

  public boolean test(final Term term) {
    return this.groundedChecker.test(term, term.tryGround());
  }

  boolean testGrounded(final Term rawTerm, final Term groundedTerm) {
    return this.groundedChecker.test(rawTerm, groundedTerm);
  }

  public String getText() {
    return this.text;
  }

  public ProlException findException(final Term term) {
    return this.exceptionFunction.apply(term);
  }
}
