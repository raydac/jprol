package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Allowed modificators for validating parameters.
 *
 * @see JProlPredicate#validate()
 * @since 3.0.0
 */
public enum ValidateModificator {
  UNBOUND("At call time, the argument must be unbound.",
      "--", s -> ProlAssertions.checkUnbound(s) == null, s -> false,
      ProlAssertions::checkUnbound),
  SOFT_UNBOUND(
      "Argument is an output argument. It may or may not be bound at call-time. If the argument is bound at call time, the goal behaves as if the argument were unbound, and then unified with that term after the goal succeeds.",
      "-", s -> true, s -> {
    final Term that = s.tryGround();
    return that.getTermType() != TermType.VAR;
  },
      s -> null),
  GROUND(
      "At call time, the argument must be ground, i.e., the argument may not contain any variables that are still unbound.",
      "++", Term::isGround,
      s -> true,
      ProlAssertions::checkGround),
  SOFT_GROUND(
      "At call time, the argument must be instantiated to a term satisfying some (informal) type specification. The argument need not necessarily be ground.",
      "+", s -> ProlAssertions.checkNonVar(s) == null,
      s -> true,
      ProlAssertions::checkNonVar),
  MAY_UNBOUND_OR_GROUND(
      "At call time, the argument must be bound to a partial term (a term which may or may not be ground) satisfying some (informal) type specification.",
      "?", s -> true,
      s -> s.tryGround().getTermType() != TermType.VAR,
      s -> null),
  META_ARGUMENT(
      "Argument is a meta-argument, for example a term that can be called as goal. This flag implies +.",
      ":",
      s -> ProlAssertions.checkMeta(s) == null,
      s -> true,
      ProlAssertions::checkMeta),
  INSTANTIATED("Argument will not be further instantiated than it is at call-time.",
      "@",
      s -> true,
      s -> s.tryGround().getTermType() != TermType.VAR,
      s -> {
        if (s.tryGround().getTermType() != TermType.VAR) {
          return null;
        } else {
          return new ProlInstantiationErrorException("Expected instantiated: " + s, s);
        }
      });

  public static final Map<String, ValidateModificator> MAPPED =
      Arrays.stream(ValidateModificator.values())
          .collect(Collectors.toMap(ValidateModificator::getText, x -> x));

  private final String text;
  private final Predicate<Term> checker;
  private final Predicate<Term> checkTypeAllowed;
  private final String description;
  private final Function<Term, ProlException> exceptionFunction;

  ValidateModificator(final String description, final String text, final Predicate<Term> checker,
                      final Predicate<Term> checkTypeAllowed,
                      final Function<Term, ProlException> exceptionFunction) {
    this.description = description;
    this.text = text;
    this.checker = checker;
    this.checkTypeAllowed = checkTypeAllowed;
    this.exceptionFunction = exceptionFunction;
  }

  public String getText() {
    return this.text;
  }

  public static Optional<ValidateModificator> find(final String text) {
    if (text == null) {
      return Optional.empty();
    }
    return Optional.ofNullable(MAPPED.get(text.trim()));
  }

  public String getDescription() {
    return this.description;
  }

  public boolean test(final Term term) {
    return this.checker.test(term);
  }

  public boolean isTypeCheckAllowed(final Term term) {
    return this.checkTypeAllowed.test(term);
  }

  public ProlException findException(final Term term) {
    return this.exceptionFunction.apply(term);
  }

}
