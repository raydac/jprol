package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.exceptions.ProlArgumentValidationException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public final class AutoArgumentValidator {

  private static final Predicate<TermStruct> ANY = x -> true;

  private static List<List<ModificatorArgument>> parse(final int arity, final String[] arguments,
                                                       final boolean assertMode) {
    final List<List<ModificatorArgument>> result = new ArrayList<>();
    for (final String s : arguments) {
      if (s != null) {
        final List<ModificatorArgument> current = new ArrayList<>();
        for (final String a : s.split(",")) {
          int index = 0;
          for (int i = 0; i < a.length(); i++) {
            if (Character.isAlphabetic(a.charAt(i))) {
              break;
            }
            index++;
          }
          final Modificator modificator = Modificator.find(a.substring(0, index).trim())
              .orElseThrow(() -> new IllegalArgumentException("Unsupported modificator: " + a));
          final ArgType argument = ArgType.find(a.substring(index).trim())
              .orElseThrow(() -> new IllegalArgumentException("Unsupported argument type:" + a));
          current.add(new ModificatorArgument(modificator, argument));
        }
        if (current.size() != arity) {
          throw new IllegalArgumentException("Expected " + arity + " arguments: " + s);
        }
        result.add(current);
      }
    }
    return result;
  }

  public static Predicate<TermStruct> makeFor(
      final int arity,
      final String[] arguments,
      final boolean assertMode) {
    final List<List<ModificatorArgument>> parsed = parse(arity, arguments, assertMode);

    Predicate<TermStruct> result = ANY;
    for (final List<ModificatorArgument> row : parsed) {
      final Predicate<TermStruct> newPredicate = s -> {
        if (s.getArity() == row.size()) {
          for (int i = 0; i < row.size(); i++) {
            if (!row.get(i).test(s.getArgumentAt(i))) {
              return false;
            }
          }
        }
        return true;
      };

      if (result == ANY) {
        result = newPredicate;
      } else {
        result = result.or(newPredicate);
      }
    }

    if (result != ANY) {
      if (assertMode) {
        if (parsed.size() == 1) {
          final ModificatorArgument[] rowArray =
              parsed.get(0).toArray(new ModificatorArgument[parsed.get(0).size()]);
          result = result.or(s -> {
            for (int i = 0; i < rowArray.length; i++) {
              final ProlException exception = rowArray[i].findException(s.getArgumentAt(i));
              if (exception != null) {
                throw exception;
              }
            }
            throw new ProlArgumentValidationException(
                "Detected inappropriate argument: " + s.toSrcString());
          });
        } else {
          result = result.or(s -> {
            throw new ProlArgumentValidationException(
                "There is not any pattern allows such call: " + s.toSrcString());
          });
        }
      }
    }
    return result;
  }

  public enum Modificator {
    UNBOUND("At call time, the argument must be unbound.",
        new String[] {"--"}, s -> ProlAssertions.checkUnbound(s) == null, s -> false,
        ProlAssertions::checkUnbound),
    SOFT_UNBOUND(
        "Argument is an output argument. It may or may not be bound at call-time. If the argument is bound at call time, the goal behaves as if the argument were unbound, and then unified with that term after the goal succeeds.",
        new String[] {"-"}, s -> true, s -> {
      final Term that = s.tryGround();
      return that.getTermType() != TermType.VAR;
    },
        s -> null),
    GROUND(
        "At call time, the argument must be ground, i.e., the argument may not contain any variables that are still unbound.",
        new String[] {"++"}, Term::isGround,
        s -> true,
        ProlAssertions::checkGround),
    SOFT_GROUND(
        "At call time, the argument must be instantiated to a term satisfying some (informal) type specification. The argument need not necessarily be ground.",
        new String[] {"+"}, s -> ProlAssertions.checkNonVar(s) == null,
        s -> true,
        ProlAssertions::checkNonVar),
    MAY_UNBOUND_OR_GROUND(
        "At call time, the argument must be bound to a partial term (a term which may or may not be ground) satisfying some (informal) type specification.",
        new String[] {"?"}, s -> true,
        s -> s.tryGround().getTermType() != TermType.VAR,
        s -> null),
    META_ARGUMENT(
        "Argument is a meta-argument, for example a term that can be called as goal. This flag implies +.",
        new String[] {":"},
        s -> ProlAssertions.checkMeta(s) == null,
        s -> true,
        ProlAssertions::checkMeta),
    INSTANTIATED("Argument will not be further instantiated than it is at call-time.",
        new String[] {"@"},
        s -> true,
        s -> s.tryGround().getTermType() != TermType.VAR,
        s -> {
          if (s.tryGround().getTermType() != TermType.VAR) {
            return null;
          } else {
            return new ProlInstantiationErrorException("Expected instantiated: " + s, s);
          }
        });

    private static final Map<String, Modificator> MAPPED = Arrays.stream(Modificator.values())
        .flatMap(x -> Arrays.stream(x.names).map(y -> Map.entry(x, y))).collect(Collectors.toMap(
            Map.Entry::getValue, Map.Entry::getKey));

    private final String[] names;
    private final Predicate<Term> checker;
    private final Predicate<Term> checkTypeAllowed;
    private final String description;
    private final Function<Term, ProlException> exceptionFunction;

    Modificator(final String description, final String[] names, final Predicate<Term> checker,
                final Predicate<Term> checkTypeAllowed,
                final Function<Term, ProlException> exceptionFunction) {
      this.description = description;
      this.names = names;
      this.checker = checker;
      this.checkTypeAllowed = checkTypeAllowed;
      this.exceptionFunction = exceptionFunction;
    }

    public static Optional<Modificator> find(final String text) {
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

  public enum ArgType {
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

    private static final List<ArgType> VALUES = Arrays.asList(ArgType.values());
    private final String text;
    private final Predicate<Term> checker;
    private final Function<Term, ProlException> exceptionFunction;

    ArgType(final String text, final Predicate<Term> checker,
            final Function<Term, ProlException> exceptionFunction) {
      this.text = text;
      this.checker = checker;
      this.exceptionFunction = exceptionFunction;
    }

    public static Optional<ArgType> find(final String text) {
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

  private static final class ModificatorArgument {
    private final Modificator modificator;
    private final ArgType argType;

    ModificatorArgument(final Modificator modificator, final ArgType argType) {
      this.modificator = modificator;
      this.argType = argType;
    }

    boolean test(final Term term) {
      if (this.modificator.test(term)) {
        if (this.modificator.isTypeCheckAllowed(term)) {
          return this.argType.test(term);
        } else {
          return true;
        }
      }
      return false;
    }

    public ProlException findException(final Term term) {
      ProlException result = this.modificator.findException(term);
      if (result != null) {
        return result;
      }
      return this.argType.findException(term);
    }
  }
}
