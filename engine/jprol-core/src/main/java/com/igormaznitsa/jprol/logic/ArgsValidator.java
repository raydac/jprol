package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.CompoundTerm;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.exceptions.ProlArgumentValidationException;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlRepresentationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public final class ArgsValidator {

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

  public static Predicate<TermStruct> makeFor(final int arity, final String[] arguments,
                                              final boolean assertMode) {
    final List<List<ModificatorArgument>> parsed = parse(arity, arguments, assertMode);

    Predicate<TermStruct> result = ANY;
    ModificatorArgument[] lastRow = null;
    for (final List<ModificatorArgument> row : parsed) {
      lastRow = row.toArray(ModificatorArgument[]::new);
      final ModificatorArgument[] finalRow = lastRow;
      final Predicate<TermStruct> newPredicate = s -> {
        for (int i = 0; i < finalRow.length; i++) {
          if (!finalRow[i].test(s.getArgumentAt(i))) {
            return false;
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

    if (assertMode && !parsed.isEmpty()) {
      if (parsed.size() == 1) {
        final ModificatorArgument[] finalRow = lastRow;
        result = result.or(s -> {
          for (int i = 0; i < finalRow.length; i++) {
            final ProlException exception = finalRow[i].findException(s.getArgumentAt(i));
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

    return result;
  }

  public enum Modificator {
    UNBOUND("At call time, the argument must be unbound.",
        new String[] {"--"}, Term::isUnground, s -> false,
        s -> s.isUnground() ? null :
            new ProlTypeErrorException("var", "Unbound variable expected: " + s, s)),
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
        s -> s.isGround() ? null : new ProlInstantiationErrorException(s)),
    SOFT_GROUND(
        "At call time, the argument must be instantiated to a term satisfying some (informal) type specification. The argument need not necessarily be ground.",
        new String[] {"+"}, s -> s.tryGround().getTermType() != TermType.VAR,
        s -> true,
        s -> s.tryGround().getTermType() != TermType.VAR ? null :
            new ProlInstantiationErrorException(s)),
    MAY_UNBOUND_OR_GROUND(
        "At call time, the argument must be bound to a partial term (a term which may or may not be ground) satisfying some (informal) type specification.",
        new String[] {"?"}, s -> true,
        s -> s.tryGround().getTermType() != TermType.VAR,
        s -> null),
    META_ARGUMENT(
        "Argument is a meta-argument, for example a term that can be called as goal. This flag implies +.",
        new String[] {":"}, s -> {
      final Term that = s.tryGround();
      return (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
          that.getTermType() == TermType.STRUCT;
    }, s -> true, s -> {
      final Term that = s.tryGround();
      final boolean flag =
          (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
              that.getTermType() == TermType.STRUCT;
      if (flag) {
        return null;
      } else {
        if (that.getTermType() == TermType.VAR) {
          return new ProlInstantiationErrorException("Expected instantiated: " + that, that);
        } else {
          return new ProlTypeErrorException("ground", that);
        }
      }
    }),
    INSTANTIATED("Argument will not be further instantiated than it is at call-time.",
        new String[] {"@"}, s -> true, s -> s.tryGround().getTermType() != TermType.VAR,
        s -> {
          if (s.tryGround().getTermType() != TermType.VAR) {
            return null;
          } else {
            return new ProlException();
          }
        }),
    MODIFIABLE_STRUCTURE("Argument contains a mutable structure that may be modified",
        new String[] {"!"}, Term::isGround, s -> true, s -> {
      if (!s.isGround()) {
        return new ProlException();
      }
      return null;
    });

    private static final Map<String, Modificator> MAPPED = Arrays.stream(Modificator.values())
        .flatMap(x -> Arrays.stream(x.names).map(y -> Map.entry(x, y))).collect(Collectors.toMap(
            Map.Entry::getValue, Map.Entry::getKey));

    private final String[] names;
    private final Predicate<Term> checker;
    private final Predicate<Term> checkTypeAllowed;
    private final String description;
    private final Function<Term, ProlException> exceptionFunction;

    Modificator(final String decription, final String[] names, final Predicate<Term> checker,
                final Predicate<Term> checkTypeAllowed,
                final Function<Term, ProlException> exceptionFunction) {
      this.description = decription;
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

    public boolean isTestTypeAllowed(final Term term) {
      return this.checkTypeAllowed.test(term);
    }

    public ProlException findException(final Term term) {
      return this.exceptionFunction.apply(term);
    }

  }

  public enum ArgType {
    TERM("term", s -> true, s -> null),
    ATOM("atom", s -> {
      final Term that = s.tryGround();
      return that.isGround() && !(that instanceof CompoundTerm);
    }, s -> {
      final Term that = s.tryGround();
      if (that.isGround() && !(that instanceof CompoundTerm)) {
        return null;
      }
      return new ProlTypeErrorException("atom", "Expected atom: " + that, that);
    }),
    NAME("name", s -> {
      final Term that = s.tryGround();
      return that.isGround() && (that.isNullList() || !(that instanceof CompoundTerm));
    }, s -> {
      final Term that = s.tryGround();
      if (that.isGround() && (that.isNullList() || !(that instanceof CompoundTerm))) {
        return null;
      }
      return new ProlTypeErrorException("name", s);
    }),
    ARITY("arity", s -> {
      final Term that = s.tryGround();
      if (that instanceof TermLong) {
        final long value = that.toNumber().longValue();
        return value >= 0 && value < (Integer.MAX_VALUE + 1L);
      }
      return false;
    }, s -> {
      final Term that = s.tryGround();
      if (that instanceof TermLong) {
        final long value = that.toNumber().longValue();
        if (value < 0) {
          return new ProlDomainErrorException("arity", "Negative value is not allowed: " + that,
              that);
        }
        if (value > Integer.MAX_VALUE) {
          return new ProlRepresentationErrorException("max_arity", "Wrong arity: " + that, that);
        }
        return null;
      }
      return new ProlTypeErrorException("arity", "Arity must be zero or greater than zero: " + s,
          s);
    }),
    ATOMIC("atomic", s -> {
      final Term that = s.tryGround();
      return that.isGround() && !(that instanceof CompoundTerm);
    }, s -> new ProlException()),
    CHAR_LIST("char_list", s -> {
      final Term that = s.tryGround();
      if (that.getTermType() == TermType.LIST) {
        final TermList termList = (TermList) that;
        if (termList.isNullList()) {
          return false;
        }
        return termList.doesContainOnlyCharCodes();
      } else {
        return false;
      }
    }, s -> new ProlException()),
    STRING("string", s -> {
      final Term that = s.tryGround();
      if (that.getTermType() == TermType.LIST) {
        final TermList termList = (TermList) that;
        if (termList.isNullList()) {
          return false;
        }
        return termList.doesContainOnlyCharCodes();
      } else {
        return false;
      }
    }, s -> new ProlException()),
    FLOAT("float", s -> s.tryGround() instanceof TermDouble,
        s -> {
          final Term t = s.tryGround();
          return t instanceof TermDouble ? null :
              new ProlTypeErrorException("numeric", "Expected float term", s);
        }),
    INTEGER("integer", s ->
        s.tryGround() instanceof TermLong,
        s -> {
          final Term t = s.tryGround();
          return t instanceof TermLong ? null :
              new ProlTypeErrorException("numeric", "Expected integer term", s);
        }),
    NUMBER("number", s -> s.tryGround() instanceof NumericTerm, s -> {
      final Term t = s.tryGround();
      if (t instanceof NumericTerm) {
        return null;
      } else {
        throw new ProlTypeErrorException("number", "Number expected: " + t, t);
      }
    }),
    GOAL("goal", s -> {
      final Term that = s.tryGround();
      return (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
          that.getTermType() == TermType.STRUCT;
    }, s -> new ProlException()),
    CALLABLE("callable", s -> {
      final Term that = s.tryGround();
      return (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
          that.getTermType() == TermType.STRUCT;
    }, s -> {
      final Term that = s.tryGround();
      if ((that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
          that.getTermType() == TermType.STRUCT) {
        return null;
      } else {
        throw new ProlTypeErrorException("callable", "Expected callable term: " + that, that);
      }
    }),
    HEAD("head", s -> {
      final Term that = s.tryGround();
      return (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
          that.getTermType() == TermType.STRUCT;
    }, s -> {
      final Term that = s.tryGround();
      if ((that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
          that.getTermType() == TermType.STRUCT) {
        return null;
      } else {
        throw new ProlTypeErrorException("head", "Expected head term: " + that, that);
      }
    }),
    BODY("body", s -> {
      final Term that = s.tryGround();
      return (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
          that.getTermType() == TermType.STRUCT;
    }, s -> {
      final Term that = s.tryGround();
      if ((that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) ||
          that.getTermType() == TermType.STRUCT) {
        return null;
      } else {
        throw new ProlTypeErrorException("body", "Expected body term: " + that, that);
      }
    }),
    EVALUABLE("evaluable", s -> {
      final Term that = s.tryGround();
      if (that instanceof NumericTerm) {
        return true;
      }
      return that.getTermType() == TermType.STRUCT;
    }, s -> {
      final Term that = s.tryGround();
      if (!(that instanceof NumericTerm) && that.getTermType() != TermType.STRUCT) {
        throw new ProlTypeErrorException("evaluable", "Evaluable member expected: " + that, that);
      }
      return null;
    }),
    LIST("list", s -> {
      final Term that = s.tryGround();
      return that.getTermType() == TermType.LIST;
    }, s -> new ProlException()),
    CHAR("char", s -> {
      final Term that = s.tryGround();
      return that.getTermType() == TermType.ATOM
          && !(that instanceof NumericTerm)
          && that.getText().length() == 1;
    }, s -> new ProlException()),
    PREDICATE_INDICATOR("predicate_indicator", s -> ProlAssertions.findIndicatorError(s) == null,
        ProlAssertions::findIndicatorError),
    OPERATOR_SPECIFIER("operator_specifier", s -> {
      final Term that = s.tryGround();
      if (that.getTermType() == TermType.ATOM && !(that instanceof NumericTerm)) {
        return OpAssoc.findForName(that.getText())
            .isPresent(); // don't change for Android API compatibility
      }
      return false;
    }, s -> new ProlException()),
    COMPOUND_TERM("compound_term", s -> {
      final Term that = s.tryGround();
      return that instanceof CompoundTerm;
    }, s -> {
      final Term t = s.tryGround();
      switch (t.getTermType()) {
        case LIST:
        case STRUCT: {
          throw new ProlCriticalError("Unexpected request for error");
        }
        case VAR: {
          throw new ProlInstantiationErrorException("Expected instantiated compound term: " + t, t);
        }
        default:
          throw new ProlTypeErrorException("compound", "Expected compound: " + t, t);
      }
    }),
    COMPOUND("compound", s -> {
      final Term that = s.tryGround();
      return that instanceof CompoundTerm;
    }, s -> new ProlException()),
    VAR("var", s -> {
      final Term that = s.tryGround();
      return that.getTermType() == TermType.VAR;
    }, s -> new ProlException());

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
        if (this.modificator.isTestTypeAllowed(term)) {
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
