package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.exceptions.ProlArgumentValidationException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public final class AutoArgumentValidator {

  private static final Predicate<TermStruct> ANY = x -> true;

  public static List<List<ModificatorArgument>> parse(final String[] argumentsTypes) {
    final List<List<ModificatorArgument>> result = new ArrayList<>();
    for (final String s : argumentsTypes) {
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
          final ValidateModificator modificator =
              ValidateModificator.find(a.substring(0, index).trim())
                  .orElseThrow(() -> new IllegalArgumentException("Unsupported modificator: " + a));
          final ValidateType argument = ValidateType.find(a.substring(index).trim())
              .orElseThrow(() -> new IllegalArgumentException("Unsupported argument type:" + a));
          current.add(new ModificatorArgument(modificator, argument));
        }
        result.add(current);
      }
    }
    return result;
  }

  public static Predicate<TermStruct> makeFor(
      final int arity,
      final List<List<ModificatorArgument>> allArguments,
      final boolean assertMode) {
    if (arity == 0) {
      return ANY;
    }
    final List<List<ModificatorArgument>> filtered =
        allArguments.stream().filter(x -> x.size() == arity).collect(
            Collectors.toList());
    if (allArguments.isEmpty()) {
      return ANY;
    }

    Predicate<TermStruct> result = ANY;
    for (final List<ModificatorArgument> row : filtered) {
      if (row.size() == arity) {
        final ModificatorArgument[] validatorRow = row.toArray(new ModificatorArgument[arity]);
        final Predicate<TermStruct> newPredicate = s -> {
          for (int i = 0; i < arity; i++) {
            if (!validatorRow[i].test(s.getArgumentAt(i))) {
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
    }

    if (result != ANY) {
      if (assertMode) {
        if (filtered.size() == 1) {
          final ModificatorArgument[] rowArray =
              filtered.get(0).toArray(new ModificatorArgument[filtered.get(0).size()]);
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

  public static final class ModificatorArgument {
    private final ValidateModificator modificator;
    private final ValidateType validateType;

    private ModificatorArgument(final ValidateModificator modificator,
                                final ValidateType validateType) {
      this.modificator = modificator;
      this.validateType = validateType;
    }

    public ValidateModificator getModificator() {
      return this.modificator;
    }

    public ValidateType getArgType() {
      return this.validateType;
    }

    public boolean test(final Term term) {
      if (this.modificator.test(term)) {
        if (this.modificator.isTypeCheckAllowed(term)) {
          return this.validateType.test(term);
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
      return this.validateType.findException(term);
    }
  }
}
