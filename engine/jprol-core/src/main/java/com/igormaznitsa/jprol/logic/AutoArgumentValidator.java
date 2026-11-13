package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.TermType.VAR;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.exceptions.ProlArgumentValidationException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public final class AutoArgumentValidator {

  private static final Predicate<TermStruct> ANY = x -> true;

  private static final ModificatorArgument[] EMPTY_MODIFICATOR_ARGUMENT_ARRAY =
      new ModificatorArgument[0];

  private AutoArgumentValidator() {
  }

  static boolean isAcceptAll(final Predicate<TermStruct> validator) {
    return validator == null || validator == ANY;
  }

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
    if (arity == 0 || allArguments.isEmpty()) {
      return ANY;
    }
    final List<List<ModificatorArgument>> filtered =
        allArguments.stream().filter(x -> x.size() == arity).collect(Collectors.toList());
    if (filtered.isEmpty()) {
      return ANY;
    }

    final ModificatorArgument[][] rows = new ModificatorArgument[filtered.size()][];
    for (int r = 0; r < filtered.size(); r++) {
      final ModificatorArgument[] validatorRow =
          filtered.get(r).toArray(EMPTY_MODIFICATOR_ARGUMENT_ARRAY);
      if (isTautologyRow(validatorRow)) {
        return ANY;
      }
      rows[r] = validatorRow;
    }

    if (rows.length == 1) {
      return new SingleRowValidator(rows[0], assertMode);
    }
    return new MultiRowValidator(rows, assertMode);
  }

  private static boolean isTautologyRow(final ModificatorArgument[] row) {
    for (final ModificatorArgument arg : row) {
      if (!arg.alwaysAccepts) {
        return false;
      }
    }
    return true;
  }

  private static boolean matchesRow(final ModificatorArgument[] row, final TermStruct struct) {
    for (int i = 0; i < row.length; i++) {
      final ModificatorArgument spec = row[i];
      if (!spec.alwaysAccepts && !spec.test(struct.getArgumentAt(i))) {
        return false;
      }
    }
    return true;
  }

  private static final class SingleRowValidator implements Predicate<TermStruct> {

    private final ModificatorArgument[] row;
    private final boolean assertMode;

    private SingleRowValidator(final ModificatorArgument[] row, final boolean assertMode) {
      this.row = row;
      this.assertMode = assertMode;
    }

    @Override
    public boolean test(final TermStruct struct) {
      if (matchesRow(this.row, struct)) {
        return true;
      }
      if (!this.assertMode) {
        return false;
      }
      for (int i = 0; i < this.row.length; i++) {
        final ProlException exception = this.row[i].findException(struct.getArgumentAt(i));
        if (exception != null) {
          throw exception;
        }
      }
      throw new ProlArgumentValidationException(
          "Detected inappropriate argument: " + struct.toSrcString());
    }
  }

  private static final class MultiRowValidator implements Predicate<TermStruct> {

    private final ModificatorArgument[][] rows;
    private final boolean assertMode;

    private MultiRowValidator(final ModificatorArgument[][] rows, final boolean assertMode) {
      this.rows = rows;
      this.assertMode = assertMode;
    }

    @Override
    public boolean test(final TermStruct struct) {
      for (final ModificatorArgument[] row : this.rows) {
        if (matchesRow(row, struct)) {
          return true;
        }
      }
      if (!this.assertMode) {
        return false;
      }
      throw new ProlArgumentValidationException(
          "There is not any pattern allows such call: " + struct.toSrcString());
    }
  }

  public static final class ModificatorArgument {
    private final ValidateModificator modificator;
    private final ValidateType validateType;
    private final boolean alwaysAccepts;

    private ModificatorArgument(
        final ValidateModificator modificator,
        final ValidateType validateType
    ) {
      this.modificator = modificator;
      this.validateType = validateType;
      this.alwaysAccepts = validateType == ValidateType.TERM
          && (modificator == ValidateModificator.MAY_UNBOUND_OR_GROUND
          || modificator == ValidateModificator.INSTANTIATED
          || modificator == ValidateModificator.SOFT_UNBOUND);
    }

    public ValidateModificator getModificator() {
      return this.modificator;
    }

    public ValidateType getArgType() {
      return this.validateType;
    }

    public boolean test(final Term term) {
      if (this.alwaysAccepts) {
        return true;
      }
      if (this.modificator == ValidateModificator.UNBOUND) {
        return term.isUnground();
      }

      final Term grounded = term.tryGround();
      switch (this.modificator) {
        case SOFT_GROUND:
          if (grounded.getTermType() == VAR) {
            return false;
          }
          break;
        case GROUND:
          if (!grounded.isGround()) {
            return false;
          }
          break;
        case META_ARGUMENT:
          if (!ProlAssertions.isCallableGrounded(grounded)) {
            return false;
          }
          break;
        case SOFT_UNBOUND:
        case MAY_UNBOUND_OR_GROUND:
        case INSTANTIATED:
          if (grounded.getTermType() == VAR) {
            return true;
          }
          break;
        default:
          if (!this.modificator.test(term)) {
            return false;
          }
          if (!this.modificator.isTypeCheckAllowed(term)) {
            return true;
          }
          break;
      }

      return this.validateType == ValidateType.TERM
          || this.validateType.testGrounded(term, grounded);
    }

    public ProlException findException(final Term term) {
      final ProlException result = this.modificator.findException(term);
      if (result != null) {
        return result;
      }
      return this.validateType.findException(term);
    }
  }
}
