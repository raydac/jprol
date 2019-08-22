package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.Terms;

import java.util.Locale;
import java.util.Optional;

import static com.igormaznitsa.jprol.data.Terms.*;
import static java.util.Arrays.stream;

public enum JProlSystemFlag {
  ADDRESS_BIT(true, Terms.newAtom("address_bit"), Terms.newLong(64)),
  ALLOW_VARIABLE_NAME_AS_FUNCTOR(true, Terms.newAtom("allow_variable_name_as_functor"), FALSE),
  OS(true, Terms.newAtom("os"), Terms.newAtom(System.getProperty("os.name", "unknown"))),
  ARCH(true, Terms.newAtom("arch"), Terms.newAtom(System.getProperty("os.arch", "unknown"))),
  BOUNDED(true, Terms.newAtom("bounded"), TRUE),
  DEBUG(false, Terms.newAtom("debug"), FALSE),
  DIALECT(true, Terms.newAtom("dialect"), Terms.newAtom("jprol")),
  ENCODING(false, Terms.newAtom("encoding"), Terms.newAtom("UTF-8")),
  GC(true, Terms.newAtom("gc"), TRUE),
  MAX_ARITY(true, Terms.newAtom("max_arity"), Terms.newLong(Integer.MAX_VALUE)),
  MAX_INTEGER(true, Terms.newAtom("max_integer"), Terms.newLong(Long.MAX_VALUE)),
  MIN_INTEGER(true, Terms.newAtom("min_integer"), Terms.newLong(Long.MIN_VALUE)),
  CPU_COUNT(true, Terms.newAtom("cpu_count"), Terms.newLong(Runtime.getRuntime().availableProcessors())),
  UNDEFINED_PREDICATE(false, Terms.newAtom("undefined_predicate"), UndefinedPredicateBehavior.ERROR.getTerm()),
  HOME(true, Terms.newAtom("home"), Terms.newAtom(System.getProperty("user.home", ""))),
  VERIFY(false, Terms.newAtom("verify"), TRUE),
  VERSION_DATA(true, Terms.newAtom("version_data"), newStruct(newAtom("jprol"), new Term[] {newLong(2), newLong(0), newLong(0), NULL_LIST}));

  private final Term nameTerm;
  private final Term defaultValue;
  private final boolean readOnly;

  JProlSystemFlag(final boolean readOnly, final Term name, final Term defaultValue) {
    this.nameTerm = name;
    this.readOnly = readOnly;
    this.defaultValue = defaultValue;
  }

  public Term getNameTerm() {
    return this.nameTerm;
  }

  public static Optional<JProlSystemFlag> find(final Term term) {
    final String termText = term.getTermType() != TermType.ATOM ? null : term.getText().toUpperCase(Locale.ENGLISH);

    Optional<JProlSystemFlag> result = Optional.empty();
    if (termText != null) {
      result = stream(JProlSystemFlag.values())
          .filter(x -> x.name().equals(termText))
          .findFirst();
    }
    return result;
  }

  public boolean isReadOnly() {
    return this.readOnly;
  }

  public Term getDefaultValue() {
    return this.defaultValue;
  }
}
