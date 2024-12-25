package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.Terms.FALSE;
import static com.igormaznitsa.jprol.data.Terms.NULL_LIST;
import static com.igormaznitsa.jprol.data.Terms.TRUE;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static java.util.Collections.unmodifiableList;

import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.utils.Utils;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Optional;

public enum JProlSystemFlag {
  ADDRESS_BIT(true, Terms.newAtom("address_bit", SourcePosition.UNKNOWN),
      Terms.newLong(Utils.getJvmBitness(), SourcePosition.UNKNOWN)),
  ALLOW_VARIABLE_NAME_AS_FUNCTOR(true,
      Terms.newAtom("allow_variable_name_as_functor", SourcePosition.UNKNOWN), FALSE),
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
  CPU_COUNT(true, Terms.newAtom("cpu_count"),
      Terms.newLong(Runtime.getRuntime().availableProcessors())),
  UNKNOWN(false, Terms.newAtom("unknown"), UndefinedPredicateBehavior.ERROR.getTerm()),
  HOME(true, Terms.newAtom("home"),
      Terms.newAtom(System.getProperty("user.home", ""), SourcePosition.UNKNOWN)),
  VERIFY(false, Terms.newAtom("verify", SourcePosition.UNKNOWN), TRUE),
  VERSION_DATA(true, Terms.newAtom("version_data", SourcePosition.UNKNOWN),
      newStruct(newAtom("jprol", SourcePosition.UNKNOWN),
          new Term[] {newLong(2, SourcePosition.UNKNOWN), newLong(0, SourcePosition.UNKNOWN),
              newLong(1, SourcePosition.UNKNOWN), NULL_LIST}));

  private final Term nameTerm;
  private final Term defaultValue;
  private final boolean readOnly;

  // optimization because JDK make copy during values call
  public static final List<JProlSystemFlag> VALUES =
      unmodifiableList(Arrays.asList(JProlSystemFlag.values()));

  JProlSystemFlag(final boolean readOnly, final Term name, final Term defaultValue) {
    this.nameTerm = name;
    this.readOnly = readOnly;
    this.defaultValue = defaultValue;
  }

  public static Optional<JProlSystemFlag> find(final Term term) {
    final String termText =
        term.getTermType() != TermType.ATOM ? null : term.getText().toUpperCase(Locale.ENGLISH);

    Optional<JProlSystemFlag> result = Optional.empty();
    if (termText != null) {
      result = JProlSystemFlag.VALUES.stream().filter(x -> x.name().equals(termText))
          .findFirst();
    }
    return result;
  }

  public Term getNameTerm() {
    return this.nameTerm;
  }

  public boolean isReadOnly() {
    return this.readOnly;
  }

  public Term getDefaultValue() {
    return this.defaultValue;
  }
}
