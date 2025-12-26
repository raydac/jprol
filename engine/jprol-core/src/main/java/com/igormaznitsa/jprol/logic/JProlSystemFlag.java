package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;
import static com.igormaznitsa.jprol.data.Terms.FALSE;
import static com.igormaznitsa.jprol.data.Terms.TRUE;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.data.Terms.newStruct;

import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;

public enum JProlSystemFlag {
  ADDRESS_BIT(true, Terms.newAtom("address_bit", SourcePosition.UNKNOWN),
      Terms.newLong(ProlUtils.getJvmBitness(), SourcePosition.UNKNOWN)),
  ALLOW_VARIABLE_NAME_AS_FUNCTOR(true,
      Terms.newAtom("allow_variable_name_as_functor", SourcePosition.UNKNOWN), FALSE),
  OS(true, Terms.newAtom("os"), Terms.newAtom(System.getProperty("os.name", "unknown"))),
  ARCH(true, Terms.newAtom("arch"), Terms.newAtom(System.getProperty("os.arch", "unknown"))),
  BOUNDED(true, Terms.newAtom("bounded"), TRUE),
  TRACE(false, Terms.newAtom("trace"), FALSE),
  DIALECT(true, Terms.newAtom("dialect"), Terms.newAtom("jprol")),
  ENCODING(false, Terms.newAtom("encoding"), Terms.newAtom(StandardCharsets.UTF_8.name())),
  GC(true, Terms.newAtom("gc"), TRUE),
  MAX_ARITY(true, Terms.newAtom("max_arity"), Terms.newLong(Integer.MAX_VALUE)),
  MAX_INTEGER(true, Terms.newAtom("max_integer"), Terms.newLong(Long.MAX_VALUE)),
  MIN_INTEGER(true, Terms.newAtom("min_integer"), Terms.newLong(Long.MIN_VALUE)),
  SHARE_KNOWLEDGE_BASE(false, Terms.newAtom("share_knowledge_base"), FALSE),
  CPU_COUNT(true, Terms.newAtom("cpu_count"),
      Terms.newLong(Runtime.getRuntime().availableProcessors())),
  UNKNOWN(false, Terms.newAtom("unknown"), UndefinedPredicateBehavior.ERROR.getTerm()),
  HOME(true, Terms.newAtom("home"),
      Terms.newAtom(System.getProperty("user.home", ""), SourcePosition.UNKNOWN)),
  VERIFY(false, Terms.newAtom("verify", SourcePosition.UNKNOWN), TRUE),
  VERSION_DATA(true, Terms.newAtom("version_data", SourcePosition.UNKNOWN),
      newStruct(
          newAtom("jprol", SourcePosition.UNKNOWN),
          newLong(2, SourcePosition.UNKNOWN),
          newLong(2, SourcePosition.UNKNOWN),
          newLong(2, SourcePosition.UNKNOWN),
          NULL_LIST)
  );

  public static final List<JProlSystemFlag> VALUES =
      List.of(JProlSystemFlag.values());
  private final Term nameTerm;
  private final Term defaultValue;
  private final boolean readOnly;

  JProlSystemFlag(final boolean readOnly, final Term name, final Term defaultValue) {
    this.nameTerm = name;
    this.readOnly = readOnly;
    this.defaultValue = defaultValue;
  }

  public static Optional<JProlSystemFlag> find(final Term term) {
    final Term thatTerm = term.tryGround();
    if (thatTerm.getTermType() != TermType.ATOM) {
      return Optional.empty();
    }
    return JProlSystemFlag.VALUES.stream().filter(x -> term.unifyTo(x.nameTerm))
        .findFirst();
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
