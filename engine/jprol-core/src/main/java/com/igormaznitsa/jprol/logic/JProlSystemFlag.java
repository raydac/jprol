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
  ADDRESS_BITS(true, Terms.newAtom("address_bits", SourcePosition.UNKNOWN),
      Terms.newLong(ProlUtils.getJvmBitness(), SourcePosition.UNKNOWN), "Bitness of the Java VM."),
  ALLOW_VARIABLE_NAME_AS_FUNCTOR(true,
      Terms.newAtom("allow_variable_name_as_functor", SourcePosition.UNKNOWN), FALSE,
      "Allows to use variable names as functors."),
  OS(true, Terms.newAtom("os"), Terms.newAtom(System.getProperty("os.name", "unknown")),
      "OS name."),
  ARCH(true, Terms.newAtom("arch"), Terms.newAtom(System.getProperty("os.arch", "unknown")),
      "Identifier for the hardware and operating system JProl is running on."),
  BOUNDED(true, Terms.newAtom("bounded"), TRUE,
      "If true, integer representation is bound by min_integer and max_integer."),
  TRACE(false, Terms.newAtom("trace"), FALSE, "Turn on tracing of internal calls."),
  DIALECT(true, Terms.newAtom("dialect"), Terms.newAtom("jprol"), "Info about the dialect."),
  ENCODING(false, Terms.newAtom("encoding"), Terms.newAtom(StandardCharsets.UTF_8.name()),
      "Text encoding charset name"),
  GC(true, Terms.newAtom("gc"), FALSE,
      "If true (default), the garbage collector is active. Added for compatibility with another prolog systems."),
  MAX_ARITY(true, Terms.newAtom("max_arity"), Terms.newLong(Integer.MAX_VALUE),
      "flag describing there is no maximum arity to compound terms."),
  MAX_INTEGER(true, Terms.newAtom("max_integer"), Terms.newLong(Long.MAX_VALUE),
      "Maximum integer value if integers are bounded."),
  MIN_INTEGER(true, Terms.newAtom("min_integer"), Terms.newLong(Long.MIN_VALUE),
      "Minimum integer value if integers are bounded."),
  SHARE_KNOWLEDGE_BASE(false, Terms.newAtom("share_knowledge_base"), FALSE,
      "Flags shows that knowledge base should be shared with child contexts."),
  CPU_COUNT(true, Terms.newAtom("cpu_count"),
      Terms.newLong(Runtime.getRuntime().availableProcessors()),
      "Number of physical CPUs or cores in the system."),
  UNKNOWN(false, Terms.newAtom("unknown"), UndefinedPredicateBehavior.ERROR.getTerm(),
      "Behavior for unknown predicate."),
  HOME(true, Terms.newAtom("home"),
      Terms.newAtom(System.getProperty("user.home", ""), SourcePosition.UNKNOWN),
      "Path to the user home folder."),
  VERIFY(false, Terms.newAtom("verify", SourcePosition.UNKNOWN), TRUE,
      "If true then every library predicate will be verified for its arguments by internal validator before call."),
  VERSION_DATA(true, Terms.newAtom("version_data", SourcePosition.UNKNOWN),
      newStruct(
          newAtom("jprol", SourcePosition.UNKNOWN),
          newLong(3, SourcePosition.UNKNOWN),
          newLong(0, SourcePosition.UNKNOWN),
          newLong(0, SourcePosition.UNKNOWN),
          NULL_LIST), "Version of the JProl engine."
  );

  public static final List<JProlSystemFlag> VALUES =
      List.of(JProlSystemFlag.values());
  private final String reference;
  private final Term nameTerm;
  private final Term defaultValue;
  private final boolean readOnly;

  JProlSystemFlag(final boolean readOnly, final Term name, final Term defaultValue,
                  final String reference) {
    this.nameTerm = name;
    this.readOnly = readOnly;
    this.defaultValue = defaultValue;
    this.reference = reference;
  }

  public static Optional<JProlSystemFlag> find(final Term term) {
    final Term thatTerm = term.tryGround();
    if (thatTerm.getTermType() != TermType.ATOM) {
      return Optional.empty();
    }
    return JProlSystemFlag.VALUES.stream().filter(x -> term.unifyWith(x.nameTerm))
        .findFirst();
  }

  public String getReference() {
    return this.reference;
  }

  public String asText() {
    return this.nameTerm.getText();
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
