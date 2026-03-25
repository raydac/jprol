package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;
import static com.igormaznitsa.jprol.data.Terms.FALSE;
import static com.igormaznitsa.jprol.data.Terms.TRUE;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.logic.JProlContext.VERSION_MAJOR;
import static com.igormaznitsa.jprol.logic.JProlContext.VERSION_MINOR;
import static com.igormaznitsa.jprol.logic.JProlContext.VERSION_PATCH;

import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public enum JProlSystemFlag {
  ADDRESS_BITS(true, Terms.newAtom("address_bits", SourcePosition.UNKNOWN),
      Terms.newLong(ProlUtils.getJvmBitness(), SourcePosition.UNKNOWN), null,
      "Bitness of the Java VM."),
  ALLOW_LIBRARY_SIGNATURE_CONFLICT(false, Terms.newAtom("allow_library_signarute_conflict"), FALSE,
      null,
      "Allow definitions of predicates with same signatures which already presented in registered libraries"),
  OS(true, Terms.newAtom("os"), Terms.newAtom(System.getProperty("os.name", "unknown")),
      null, "OS name."),
  ARCH(true, Terms.newAtom("arch"), Terms.newAtom(System.getProperty("os.arch", "unknown")),
      null, "Identifier for the hardware and operating system JProl is running on."),
  BOUNDED(true, Terms.newAtom("bounded"), TRUE,
      null, "If true, integer representation is bound by min_integer and max_integer."),
  TRACE(false, Terms.newAtom("trace"), FALSE, null, "Turn on tracing of internal calls."),
  DIALECT(true, Terms.newAtom("dialect"), Terms.newAtom("jprol"), null, "Info about the dialect."),
  ENCODING(false, Terms.newAtom("encoding"), Terms.newAtom(StandardCharsets.UTF_8.name()),
      null, "Text encoding charset name"),
  GC(true, Terms.newAtom("gc"), FALSE,
      null,
      "If true (default), the garbage collector is active. Added for compatibility with another prolog systems."),
  MAX_ARITY(true, Terms.newAtom("max_arity"), Terms.newLong(Integer.MAX_VALUE),
      null, "flag describing there is no maximum arity to compound terms."),
  MAX_INTEGER(true, Terms.newAtom("max_integer"), Terms.newLong(Long.MAX_VALUE),
      null, "Maximum integer value if integers are bounded."),
  MIN_INTEGER(true, Terms.newAtom("min_integer"), Terms.newLong(Long.MIN_VALUE),
      null, "Minimum integer value if integers are bounded."),
  SHARE_KNOWLEDGE_BASE(false, Terms.newAtom("share_knowledge_base"), FALSE,
      null, "Flags shows that knowledge base should be shared with child contexts."),
  CPU_COUNT(true, Terms.newAtom("cpu_count"),
      Terms.newLong(Runtime.getRuntime().availableProcessors()),
      null, "Number of physical CPUs or cores in the system."),
  MAX_PROVE_STACK_DEPTH(false, Terms.newAtom("max_prove_stack_depth"), Terms.newLong(1_000L),
      x -> {
        final Term term = x.tryGround();
        if (term instanceof NumericTerm) {
          final int depth = term.toNumber().intValue();
          return depth > 0;
        } else {
          return false;
        }
      }, "Max prove stack depth."),
  UNKNOWN(false, Terms.newAtom("unknown"), UndefinedPredicateBehavior.ERROR.getTerm(),
      x -> {
        final Term term = x.tryGround();
        return UndefinedPredicateBehavior.find(term.getText()).isPresent();
      }, "Behavior for unknown predicate."),
  HOME(true, Terms.newAtom("home"),
      Terms.newAtom(System.getProperty("user.home", ""), SourcePosition.UNKNOWN),
      null, "Path to the user home folder."),
  VERIFY(false, Terms.newAtom("verify", SourcePosition.UNKNOWN), TRUE,
      null,
      "If true then every library predicate will be verified for its arguments by internal validator before call."),
  VERSION_DATA(true, Terms.newAtom("version_data", SourcePosition.UNKNOWN),
      newStruct(
          newAtom("jprol", SourcePosition.UNKNOWN),
          newLong(VERSION_MAJOR, SourcePosition.UNKNOWN),
          newLong(VERSION_MINOR, SourcePosition.UNKNOWN),
          newLong(VERSION_PATCH, SourcePosition.UNKNOWN),
          NULL_LIST), null, "Version of the JProl engine."
  );

  public static final List<JProlSystemFlag> VALUES =
      List.of(JProlSystemFlag.values());
  private final String reference;
  private final Term nameTerm;
  private final Term defaultValue;
  private final boolean readOnly;
  private final Predicate<Term> valueValidator;

  JProlSystemFlag(final boolean readOnly,
                  final Term name,
                  final Term defaultValue,
                  final Predicate<Term> valueValidator,
                  final String reference) {
    this.nameTerm = name;
    this.readOnly = readOnly;
    this.defaultValue = defaultValue;
    this.reference = reference;
    this.valueValidator = valueValidator == null ? (x -> true) : valueValidator;
  }

  public Predicate<Term> getValueValidator() {
    return this.valueValidator;
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
