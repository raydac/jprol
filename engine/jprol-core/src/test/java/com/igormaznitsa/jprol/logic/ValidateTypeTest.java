package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newDouble;
import static com.igormaznitsa.jprol.data.Terms.newList;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.data.Terms.newVar;
import static com.igormaznitsa.jprol.logic.ValidateType.ARITY;
import static com.igormaznitsa.jprol.logic.ValidateType.ATOM;
import static com.igormaznitsa.jprol.logic.ValidateType.ATOMIC;
import static com.igormaznitsa.jprol.logic.ValidateType.BODY;
import static com.igormaznitsa.jprol.logic.ValidateType.CALLABLE;
import static com.igormaznitsa.jprol.logic.ValidateType.CHAR;
import static com.igormaznitsa.jprol.logic.ValidateType.CHAR_LIST;
import static com.igormaznitsa.jprol.logic.ValidateType.CODE_LIST;
import static com.igormaznitsa.jprol.logic.ValidateType.COMPOUND;
import static com.igormaznitsa.jprol.logic.ValidateType.COMPOUND_TERM;
import static com.igormaznitsa.jprol.logic.ValidateType.EVALUABLE;
import static com.igormaznitsa.jprol.logic.ValidateType.FLOAT;
import static com.igormaznitsa.jprol.logic.ValidateType.FORMAT;
import static com.igormaznitsa.jprol.logic.ValidateType.GOAL;
import static com.igormaznitsa.jprol.logic.ValidateType.HEAD;
import static com.igormaznitsa.jprol.logic.ValidateType.INTEGER;
import static com.igormaznitsa.jprol.logic.ValidateType.IO_MODE;
import static com.igormaznitsa.jprol.logic.ValidateType.LIST;
import static com.igormaznitsa.jprol.logic.ValidateType.NAME;
import static com.igormaznitsa.jprol.logic.ValidateType.NONEMPTY_LIST;
import static com.igormaznitsa.jprol.logic.ValidateType.NUMBER;
import static com.igormaznitsa.jprol.logic.ValidateType.OPERATOR_SPECIFIER;
import static com.igormaznitsa.jprol.logic.ValidateType.PREDICATE_INDICATOR;
import static com.igormaznitsa.jprol.logic.ValidateType.STRING;
import static com.igormaznitsa.jprol.logic.ValidateType.TERM;
import static com.igormaznitsa.jprol.logic.ValidateType.TRIGGER_EVENT;
import static com.igormaznitsa.jprol.logic.ValidateType.VAR;
import static java.util.Locale.ROOT;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toSet;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlRepresentationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class ValidateTypeTest {

  private static final Term FOO = newAtom("foo");
  private static final Term CHAR_A = newAtom("a");
  private static final Term CHAR_B = newAtom("b");
  private static final Term TWO_CHARS = newAtom("ab");
  private static final Term LONG_TWO = newLong(2L);
  private static final Term LONG_ZERO = newLong(0L);
  private static final Term LONG_NEGATIVE = newLong(-1L);
  private static final Term LONG_MAX_ARITY = newLong(Integer.MAX_VALUE);
  private static final Term LONG_OVER_ARITY = newLong((long) Integer.MAX_VALUE + 1L);
  private static final Term CODE_A = newLong(65L);
  private static final Term CODE_B = newLong(66L);
  private static final Term WIDE_CODE = newLong(0x10000L);
  private static final Term PI = newDouble(1.5d);
  private static final Term EMPTY = newList();
  private static final Term ONE_ELEMENT = list(FOO);
  private static final Term CHARS = list(CHAR_A, CHAR_B);
  private static final Term CODES = list(CODE_A, CODE_B);
  private static final Term OPEN_CHARS = newList(CHAR_A, newVar("Tail"));
  private static final Term STRUCTURE = newStruct(FOO, CHAR_A);
  private static final Term ZERO_ARITY = newStruct(newAtom("p"));
  private static final Term UNBOUND = newVar("X");
  private static final Term BOUND_ATOM = boundTo(FOO);
  private static final Term CHAINED_ATOM = chainedTo(FOO);
  private static final Term FREE_STRUCTURE = newStruct(FOO, newVar("Y"));
  private static final Term INDICATOR = indicator(FOO, LONG_TWO);
  private static final Term BOUND_INDICATOR = boundTo(INDICATOR);
  private static final Term EVALUABLE_GOAL = evaluableGoal();

  private static Stream<TypeCase> typeCases() {
    return Stream.of(
        universalTerms(),
        atoms(),
        formats(),
        names(),
        arities(),
        atomics(),
        charLists(),
        codeLists(),
        strings(),
        numbers(),
        callables(),
        compounds(),
        characters(),
        events(),
        indicators(),
        specifiers(),
        variables()
    ).flatMap(cases -> cases);
  }

  private static Stream<TypeCase> universalTerms() {
    return Stream.of(FOO, LONG_TWO, PI, EMPTY, ONE_ELEMENT, STRUCTURE, UNBOUND, FREE_STRUCTURE)
        .map(term -> ok(TERM, term.getTermType().name(), term));
  }

  private static Stream<TypeCase> atoms() {
    return Stream.of(
        ok(ATOM, "atom", FOO),
        ok(ATOM, "bound atom", BOUND_ATOM),
        ok(ATOM, "chained atom", CHAINED_ATOM),
        bad(ATOM, "integer", LONG_TWO, ProlTypeErrorException.class),
        bad(ATOM, "float", PI, ProlTypeErrorException.class),
        bad(ATOM, "list", EMPTY, ProlTypeErrorException.class),
        bad(ATOM, "structure", STRUCTURE, ProlTypeErrorException.class),
        bad(ATOM, "unbound", UNBOUND, ProlInstantiationErrorException.class)
    );
  }

  private static Stream<TypeCase> formats() {
    return Stream.of(
        ok(FORMAT, "atom", FOO),
        ok(FORMAT, "empty list", EMPTY),
        ok(FORMAT, "char list", CHARS),
        ok(FORMAT, "list of structures", list(STRUCTURE)),
        bad(FORMAT, "integer", LONG_TWO, ProlTypeErrorException.class),
        bad(FORMAT, "structure", STRUCTURE, ProlTypeErrorException.class),
        bad(FORMAT, "unbound", UNBOUND, ProlInstantiationErrorException.class)
    );
  }

  private static Stream<TypeCase> names() {
    return Stream.of(
        ok(NAME, "atom", FOO),
        ok(NAME, "integer", LONG_TWO),
        ok(NAME, "float", PI),
        ok(NAME, "empty list", EMPTY),
        bad(NAME, "list", ONE_ELEMENT, ProlTypeErrorException.class),
        bad(NAME, "structure", STRUCTURE, ProlTypeErrorException.class),
        bad(NAME, "unbound", UNBOUND, ProlTypeErrorException.class),
        bad(NAME, "free structure", FREE_STRUCTURE, ProlTypeErrorException.class)
    );
  }

  private static Stream<TypeCase> arities() {
    return Stream.of(
        ok(ARITY, "zero", LONG_ZERO),
        ok(ARITY, "two", LONG_TWO),
        ok(ARITY, "max int", LONG_MAX_ARITY),
        ok(ARITY, "bound arity", boundTo(LONG_TWO)),
        bad(ARITY, "negative", LONG_NEGATIVE, ProlDomainErrorException.class),
        bad(ARITY, "above max int", LONG_OVER_ARITY, ProlDomainErrorException.class),
        bad(ARITY, "float", PI, ProlTypeErrorException.class),
        bad(ARITY, "atom", FOO, ProlTypeErrorException.class),
        bad(ARITY, "unbound", UNBOUND, ProlTypeErrorException.class)
    );
  }

  private static Stream<TypeCase> atomics() {
    return Stream.of(
        ok(ATOMIC, "atom", FOO),
        ok(ATOMIC, "integer", LONG_TWO),
        ok(ATOMIC, "float", PI),
        ok(ATOMIC, "empty list", EMPTY),
        bad(ATOMIC, "list", ONE_ELEMENT, ProlTypeErrorException.class),
        bad(ATOMIC, "structure", STRUCTURE, ProlTypeErrorException.class),
        bad(ATOMIC, "unbound", UNBOUND, ProlTypeErrorException.class)
    );
  }

  private static Stream<TypeCase> charLists() {
    return Stream.of(
        ok(CHAR_LIST, "empty", EMPTY),
        ok(CHAR_LIST, "chars", CHARS),
        ok(CHAR_LIST, "open tail", OPEN_CHARS),
        bad(CHAR_LIST, "codes", CODES, ProlTypeErrorException.class),
        bad(CHAR_LIST, "long char", list(TWO_CHARS), ProlRepresentationErrorException.class),
        bad(CHAR_LIST, "atom", FOO, ProlTypeErrorException.class),
        bad(CHAR_LIST, "unbound", UNBOUND, ProlInstantiationErrorException.class)
    );
  }

  private static Stream<TypeCase> codeLists() {
    return Stream.of(
        ok(CODE_LIST, "empty", EMPTY),
        ok(CODE_LIST, "codes", CODES),
        ok(CODE_LIST, "open tail", newList(CODE_A, newVar("Codes"))),
        bad(CODE_LIST, "chars", CHARS, ProlRepresentationErrorException.class),
        bad(CODE_LIST, "code above 16 bits", list(WIDE_CODE),
            ProlRepresentationErrorException.class),
        bad(CODE_LIST, "negative code", list(LONG_NEGATIVE),
            ProlRepresentationErrorException.class),
        bad(CODE_LIST, "atom", FOO, ProlTypeErrorException.class),
        bad(CODE_LIST, "unbound", UNBOUND, ProlInstantiationErrorException.class)
    );
  }

  private static Stream<TypeCase> strings() {
    return Stream.of(
        ok(STRING, "empty", EMPTY),
        ok(STRING, "codes", CODES),
        ok(STRING, "chars", CHARS),
        ok(STRING, "unicode code point", list(WIDE_CODE)),
        bad(STRING, "long char", list(TWO_CHARS), ProlDomainErrorException.class),
        bad(STRING, "negative code", list(LONG_NEGATIVE), ProlDomainErrorException.class),
        bad(STRING, "atom", FOO, ProlTypeErrorException.class),
        bad(STRING, "unbound", UNBOUND, ProlTypeErrorException.class)
    );
  }

  private static Stream<TypeCase> numbers() {
    return Stream.of(
        ok(FLOAT, "float", PI),
        ok(FLOAT, "bound float", boundTo(PI)),
        bad(FLOAT, "integer", LONG_TWO, ProlTypeErrorException.class),
        bad(FLOAT, "atom", FOO, ProlTypeErrorException.class),
        bad(FLOAT, "unbound", UNBOUND, ProlTypeErrorException.class),
        ok(INTEGER, "integer", LONG_TWO),
        ok(INTEGER, "negative", LONG_NEGATIVE),
        ok(INTEGER, "bound integer", boundTo(LONG_TWO)),
        bad(INTEGER, "float", PI, ProlTypeErrorException.class),
        bad(INTEGER, "atom", FOO, ProlTypeErrorException.class),
        bad(INTEGER, "unbound", UNBOUND, ProlTypeErrorException.class),
        ok(NUMBER, "integer", LONG_TWO),
        ok(NUMBER, "float", PI),
        bad(NUMBER, "atom", FOO, ProlTypeErrorException.class),
        bad(NUMBER, "unbound", UNBOUND, ProlTypeErrorException.class)
    );
  }

  private static Stream<TypeCase> callables() {
    return Stream.of(
        ok(GOAL, "atom", FOO),
        ok(GOAL, "structure", STRUCTURE),
        bad(GOAL, "integer", LONG_TWO, ProlTypeErrorException.class),
        bad(GOAL, "list", EMPTY, ProlTypeErrorException.class),
        bad(GOAL, "unbound", UNBOUND, ProlTypeErrorException.class),
        ok(CALLABLE, "atom", FOO),
        ok(CALLABLE, "structure", STRUCTURE),
        ok(CALLABLE, "bound goal", BOUND_ATOM),
        bad(CALLABLE, "integer", LONG_TWO, ProlTypeErrorException.class),
        bad(CALLABLE, "list", EMPTY, ProlTypeErrorException.class),
        bad(CALLABLE, "unbound", UNBOUND, ProlInstantiationErrorException.class),
        ok(HEAD, "atom", FOO),
        ok(HEAD, "structure", STRUCTURE),
        ok(HEAD, "zero arity", ZERO_ARITY),
        bad(HEAD, "integer", LONG_TWO, ProlTypeErrorException.class),
        bad(HEAD, "list", EMPTY, ProlTypeErrorException.class),
        bad(HEAD, "unbound", UNBOUND, ProlTypeErrorException.class),
        ok(BODY, "atom", FOO),
        ok(BODY, "structure", STRUCTURE),
        bad(BODY, "integer", LONG_TWO, ProlTypeErrorException.class),
        bad(BODY, "list", EMPTY, ProlTypeErrorException.class),
        bad(BODY, "unbound", UNBOUND, ProlTypeErrorException.class),
        ok(EVALUABLE, "integer", LONG_TWO),
        ok(EVALUABLE, "float", PI),
        ok(EVALUABLE, "evaluable structure", EVALUABLE_GOAL),
        bad(EVALUABLE, "plain structure", STRUCTURE, ProlTypeErrorException.class),
        bad(EVALUABLE, "atom", FOO, ProlTypeErrorException.class),
        bad(EVALUABLE, "unbound", UNBOUND, ProlTypeErrorException.class)
    );
  }

  private static Stream<TypeCase> compounds() {
    return Stream.of(LIST, NONEMPTY_LIST, COMPOUND, COMPOUND_TERM).flatMap(type -> {
      if (type == LIST) {
        return Stream.of(
            ok(LIST, "empty", EMPTY),
            ok(LIST, "one element", ONE_ELEMENT),
            ok(LIST, "open tail", OPEN_CHARS),
            ok(LIST, "bound list", boundTo(ONE_ELEMENT)),
            bad(LIST, "atom", FOO, ProlTypeErrorException.class),
            bad(LIST, "structure", STRUCTURE, ProlTypeErrorException.class),
            bad(LIST, "unbound", UNBOUND, ProlTypeErrorException.class)
        );
      }
      if (type == NONEMPTY_LIST) {
        return Stream.of(
            ok(NONEMPTY_LIST, "one element", ONE_ELEMENT),
            ok(NONEMPTY_LIST, "open tail", OPEN_CHARS),
            bad(NONEMPTY_LIST, "empty", EMPTY, ProlDomainErrorException.class),
            bad(NONEMPTY_LIST, "atom", FOO, ProlTypeErrorException.class),
            bad(NONEMPTY_LIST, "unbound", UNBOUND, ProlTypeErrorException.class)
        );
      }
      return Stream.of(
          ok(type, "list", ONE_ELEMENT),
          ok(type, "empty list", EMPTY),
          ok(type, "structure", STRUCTURE),
          ok(type, "bound structure", boundTo(STRUCTURE)),
          bad(type, "atom", FOO, ProlTypeErrorException.class),
          bad(type, "integer", LONG_TWO, ProlTypeErrorException.class),
          bad(type, "unbound", UNBOUND, ProlInstantiationErrorException.class)
      );
    });
  }

  private static Stream<TypeCase> characters() {
    return Stream.of(
        ok(CHAR, "one char", CHAR_A),
        bad(CHAR, "two chars", TWO_CHARS, ProlTypeErrorException.class),
        bad(CHAR, "empty atom", newAtom(""), ProlTypeErrorException.class),
        bad(CHAR, "integer", LONG_TWO, ProlTypeErrorException.class),
        bad(CHAR, "unbound", UNBOUND, ProlTypeErrorException.class)
    );
  }

  private static Stream<TypeCase> events() {
    return Stream.of(
        ok(TRIGGER_EVENT, "onassert", newAtom("onassert")),
        ok(TRIGGER_EVENT, "onretract", newAtom("onretract")),
        ok(TRIGGER_EVENT, "onassertretract", newAtom("onassertretract")),
        bad(TRIGGER_EVENT, "other atom", FOO, ProlDomainErrorException.class),
        bad(TRIGGER_EVENT, "different case", newAtom("onAssert"), ProlDomainErrorException.class),
        bad(TRIGGER_EVENT, "integer", LONG_TWO, ProlDomainErrorException.class),
        bad(TRIGGER_EVENT, "unbound", UNBOUND, ProlInstantiationErrorException.class),
        ok(IO_MODE, "read", newAtom("read")),
        ok(IO_MODE, "write", newAtom("write")),
        ok(IO_MODE, "append", newAtom("append")),
        bad(IO_MODE, "other atom", FOO, ProlDomainErrorException.class),
        bad(IO_MODE, "different case", newAtom("Read"), ProlDomainErrorException.class),
        bad(IO_MODE, "integer", LONG_TWO, ProlDomainErrorException.class),
        bad(IO_MODE, "unbound", UNBOUND, ProlDomainErrorException.class)
    );
  }

  private static Stream<TypeCase> indicators() {
    return Stream.of(
        ok(PREDICATE_INDICATOR, "foo/2", INDICATOR),
        ok(PREDICATE_INDICATOR, "foo/0", indicator(FOO, LONG_ZERO)),
        ok(PREDICATE_INDICATOR, "max arity", indicator(FOO, LONG_MAX_ARITY)),
        ok(PREDICATE_INDICATOR, "zero-arity name", indicator(ZERO_ARITY, LONG_TWO)),
        ok(PREDICATE_INDICATOR, "free name", indicator(newVar("Name"), LONG_TWO)),
        ok(PREDICATE_INDICATOR, "free arity", indicator(FOO, newVar("Arity"))),
        ok(PREDICATE_INDICATOR, "free pair", indicator(newVar("Name"), newVar("Arity"))),
        ok(PREDICATE_INDICATOR, "bound indicator", BOUND_INDICATOR),
        bad(PREDICATE_INDICATOR, "atom", FOO, ProlTypeErrorException.class),
        bad(PREDICATE_INDICATOR, "integer name", indicator(LONG_TWO, LONG_ZERO),
            ProlTypeErrorException.class),
        bad(PREDICATE_INDICATOR, "structured name", indicator(STRUCTURE, LONG_ZERO),
            ProlTypeErrorException.class),
        bad(PREDICATE_INDICATOR, "atom arity", indicator(FOO, FOO), ProlTypeErrorException.class),
        bad(PREDICATE_INDICATOR, "float arity", indicator(FOO, PI), ProlTypeErrorException.class),
        bad(PREDICATE_INDICATOR, "negative arity", indicator(FOO, LONG_NEGATIVE),
            ProlDomainErrorException.class),
        bad(PREDICATE_INDICATOR, "arity above max int", indicator(FOO, LONG_OVER_ARITY),
            ProlRepresentationErrorException.class),
        bad(PREDICATE_INDICATOR, "bound atom", BOUND_ATOM, ProlTypeErrorException.class),
        bad(PREDICATE_INDICATOR, "unbound", UNBOUND, ProlInstantiationErrorException.class)
    );
  }

  private static Stream<TypeCase> specifiers() {
    return Stream.concat(
        Stream.of("xf", "yf", "fx", "fy", "xfx", "xfy", "yfx")
            .map(name -> ok(OPERATOR_SPECIFIER, name, newAtom(name))),
        Stream.of(
            bad(OPERATOR_SPECIFIER, "unknown", newAtom("xfz"), ProlDomainErrorException.class),
            bad(OPERATOR_SPECIFIER, "different case", newAtom("XFX"),
                ProlDomainErrorException.class),
            bad(OPERATOR_SPECIFIER, "integer", LONG_TWO, ProlDomainErrorException.class),
            bad(OPERATOR_SPECIFIER, "unbound", UNBOUND, ProlDomainErrorException.class)
        )
    );
  }

  private static Stream<TypeCase> variables() {
    return Stream.of(
        ok(VAR, "unbound", UNBOUND),
        ok(VAR, "ground atom", FOO),
        ok(VAR, "ground structure", STRUCTURE),
        ok(VAR, "ground list", ONE_ELEMENT),
        bad(VAR, "structure with a free variable", FREE_STRUCTURE, ProlTypeErrorException.class),
        bad(VAR, "open list", OPEN_CHARS, ProlTypeErrorException.class)
    );
  }

  private static TypeCase ok(final ValidateType type, final String label, final Term term) {
    return new TypeCase(type, type.getText() + " accepts " + label, term, true, null);
  }

  private static TypeCase bad(
      final ValidateType type,
      final String label,
      final Term term,
      final Class<? extends ProlException> error
  ) {
    return new TypeCase(type, type.getText() + " rejects " + label, term, false, error);
  }

  private static Term list(final Term... items) {
    Term tail = newList();
    for (int index = items.length - 1; index >= 0; index--) {
      tail = newList(items[index], tail);
    }
    return tail;
  }

  private static Term indicator(final Term name, final Term arity) {
    return newStruct(newAtom("/"), name, arity);
  }

  private static Term boundTo(final Term value) {
    final TermVar variable = newVar("Bound");
    variable.setValue(value);
    return variable;
  }

  private static Term chainedTo(final Term value) {
    final TermVar outer = newVar("Outer");
    final TermVar inner = newVar("Inner");
    outer.setValue(inner);
    inner.setValue(value);
    return outer;
  }

  private static Term evaluableGoal() {
    final PredicateInvoker invoker = mock(PredicateInvoker.class);
    when(invoker.isEvaluable()).thenReturn(true);
    final TermStruct goal = newStruct(newAtom("abs"), newLong(1L));
    goal.setPredicateProcessor(invoker);
    return goal;
  }

  @Test
  void everyTextRoundTripsIgnoringCaseAndSpace() {
    final Set<String> texts = EnumSet.allOf(ValidateType.class).stream()
        .map(ValidateType::getText)
        .collect(toSet());
    assertEquals(ValidateType.values().length, texts.size());

    texts.forEach(text -> {
      assertEquals(text, ValidateType.find(text).orElseThrow().getText());
      assertEquals(text,
          ValidateType.find("  " + text.toUpperCase(ROOT) + " ").orElseThrow().getText());
    });
    assertTrue(ValidateType.find("no-such-type").isEmpty());
    assertTrue(ValidateType.find("   ").isEmpty());
  }

  @Test
  void everyTypeHasAnAcceptedTermAndARejectedTerm() {
    final Map<ValidateType, List<TypeCase>> grouped =
        typeCases().collect(groupingBy(TypeCase::getType));
    assertEquals(EnumSet.allOf(ValidateType.class), grouped.keySet());

    grouped.forEach((type, rows) -> {
      assertTrue(rows.stream().anyMatch(TypeCase::isAccepted), type::getText);
      if (type != TERM) {
        assertTrue(rows.stream().anyMatch(row -> !row.isAccepted()), type::getText);
        assertTrue(
            rows.stream().filter(row -> !row.isAccepted()).allMatch(row -> row.getError() != null));
      }
    });
  }

  @ParameterizedTest(name = "{0}")
  @MethodSource("typeCases")
  void typeAcceptsOrRejects(final TypeCase typeCase) {
    assertEquals(typeCase.isAccepted(), typeCase.getType().test(typeCase.getTerm()),
        typeCase::getLabel);

    final ProlException reported = typeCase.getType().findException(typeCase.getTerm());
    if (typeCase.isAccepted()) {
      assertNull(reported, typeCase::getLabel);
      return;
    }
    assertInstanceOf(typeCase.getError(), reported, typeCase::getLabel);
  }

  private static final class TypeCase {
    private final ValidateType type;
    private final String label;
    private final Term term;
    private final boolean accepted;
    private final Class<? extends ProlException> error;

    private TypeCase(
        final ValidateType type,
        final String label,
        final Term term,
        final boolean accepted,
        final Class<? extends ProlException> error
    ) {
      this.type = type;
      this.label = label;
      this.term = term;
      this.accepted = accepted;
      this.error = error;
    }

    private ValidateType getType() {
      return this.type;
    }

    private String getLabel() {
      return this.label;
    }

    private Term getTerm() {
      return this.term;
    }

    private boolean isAccepted() {
      return this.accepted;
    }

    private Class<? extends ProlException> getError() {
      return this.error;
    }

    @Override
    public String toString() {
      return this.label;
    }
  }
}
