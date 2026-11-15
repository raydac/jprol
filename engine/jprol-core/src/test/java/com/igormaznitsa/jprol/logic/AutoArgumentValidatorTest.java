package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newDouble;
import static com.igormaznitsa.jprol.data.Terms.newList;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.data.Terms.newVar;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlArgumentValidationException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.AutoArgumentValidator.ModificatorArgument;
import java.util.List;
import java.util.function.Predicate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AutoArgumentValidatorTest {

  private static Predicate<TermStruct> checker(
      final boolean assertMode,
      final int arity,
      final String... patterns
  ) {
    return AutoArgumentValidator.makeFor(arity, AutoArgumentValidator.parse(patterns), assertMode);
  }

  private static TermStruct goal(final Term... arguments) {
    return newStruct(newAtom("goal"), arguments);
  }

  @Test
  void everyModificatorTextRoundTrips() {
    for (final ValidateModificator modificator : ValidateModificator.values()) {
      assertEquals(modificator, ValidateModificator.find(modificator.getText()).orElseThrow());
      assertEquals(
          modificator,
          ValidateModificator.find(" " + modificator.getText() + " ").orElseThrow());
    }
    assertTrue(ValidateModificator.find(null).isEmpty());
    assertTrue(ValidateModificator.find("??").isEmpty());
  }

  @Test
  void parseReadsModificatorAndType() {
    final List<List<ModificatorArgument>> parsed = AutoArgumentValidator.parse(new String[] {
        null,
        " ++Atom , + list ",
        "?predicate_indicator,-term",
        null
    });

    assertEquals(2, parsed.size());
    assertEquals(ValidateModificator.GROUND, parsed.get(0).get(0).getModificator());
    assertEquals(ValidateType.ATOM, parsed.get(0).get(0).getArgType());
    assertEquals(ValidateModificator.SOFT_GROUND, parsed.get(0).get(1).getModificator());
    assertEquals(ValidateType.LIST, parsed.get(0).get(1).getArgType());
    assertEquals(ValidateModificator.MAY_UNBOUND_OR_GROUND, parsed.get(1).get(0).getModificator());
    assertEquals(ValidateType.PREDICATE_INDICATOR, parsed.get(1).get(0).getArgType());
    assertEquals(ValidateModificator.SOFT_UNBOUND, parsed.get(1).get(1).getModificator());
    assertEquals(ValidateType.TERM, parsed.get(1).get(1).getArgType());
  }

  @Test
  void parseRejectsUnknownModificatorAndUnknownType() {
    assertThrows(IllegalArgumentException.class,
        () -> AutoArgumentValidator.parse(new String[] {"%%atom"}));
    assertThrows(IllegalArgumentException.class,
        () -> AutoArgumentValidator.parse(new String[] {"++unknown"}));
    assertThrows(IllegalArgumentException.class,
        () -> AutoArgumentValidator.parse(new String[] {""}));
  }

  @Test
  void missingOrMismatchedPatternsAcceptEveryGoal() {
    assertTrue(AutoArgumentValidator.isAcceptAll(checker(true, 0, "++atom")));
    assertTrue(AutoArgumentValidator.isAcceptAll(checker(true, 2)));
    assertTrue(AutoArgumentValidator.isAcceptAll(checker(true, 1, "++atom,+list")));
    assertTrue(checker(true, 1, "++atom,+list").test(goal(newLong(1L))));
  }

  @Test
  void termModesThatCannotFailAreDropped() {
    assertTrue(AutoArgumentValidator.isAcceptAll(checker(true, 1, "?term")));
    assertTrue(AutoArgumentValidator.isAcceptAll(checker(true, 1, "@term")));
    assertTrue(AutoArgumentValidator.isAcceptAll(checker(true, 2, "-term,?term")));
    assertTrue(AutoArgumentValidator.isAcceptAll(checker(true, 1, "++atom", "?term")));
    assertTrue(checker(true, 1, "++atom", "?term").test(goal(newLong(1L))));
  }

  @Test
  void groundAndInstantiatedTermModesStillCheck() {
    final Predicate<TermStruct> ground = checker(false, 1, "++term");
    assertFalse(AutoArgumentValidator.isAcceptAll(ground));
    assertTrue(ground.test(goal(newAtom("a"))));
    assertTrue(ground.test(goal(newList())));
    assertFalse(ground.test(goal(newVar("X"))));
    assertFalse(ground.test(goal(newList(newAtom("a"), newVar("Tail")))));

    final Predicate<TermStruct> instantiated = checker(false, 1, "+term");
    assertTrue(instantiated.test(goal(newList(newAtom("a"), newVar("Tail")))));
    assertFalse(instantiated.test(goal(newVar("X"))));

    final Predicate<TermStruct> meta = checker(false, 1, ":term");
    assertTrue(meta.test(goal(newAtom("goal"))));
    assertTrue(meta.test(goal(newStruct(newAtom("g"), newAtom("a")))));
    assertFalse(meta.test(goal(newLong(1L))));
    assertFalse(meta.test(goal(newList())));
    assertFalse(meta.test(goal(newVar("X"))));
  }

  @ParameterizedTest
  @ValueSource(strings = {"?", "-", "@"})
  void optionalModesCheckTheTypeOnlyWhenTheArgumentIsBound(final String mode) {
    final Predicate<TermStruct> quiet = checker(false, 1, mode + "integer");
    assertTrue(quiet.test(goal(newVar("X"))));
    assertTrue(quiet.test(goal(newLong(3L))));
    assertFalse(quiet.test(goal(newAtom("a"))));
    assertFalse(quiet.test(goal(newDouble(3.0d))));

    final ProlTypeErrorException error = assertThrows(
        ProlTypeErrorException.class,
        () -> checker(true, 1, mode + "integer").test(goal(newAtom("a"))));
    assertEquals("integer", error.getValidType());
  }

  @Test
  void groundModeRejectsVariablesBeforeTheType() {
    assertTrue(checker(false, 1, "++integer").test(goal(newLong(3L))));
    assertThrows(
        ProlInstantiationErrorException.class,
        () -> checker(true, 1, "++integer").test(goal(newVar("X"))));

    final ProlTypeErrorException error = assertThrows(
        ProlTypeErrorException.class,
        () -> checker(true, 1, "++integer").test(goal(newDouble(3.0d))));
    assertEquals("integer", error.getValidType());
  }

  @Test
  void softGroundModeAllowsAPartialTermOfTheRequestedType() {
    final Predicate<TermStruct> lists = checker(false, 1, "+list");
    assertTrue(lists.test(goal(newList())));
    assertTrue(lists.test(goal(newList(newAtom("a"), newVar("Tail")))));
    assertFalse(lists.test(goal(newVar("X"))));
    assertFalse(lists.test(goal(newAtom("a"))));

    assertThrows(
        ProlInstantiationErrorException.class,
        () -> checker(true, 1, "+integer").test(goal(newVar("X"))));
    assertFalse(checker(false, 1, "++list").test(goal(newList(newAtom("a"), newVar("Tail")))));
  }

  @Test
  void unboundModeIgnoresTheDeclaredType() {
    final Predicate<TermStruct> unbound = checker(false, 1, "--integer");
    assertTrue(unbound.test(goal(newVar("X"))));
    assertTrue(unbound.test(goal(newList(newAtom("a"), newVar("Tail")))));
    assertFalse(unbound.test(goal(newLong(1L))));
    assertFalse(unbound.test(goal(newList())));

    final ProlTypeErrorException error = assertThrows(
        ProlTypeErrorException.class,
        () -> checker(true, 1, "--integer").test(goal(newLong(1L))));
    assertEquals("var", error.getValidType());
  }

  @Test
  void quietModeReturnsFalseAndAssertModeReportsTheFirstBadArgument() {
    final Predicate<TermStruct> quiet = checker(false, 2, "+atom,+integer");
    assertTrue(quiet.test(goal(newAtom("a"), newLong(1L))));
    assertFalse(quiet.test(goal(newLong(1L), newAtom("b"))));
    assertFalse(quiet.test(goal(newAtom("a"), newAtom("b"))));

    final Predicate<TermStruct> asserting = checker(true, 2, "+atom,+integer");
    final ProlTypeErrorException first = assertThrows(
        ProlTypeErrorException.class,
        () -> asserting.test(goal(newLong(1L), newAtom("b"))));
    assertEquals("atom", first.getValidType());

    final ProlTypeErrorException second = assertThrows(
        ProlTypeErrorException.class,
        () -> asserting.test(goal(newAtom("a"), newAtom("b"))));
    assertEquals("integer", second.getValidType());
  }

  @Test
  void alternativePatternsAcceptAGoalThatMatchesAnyRow() {
    final Predicate<TermStruct> either = checker(true, 1, "++atom", "++list");
    assertTrue(either.test(goal(newAtom("a"))));
    assertTrue(either.test(goal(newList(newAtom("a")))));
    assertThrows(ProlArgumentValidationException.class, () -> either.test(goal(newLong(1L))));
    assertFalse(checker(false, 1, "++atom", "++list").test(goal(newLong(1L))));
  }

  @Test
  void onlyPatternsOfThePredicateArityAreApplied() {
    final List<List<ModificatorArgument>> patterns = AutoArgumentValidator.parse(new String[] {
        "++atom",
        "++integer,+list"
    });

    final Predicate<TermStruct> unary = AutoArgumentValidator.makeFor(1, patterns, true);
    assertTrue(unary.test(goal(newAtom("a"))));
    assertThrows(ProlTypeErrorException.class, () -> unary.test(goal(newLong(1L))));

    final Predicate<TermStruct> binary = AutoArgumentValidator.makeFor(2, patterns, false);
    assertTrue(binary.test(goal(newLong(1L), newList(newAtom("a")))));
    assertFalse(binary.test(goal(newAtom("a"), newList())));
  }

  @Test
  void validationFollowsAVariableToItsValue() {
    final TermVar atom = newVar("A");
    atom.setValue(newAtom("foo"));
    assertTrue(checker(true, 1, "++atom").test(goal(atom)));

    final TermVar indicator = newVar("PI");
    indicator.setValue(newStruct(newAtom("/"), newAtom("foo"), newLong(2L)));
    assertTrue(checker(true, 1, "+predicate_indicator").test(goal(indicator)));

    final TermVar name = newVar("Name");
    assertTrue(checker(false, 1, "?predicate_indicator").test(
        goal(newStruct(newAtom("/"), newAtom("foo"), name))));
    assertTrue(checker(false, 1, "?predicate_indicator").test(goal(newVar("PI"))));
    assertFalse(checker(false, 1, "?predicate_indicator").test(goal(newAtom("foo"))));
    assertFalse(checker(false, 1, "+predicate_indicator").test(goal(newVar("PI"))));
  }
}
