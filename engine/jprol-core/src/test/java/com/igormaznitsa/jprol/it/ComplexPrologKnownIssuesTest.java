package com.igormaznitsa.jprol.it;

import static java.time.Duration.ofSeconds;
import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;

import com.igormaznitsa.jprol.exceptions.ProlCustomErrorException;
import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import org.junit.jupiter.api.Test;

class ComplexPrologKnownIssuesTest extends AbstractJProlTest {

  private static final String COUNTER =
      "count(N, N) :- !. count(I, N) :- I1 is I + 1, count(I1, N).";

  @Test
  void testNegationNeverBindsVariables() {
    this.checkOnce("\\+ (X = a, fail), var(X).", true);
    this.checkOnce("\\+ \\+ X = a, var(X).", true);
    this.checkOnce("not(not(X = a)), var(X).", true);
    assertEquals(singletonList("'b'"), this.collectSolutions("", "\\+ \\+ X = a, X = b.", "X"));
  }

  @Test
  void testIdentityOfAliasedVariables() {
    this.checkOnce("A = X, B = X, A == B.", true);
    this.checkOnce("copy_term(f(X, X), f(A, B)), A == B.", true);
  }

  @Test
  void testCutInIfThenElseConditionIsLocal() {
    assertEquals(asList("1", "2"),
        this.collectSolutions("p(1). p(2).", "member(X, [1,2]), ( p(X), ! -> R = X ; R = none ).",
            "R"));
    assertEquals(asList("1", "9"),
        this.collectSolutions("", "( member(X, [1,2,3]), ! -> true ; true ) ; X = 9.", "X"));
    assertEquals(asList("1", "2"),
        this.collectSolutions(
            "p(1). p(2). t(R) :- member(X, [1,2]), ( p(X), ! -> R = X ; R = none ).",
            "t(R).", "R"));
  }

  @Test
  void testCatchRethrowsUnmatchedBall() {
    final ProlCustomErrorException error = assertThrows(ProlCustomErrorException.class,
        () -> this.prepareGoal("catch(throw(a), b, true).").prove());
    assertEquals("a", error.getErrorTerm().getText());
    assertThrows(ProlEvaluationErrorException.class,
        () -> this.prepareGoal("catch(X is 1/0, error(type_error(_, _), _), true).").prove());
    assertEquals("'inner'",
        this.proveSingle("", "catch(catch(throw(inner), outer, R = wrong), Ball, R = Ball).", "R"));
  }

  @Test
  void testCatchUnifiesBallAsThrown() {
    this.checkOnce("catch(throw(a), a, true).", true);
    assertEquals("42",
        this.proveSingle("", "catch(throw(error(my, 42)), error(my, V), true).", "V"));
    assertEquals("'second'",
        this.proveSingle("", "catch(catch(throw(first), first, throw(second)), X, R = X).", "R"));
    assertEquals("caught(2)",
        this.proveSingle("p(1). p(2). p(3).",
            "catch((p(X), X > 1, throw(found(X))), found(Y), R = caught(Y)).", "R"));
  }

  @Test
  void testCatchUndoesGoalBindings() {
    this.checkOnce("catch((X = bound, throw(oops)), _, true), var(X).", true);
  }

  @Test
  void testIsoShapedBuiltinErrorTerms() {
    this.checkOnce("catch(X is 1/0, error(evaluation_error(zero_divisor), _), true).", true);
    this.checkOnce("catch(arg(x, f(a), _), error(type_error(integer, x), _), true).", true);
    this.checkOnce(
        "catch(undefined_xyz(1), error(existence_error(procedure, undefined_xyz/1), _), true).",
        true);
  }

  @Test
  void testUnivDereferencesBoundFunctor() {
    assertEquals("foo(1)", this.proveSingle("", "P = foo, G =.. [P, 1].", "G"));
    assertEquals("[2,4,6]",
        this.proveSingle(
            "map(_, [], []). map(P, [X|Xs], [Y|Ys]) :- G =.. [P, X, Y], call(G), map(P, Xs, Ys)."
                + "double(X, Y) :- Y is X * 2.", "map(double, [1,2,3], L).", "L"));
  }

  @Test
  void testModTakesSignOfDivisor() {
    assertEquals("2", this.proveSingle("", "X is -7 mod 3.", "X"));
    assertEquals("-2", this.proveSingle("", "X is 7 mod -3.", "X"));
  }

  @Test
  void testIntegerOverflowIsReported() {
    assertThrows(ProlEvaluationErrorException.class,
        () -> this.prepareGoal("X is 9223372036854775807 + 1.").prove());
  }

  @Test
  void testLengthEnumeratesListSizes() {
    assertEquals(asList("0", "1", "2", "3"),
        this.collectSolutions("", "length(L, N), ( N >= 3, ! ; true ).", "N"));
  }

  @Test
  void testRetractMatchesClauseBody() {
    assertEquals("'b'",
        this.proveSingle(":- dynamic(r/1). r(a) :- true. r(b) :- write(x).",
            "retract((r(X) :- write(_))).", "X"));
  }

  @Test
  void testLogicalUpdateViewOfClauseIteration() {
    assertEquals(asList("1", "2", "3"),
        this.collectSolutions(":- dynamic(c/1). c(1). c(2). c(3).",
            "c(X), once(retract(c(_))).", "X"));
    assertEquals(asList("1", "2"),
        this.collectSolutions(":- dynamic(d/1). d(1). d(2).", "d(X), Y is X + 10, assertz(d(Y)).",
            "X"));
  }

  @Test
  void testLogicalUpdateViewOfRetract() {
    assertEquals("[1]", assertTimeoutPreemptively(ofSeconds(5),
        () -> this.proveSingle(":- dynamic(k/1). k(0).",
            "( retract(k(_)), assertz(k(1)), fail ; findall(X, k(X), L) ).", "L")));
  }

  @Test
  void testDeepRecursion() {
    this.checkOnce(COUNTER, "count(0, 100000).", true);
    assertEquals("50005000",
        this.proveSingle("sum([], A, A). sum([H|T], A, S) :- A1 is A + H, sum(T, A1, S)."
                + "range(H, H, [H]) :- !. range(L, H, [L|T]) :- L1 is L + 1, range(L1, H, T).",
            "range(1, 10000, L), sum(L, 0, S).", "S"));
  }

  @Test
  void testBetweenScalesLinearly() {
    assertTimeoutPreemptively(ofSeconds(5),
        () -> this.checkOnce("findall(X, between(1, 20000, X), L), length(L, 20000).", true));
  }
}
