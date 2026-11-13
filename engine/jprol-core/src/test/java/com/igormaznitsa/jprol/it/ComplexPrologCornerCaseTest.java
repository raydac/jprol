package com.igormaznitsa.jprol.it;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.utils.DefaultKeyValueTermStore;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

@Timeout(30)
class ComplexPrologCornerCaseTest extends AbstractJProlTest {

  private static final String PERMUTATION =
      "sel(X, [X|T], T). sel(X, [H|T], [H|R]) :- sel(X, T, R)."
          + "perm([], []). perm(L, [H|T]) :- sel(H, L, R), perm(R, T).";

  private static final String ANONYMOUS_VARIABLE_NAMES = "_[A-Za-z0-9]*";

  @Test
  void testCutInsideIfThenElseConditionKeepsClauseAlternatives() {
    assertEquals(asList("1", "9"),
        this.collectSolutions("t(X) :- ( member(X, [1,2,3]), ! -> true ; true ). t(9).", "t(X).",
            "X"));
  }

  @Test
  void testIfThenElseInsideClauseKeepsOuterAlternatives() {
    assertEquals(asList("1", "3"),
        this.collectSolutions("p(1). p(2). q(X) :- ( p(X) -> true ; fail ). q(3).", "q(X).", "X"));
  }

  @Test
  void testCutInsideThenBranchCutsClause() {
    assertEquals(singletonList("1"),
        this.collectSolutions("p(1). p(2). r(X) :- ( true -> p(X), ! ; true ). r(9).", "r(X).",
            "X"));
  }

  @Test
  void testCutInsideDisjunctionCutsClause() {
    assertEquals(singletonList("'a'"),
        this.collectSolutions("t(X) :- ( X = a, ! ; X = b ). t(c).", "t(X).", "X"));
  }

  @Test
  void testCutInsideCallIsOpaque() {
    assertEquals(asList("1", "2", "3"),
        this.collectSolutions("p(1). p(2). s(X) :- call((p(X), !)). s(X) :- X = 2. s(3).",
            "s(X).", "X"));
  }

  @Test
  void testCutInsideNegationIsLocal() {
    assertEquals(asList("1", "2"),
        this.collectSolutions("p(1). p(2). n(X) :- p(X), \\+ (X = 3, !). n(4) :- fail.", "n(X).",
            "X"));
  }

  @Test
  void testCutInsideFindallIsLocal() {
    assertEquals("[1]", this.proveSingle("p(1). p(2).", "findall(X, (p(X), !), L).", "L"));
    assertEquals("[1,2]", this.proveSingle("p(1). p(2).", "findall(X, p(X), L), !.", "L"));
  }

  @Test
  void testIfThenWithoutElseFailsWhenConditionFails() {
    this.checkOnce("( fail -> true ).", false);
    this.checkOnce("( true -> fail ).", false);
    this.checkOnce("( true -> true ).", true);
  }

  @Test
  void testIfThenElseTakesOnlyFirstConditionSolution() {
    assertEquals(singletonList("1"),
        this.collectSolutions("p(1). p(2). p(3).", "( p(X) -> true ; X = none ).", "X"));
    assertEquals(asList("1", "2", "3"),
        this.collectSolutions("p(1). p(2). p(3).", "( p(X) ; X = none ), X \\== none.", "X"));
  }

  @Test
  void testNestedIfThenElseChain() {
    final String classify = "classify(N, C) :- ( N < 0 -> C = negative ; N =:= 0 -> C = zero"
        + " ; N < 10 -> C = small ; C = big ).";
    assertEquals("'negative'", this.proveSingle(classify, "classify(-5, C).", "C"));
    assertEquals("'zero'", this.proveSingle(classify, "classify(0, C).", "C"));
    assertEquals("'small'", this.proveSingle(classify, "classify(7, C).", "C"));
    assertEquals("'big'", this.proveSingle(classify, "classify(100, C).", "C"));
  }

  @Test
  void testFailedDisjunctionBranchUndoesBindings() {
    this.checkOnce("( X = a, fail ; true ), var(X).", true);
    this.checkOnce("( X = a, Y = b, fail ; X = c ), var(Y), X == c.", true);
  }

  @Test
  void testCatchIsTransparentToBacktrackingOfGoal() {
    assertEquals(asList("1", "2", "3"),
        this.collectSolutions("p(1). p(2). p(3).", "catch(p(X), _, true).", "X"));
    assertEquals(singletonList("2"),
        this.collectSolutions("r(1). r(2).", "catch(r(X), _, true), X > 1.", "X"));
  }

  @Test
  void testCatchRecognizesBuiltinErrorKinds() {
    final String errorKind = "catch(Goal, error(E, _), true), functor(E, Kind, _).";
    assertEquals("'evaluation_error'",
        this.proveSingle("", errorKind.replace("Goal", "X is 1/0"), "Kind"));
    assertEquals("'instantiation_error'",
        this.proveSingle("", errorKind.replace("Goal", "atom_length(_, _)"), "Kind"));
    assertEquals("'type_error'",
        this.proveSingle("", errorKind.replace("Goal", "arg(x, f(a), _)"), "Kind"));
    assertEquals("'existence_error'",
        this.proveSingle("", errorKind.replace("Goal", "undefined_predicate_xyz(1)"), "Kind"));
  }

  @Test
  void testUncaughtBuiltinErrorsReachJava() {
    assertThrows(ProlEvaluationErrorException.class,
        () -> this.prepareGoal("X is 10 // 0.").prove());
    assertThrows(ProlExistenceErrorException.class,
        () -> this.prepareGoal("undefined_predicate_xyz.").prove());
  }

  @Test
  void testFindallWithNoSolutionsGivesEmptyList() {
    assertEquals("[]", this.proveSingle("", "findall(X, fail, L).", "L"));
  }

  @Test
  void testBagofAndSetofFailWithoutSolutions() {
    this.checkOnce("bagof(X, fail, L).", false);
    this.checkOnce("setof(X, fail, L).", false);
  }

  @Test
  void testNestedFindall() {
    assertEquals("[[1,2],[2,4],[3,6]]",
        this.proveSingle("n(1). n(2). n(3).",
            "findall(P, (n(X), findall(Y, (member(K,[1,2]), Y is X*K), P)), L).", "L"));
  }

  @Test
  void testFindallCopiesFreeVariables() {
    this.checkOnce("findall(f(X), member(_, [a,b]), [f(A), f(B)]), A = 1, var(B).", true);
  }

  @Test
  void testBagofGroupsByFreeVariable() {
    assertEquals(asList("[1,3]", "[2]"),
        this.collectSolutions(
            "age(a, 1). age(b, 2). age(c, 3). kind(a, x). kind(b, y). kind(c, x).",
            "bagof(N, P^(age(P, N), kind(P, K)), L).", "L"));
  }

  @Test
  void testSetofSortsAndRemovesDuplicates() {
    assertEquals("['a','b','c']",
        this.proveSingle("v(c). v(a). v(b). v(a). v(c).", "setof(X, v(X), L).", "L"));
  }

  @Test
  void testSetofStandardOrderOfMixedTerms() {
    assertEquals("[_,1,2.0,'a',f(1)]",
        this.proveSingle("", "setof(X, member(X, [f(1), a, 2.0, 1, Y, a]), L).", "L")
            .replaceFirst("^\\[[A-Z_][A-Za-z0-9_]*,", "[_,"));
  }

  @Test
  void testSortOrdersPairsByWholeTerm() {
    assertEquals("['a' - 1,'a' - 2,'b' - 3,'c' - 1]",
        this.proveSingle("", "sort([c-1, a-2, b-3, a-1], L).", "L"));
  }

  @Test
  void testMsortKeepsDuplicates() {
    assertEquals("[1,1,2,3,3]", this.proveSingle("", "msort([3,1,2,3,1], L).", "L"));
  }

  @Test
  void testCompareStandardOrder() {
    this.checkOnce("compare(<, 1, a).", true);
    this.checkOnce("compare(<, a, f(a)).", true);
    this.checkOnce("compare(<, f(b), g(a)).", true);
    this.checkOnce("compare(<, f(a, b), g(a)).", false);
    this.checkOnce("compare(=, f(X), f(X)).", true);
    this.checkOnce("compare(<, X, 1).", true);
  }

  @Test
  void testRetractOfCurrentSolutionDuringIteration() {
    assertEquals(asList("1", "2", "3"),
        this.collectSolutions(":- dynamic(c/1). c(1). c(2). c(3).",
            "c(X), retract(c(X)).", "X"));
  }

  @Test
  void testRetractBindsRemovedClause() {
    assertEquals("0", this.proveSingle(":- dynamic(cnt/1). cnt(0).", "retract(cnt(C)).", "C"));
    assertEquals(asList("0", "1"),
        this.collectSolutions(":- dynamic(cnt/1). cnt(0). cnt(1).", "retract(cnt(C)).", "C"));
  }

  @Test
  void testForallRetractRemovesSelectedClauses() {
    assertEquals("[2]",
        this.proveSingle(":- dynamic(e/1). e(1). e(2). e(3).",
            "forall(member(X, [1,3]), retract(e(X))), findall(Y, e(Y), L).", "L"));
  }

  @Test
  void testAssertedRuleKeepsIndependentVariables() {
    assertEquals("[f(1,2),f(3,4)]",
        this.proveSingle(":- dynamic(pair/2).",
            "assertz((pair(A, B) :- B is A + 1)),"
                + " findall(f(X,Y), (member(X,[1,3]), pair(X,Y)), L).",
            "L"));
  }

  @Test
  void testVanillaMetaInterpreter() {
    final String interpreter = ":- dynamic(app/3). :- dynamic(nrev/2)."
        + "app([], L, L). app([H|T], L, [H|R]) :- app(T, L, R)."
        + "nrev([], []). nrev([H|T], R) :- nrev(T, RT), app(RT, [H], R)."
        + "solve(true) :- !. solve((A, B)) :- !, solve(A), solve(B)."
        + "solve(G) :- is_builtin(G), !, call(G)."
        + "solve(G) :- clause(G, Body), solve(Body)."
        + "is_builtin(G) :- functor(G, N, A), \\+ member(N/A, [app/3, nrev/2]).";

    assertEquals("[5,4,3,2,1]",
        this.proveSingle(interpreter, "solve(nrev([1,2,3,4,5], R)).", "R"));
    assertEquals(asList("[]", "[1]", "[1,2]"),
        this.collectSolutions(interpreter, "solve(app(X, _, [1,2])).", "X"));
  }

  @Test
  void testAppendGeneratesAllSplits() {
    assertEquals(asList("[] + [1,2,3]", "[1] + [2,3]", "[1,2] + [3]", "[1,2,3] + []"),
        this.collectSolutions("split(L, A+B) :- append(A, B, L).", "split([1,2,3], S).", "S"));
  }

  @Test
  void testLengthBuildsListOfFreshVariables() {
    assertEquals("[_,_,_]",
        this.proveSingle("", "length(L, 3).", "L").replaceAll(ANONYMOUS_VARIABLE_NAMES, "_"));
    this.checkOnce("length(L, 2), L = [A, B], A \\== B.", true);
  }

  @Test
  void testPermutationsCount() {
    assertEquals("120",
        this.proveSingle(PERMUTATION, "findall(P, perm([a,b,c,d,e], P), Ps), length(Ps, N).",
            "N"));
  }

  @Test
  void testSixQueensHasFourSolutions() {
    assertEquals("4",
        this.proveSingle(PERMUTATION
                + "queens(N, Qs) :- numlist(1, N, Ns), perm(Ns, Qs), safe(Qs)."
                + "numlist(L, H, []) :- L > H, !."
                + "numlist(L, H, [L|T]) :- L1 is L + 1, numlist(L1, H, T)."
                + "safe([]). safe([Q|Qs]) :- noattack(Q, Qs, 1), safe(Qs)."
                + "noattack(_, [], _)."
                + "noattack(Q, [Q1|Qs], D) :- Q =\\= Q1 + D, Q =\\= Q1 - D, D1 is D + 1,"
                + " noattack(Q, Qs, D1).",
            "findall(Q, queens(6, Q), All), length(All, N).", "N"));
  }

  @Test
  void testQuicksortWithDifferenceLists() {
    assertEquals("[0,1,1,2,3,4,5,5,6,9]",
        this.proveSingle("qs(L, S) :- qs(L, S, [])."
                + "qs([], T, T)."
                + "qs([H|T], S, Tail) :- part(H, T, Lo, Hi), qs(Lo, S, [H|M]), qs(Hi, M, Tail)."
                + "part(_, [], [], [])."
                + "part(P, [X|Xs], [X|Lo], Hi) :- X =< P, !, part(P, Xs, Lo, Hi)."
                + "part(P, [X|Xs], Lo, [X|Hi]) :- part(P, Xs, Lo, Hi).",
            "qs([3,1,4,1,5,9,2,6,5,0], S).", "S"));
  }

  @Test
  void testGraphPathWithCycleAvoidance() {
    final String graph = "edge(a,b). edge(b,c). edge(c,a). edge(c,d). edge(b,d)."
        + "path(X, Y, P) :- path(X, Y, [X], R), reverse(R, P)."
        + "path(X, X, V, V)."
        + "path(X, Y, V, P) :- edge(X, Z), \\+ member(Z, V), path(Z, Y, [Z|V], P).";
    assertEquals(asList("['a','b','c','d']", "['a','b','d']"),
        this.collectSolutions(graph, "path(a, d, P).", "P"));
    assertEquals(singletonList("['a']"), this.collectSolutions(graph, "path(a, a, P).", "P"));
  }

  @Test
  void testPeanoArithmeticIsReversible() {
    final String peano = "plus(0, Y, Y). plus(s(X), Y, s(Z)) :- plus(X, Y, Z).";
    assertEquals("s(s(s(0)))", this.proveSingle(peano, "plus(s(0), s(s(0)), R).", "R"));
    assertEquals(asList("0", "s(0)", "s(s(0))"),
        this.collectSolutions(peano, "plus(X, _, s(s(0))).", "X"));
  }

  @Test
  void testMutualRecursion() {
    final String parity =
        "ev(0). ev(N) :- N > 0, M is N - 1, od(M). od(N) :- N > 0, M is N - 1, ev(M).";
    this.checkOnce(parity, "ev(1000).", true);
    this.checkOnce(parity, "od(1000).", false);
  }

  @Test
  void testRecursionOfThousandsOfLevels() {
    this.checkOnce("count(N, N) :- !. count(I, N) :- I1 is I + 1, count(I1, N).",
        "count(0, 2000).", true);
  }

  @Test
  void testSumOfLongList() {
    assertEquals("500500",
        this.proveSingle("sum([], A, A). sum([H|T], A, S) :- A1 is A + H, sum(T, A1, S)."
                + "range(H, H, [H]) :- !. range(L, H, [L|T]) :- L1 is L + 1, range(L1, H, T).",
            "range(1, 1000, L), sum(L, 0, S).", "S"));
  }

  @Test
  void testRepeatWithCutTerminates() {
    assertEquals("5",
        this.proveSingle(":- dynamic(cnt/1). cnt(0)."
                + "tick(N) :- once(retract(cnt(C))), N is C + 1, assertz(cnt(N)).",
            "repeat, tick(N), N >= 5, !, R = N.", "R"));
  }

  @Test
  void testBetweenBacktrackingAndBounds() {
    assertEquals(asList("1", "2", "3"), this.collectSolutions("", "between(1, 3, X).", "X"));
    this.checkOnce("between(3, 1, X).", false);
    this.checkOnce("between(1, 3, 3).", true);
    this.checkOnce("between(1, 3, 4).", false);
  }

  @Test
  void testUnificationSharesVariablesAcrossArguments() {
    assertEquals("f('a','a','a')",
        this.proveSingle("", "T = f(X, Y, Z), X = Y, Y = Z, Z = a.", "T"));
    this.checkOnce("f(X, X) = f(a, b).", false);
    this.checkOnce("f(X, b) = f(a, X).", false);
    this.checkOnce("[H|T] = [1,2,3], H == 1, T == [2,3].", true);
    this.checkOnce("[_, _ | T] = [1].", false);
    this.checkOnce("X = [1,2|T], T = [3], X == [1,2,3].", true);
  }

  @Test
  void testDeepStructureUnification() {
    this.checkOnce("mk(0, leaf) :- !. mk(N, node(L, R)) :- M is N - 1, mk(M, L), mk(M, R).",
        "mk(12, A), mk(12, B), A = B.", true);
  }

  @Test
  void testCopyTermPreservesSharingAndFreshness() {
    this.checkOnce("copy_term(f(X, X, Y), f(A, B, C)), A = 1, B == 1, var(C).", true);
    this.checkOnce("copy_term(f(X), f(Y)), X \\== Y.", true);
    this.checkOnce("copy_term(f(X), f(Y)), Y = 1, var(X).", true);
    this.checkOnce("X = a, copy_term(f(X), f(Y)), Y == a.", true);
  }

  @Test
  void testUnivRoundTrip() {
    assertEquals("foo('a','b')", this.proveSingle("", "T =.. [foo, a, b].", "T"));
    assertEquals("['foo','a','b']", this.proveSingle("", "foo(a, b) =.. L.", "L"));
    assertEquals("['abc']", this.proveSingle("", "abc =.. L.", "L"));
    assertEquals("[1]", this.proveSingle("", "1 =.. L.", "L"));
    this.checkOnce("T =.. [foo, X], T = foo(1), X == 1.", true);
  }

  @Test
  void testFunctorAndArgOnCompoundTerms() {
    assertEquals("f(_,_,_)",
        this.proveSingle("", "functor(T, f, 3).", "T").replaceAll(ANONYMOUS_VARIABLE_NAMES, "_"));
    assertEquals(asList("1", "2", "3"),
        this.collectSolutions("",
            "functor(f(a,b,c), _, N), between(1, N, I), arg(I, f(a,b,c), _).", "I"));
    this.checkOnce("arg(0, f(a), _).", false);
    this.checkOnce("arg(2, f(a), _).", false);
  }

  @Test
  void testArithmeticCornerCases() {
    assertEquals("-1", this.proveSingle("", "X is -7 // 7.", "X"));
    assertEquals("-3", this.proveSingle("", "X is -7 // 2.", "X"));
    assertEquals("-1", this.proveSingle("", "X is -7 rem 3.", "X"));
    assertEquals("2.5", this.proveSingle("", "X is 5 / 2.0.", "X"));
    assertEquals("0.5", this.proveSingle("", "X is 2 ** -1.", "X"));
    this.checkOnce("1 =:= 1.0.", true);
    this.checkOnce("1 == 1.0.", false);
    this.checkOnce("1 = 1.0.", false);
  }

  @Test
  void testSubAtomEnumeratesAllSubstrings() {
    assertEquals("10",
        this.proveSingle("", "findall(S, sub_atom(abc, _, _, _, S), L), length(L, N).", "N"));
  }

  @Test
  void testAtomConcatEnumeratesSplits() {
    assertEquals(asList("'' + 'ab'", "'a' + 'b'", "'ab' + ''"),
        this.collectSolutions("", "atom_concat(X, Y, ab), R = X+Y.", "R"));
  }

  @Test
  void testDoubleQuotedTextIsCodeList() {
    assertEquals("[97,98,99]", this.proveSingle("", "X = \"abc\".", "X"));
  }

  @Test
  void testForallChecksAllSolutions() {
    this.checkOnce("forall(member(X, [1,2,3]), X > 0).", true);
    this.checkOnce("forall(member(X, [1,-2,3]), X > 0).", false);
    this.checkOnce("forall(fail, fail).", true);
  }

  @Test
  void testOnceDiscardsAlternatives() {
    assertEquals(singletonList("1"), this.collectSolutions("p(1). p(2).", "once(p(X)).", "X"));
  }

  @Test
  void testUserDefinedOperatorsInClauses() {
    assertEquals("'yes'",
        this.proveSingle(":- op(700, xfx, likes). :- op(200, xfy, and). alice likes bob and carol.",
            "alice likes X and Y, X == bob, Y == carol, R = yes.", "R"));
  }

  @Test
  void testGoalWithoutSolutions() {
    assertEquals(emptyList(), this.collectSolutions("p(1).", "p(X), X > 1.", "X"));
  }

  @Test
  void testVariableBindingsResetBetweenSolutions() {
    try (final JProlContext context = this.makeContextAndConsult("p(1, a). p(2, b).")) {
      final JProlChoicePoint choicePoint = context.makeChoicePoint("p(X, Y).");

      assertNotNull(choicePoint.prove());
      assertEquals("1", getVarAsText(choicePoint, "X"));
      assertEquals("'a'", getVarAsText(choicePoint, "Y"));

      assertNotNull(choicePoint.prove());
      assertEquals("2", getVarAsText(choicePoint, "X"));
      assertEquals("'b'", getVarAsText(choicePoint, "Y"));

      assertNull(choicePoint.prove());
    }
  }

  @Test
  void testCallBacktrackingPreservesInnerBoundVariablesAndIsolatesFreeVariables() {
    assertEquals(
        asList("11", "21", "31"),
        this.collectSolutions(
            "",
            "call((X = 1, member(Y, [10, 20, 30]), Z is X + Y)).",
            "Z"
        )
    );
    assertEquals(
        "[10,20]",
        this.proveSingle(
            "",
            "G = (X = 5, member(Y, [2, 4]), Z is X * Y), findall(Z, call(G), L).",
            "L"
        )
    );
    assertEquals(
        "f('a') - 2",
        this.proveSingle(
            "",
            "call((X = f(Free), (Y = 1 ; Y = 2))), Free = a, Y == 2, R = X - Y.",
            "R"
        )
    );
    assertEquals(
        asList("1", "9"),
        this.collectSolutions(
            "p(1). p(2). q(X) :- call(call((p(X), !))). q(9).",
            "q(X).",
            "X"
        )
    );
  }

  @Test
  void testCatchMultiSolutionGoalAndMultiSolutionHandlerSemantics() {
    assertEquals(
        asList("6", "10", "14"),
        this.collectSolutions(
            "",
            "catch((X = 2, member(Y, [3, 5, 7]), Z is X * Y), _, Z = -1).",
            "Z"
        )
    );
    assertEquals(
        asList("a(7)", "b(7)", "c(7)"),
        this.collectSolutions(
            "",
            "catch(throw(found(7)), found(V), (R = a(V) ; R = b(V) ; R = c(V))).",
            "R"
        )
    );
    assertEquals(
        "second('bound')",
        this.proveSingle(
            "",
            "catch((Y = ok ; throw(box(Free))), box(H), (R = first(H) ; R = second(H))),"
                + " H = bound, R == second(bound).",
            "R"
        )
    );
    this.checkOnce(
        "catch(catch(throw(err(1, 2)), err(X, X), R = inner), err(A, B), R = outer(A, B)),"
            + " var(X), R == outer(1, 2).",
        true
    );
    assertEquals(
        asList("'boom'", "'boom'"),
        this.collectSolutions(
            "recover :- true. recover :- true.",
            "catch(throw(boom), E, recover).",
            "E"
        )
    );
    assertEquals(
        "'escalated'",
        this.proveSingle(
            "rethrow_atom :- throw(escalated).",
            "catch(catch(throw(escalated), escalated, rethrow_atom), Out, true).",
            "Out"
        )
    );
  }

  @Test
  void testSubAtomSearchWithInstantiatedSubSharedVarsAndBounds() {
    assertEquals(
        asList("2 - 2 - 7", "9 - 2 - 0"),
        this.collectSolutions(
            "",
            "sub_atom(abracadabra, B, L, A, ra), R = B - L - A.",
            "R"
        )
    );
    assertEquals(
        asList("2 - 2 - 2", "4 - 2 - 0"),
        this.collectSolutions(
            "",
            "sub_atom(banana, B, L, A, na), R = B - L - A.",
            "R"
        )
    );
    assertEquals(
        asList("0 - 2", "1 - 2", "2 - 2"),
        this.collectSolutions(
            "",
            "sub_atom(aaaa, B, L, _, aa), R = B - L.",
            "R"
        )
    );
    assertEquals(
        asList("0 - 0 - 2", "1 - 0 - 1", "2 - 0 - 0"),
        this.collectSolutions(
            "",
            "sub_atom(ab, B, L, A, ''), R = B - L - A.",
            "R"
        )
    );
    assertEquals(
        asList("0 - 4 - ''", "1 - 2 - 'b'", "2 - 0 - 'cd'"),
        this.collectSolutions(
            "",
            "sub_atom(abcd, X, X, A, S), R = X - A - S.",
            "R"
        )
    );
    this.checkOnce("sub_atom(abc, 2, 2, 0, _).", false);
    this.checkOnce("sub_atom(abc, -1, 1, _, _).", false);
    this.checkOnce("sub_atom(abc, 0, 5, _, _).", false);
    this.checkOnce("sub_atom(abc, _, _, _, xyz).", false);
  }

  @Test
  void testFullMetaCircularInterpreterWithCutIfThenElseNegationAndCatch() {
    final String metaInterpreter =
        "q(1). q(2). r(10). r(20)."
            + "cut_demo(X - Y) :- q(X), !, r(Y)."
            + "cut_demo(999)."
            + "cond_cut(X) :- ( q(X), ! -> X > 0 ; X = -1 )."
            + "cond_cut(77)."
            + "risky(1, ok)."
            + "risky(2, _) :- throw(bad_input(2))."
            + "safe_run(N, Out) :- catch(risky(N, Out), bad_input(K), Out = recovered(K))."
            + "user_pred(cut_demo/1). user_pred(cond_cut/1). user_pred(q/1). user_pred(r/1)."
            + "user_pred(risky/2). user_pred(safe_run/2)."
            + "mi_goal(true) :- !."
            + "mi_goal((A, B)) :- !, mi_goal(A), mi_goal(B)."
            + "mi_goal((C -> T ; E)) :- !, ( mi_goal(C) -> mi_goal(T) ; mi_goal(E) )."
            + "mi_goal((C -> T)) :- !, ( mi_goal(C) -> mi_goal(T) )."
            + "mi_goal((A ; B)) :- !, ( mi_goal(A) ; mi_goal(B) )."
            + "mi_goal(\\+ A) :- !, \\+ mi_goal(A)."
            + "mi_goal(catch(G, C, H)) :- !, catch(mi_goal(G), C, mi_goal(H))."
            + "mi_goal(throw(B)) :- !, throw(B)."
            + "mi_goal(G) :- functor(G, F, A), \\+ user_pred(F/A), !, call(G)."
            + "mi_goal(G) :-"
            + "  clause(G, Body),"
            + "  mi_clause_body(Body, HadCut, Remainder),"
            + "  ( HadCut == yes -> !, mi_goal(Remainder) ; true )."
            + "mi_clause_body(!, yes, true) :- !."
            + "mi_clause_body((A, B), HadCut, Remainder) :- !,"
            + "  mi_clause_body(A, CutA, RemA),"
            + "  ( CutA == yes -> HadCut = yes, Remainder = (RemA, B)"
            + "  ; mi_clause_body(B, HadCut, Remainder)"
            + "  )."
            + "mi_clause_body((C -> T ; E), HadCut, Remainder) :- !,"
            + "  ( mi_goal(C) -> mi_clause_body(T, HadCut, Remainder)"
            + "  ; mi_clause_body(E, HadCut, Remainder)"
            + "  )."
            + "mi_clause_body((A ; B), HadCut, Remainder) :- !,"
            + "  ( mi_clause_body(A, HadCut, Remainder)"
            + "  ; mi_clause_body(B, HadCut, Remainder)"
            + "  )."
            + "mi_clause_body(Goal, no, true) :- mi_goal(Goal).";

    assertEquals(
        asList("1 - 10", "1 - 20"),
        this.collectSolutions(metaInterpreter, "mi_goal(cut_demo(Z)).", "Z")
    );
    assertEquals(
        asList("1", "77"),
        this.collectSolutions(metaInterpreter, "mi_goal(cond_cut(Z)).", "Z")
    );
    assertEquals(
        asList("'ok'", "recovered(2)"),
        this.collectSolutions(
            metaInterpreter,
            "member(N, [1, 2]), mi_goal((safe_run(N, Z), \\+ Z == forbidden)).",
            "Z"
        )
    );
  }

  @Test
  void testFirstArgumentIndexingWithMixedTypesWildcardsAndDynamicMutations() {
    final String indexedProgram =
        ":- dynamic(idx/2)."
            + "idx(X, w_head) :- var(X), !, fail."
            + "idx(_, any_top)."
            + "idx(alpha, atom_alpha)."
            + "idx(alpha(), zero_arity_alpha)."
            + "idx(42, int_pos)."
            + "idx(-42, int_neg)."
            + "idx(42.0, float_pos)."
            + "idx([], nil_list)."
            + "idx([1, 2], list_12)."
            + "idx([1, 3], list_13)."
            + "idx(f(a), struct_f1)."
            + "idx(f(a, b), struct_f2)."
            + "idx(_, any_bottom).";

    assertEquals(
        asList("'any_top'", "'atom_alpha'", "'zero_arity_alpha'", "'any_bottom'"),
        this.collectSolutions(indexedProgram, "K = B, B = alpha, idx(K, V).", "V")
    );
    assertEquals(
        asList("'any_top'", "'int_pos'", "'any_bottom'"),
        this.collectSolutions(indexedProgram, "idx(42, V).", "V")
    );
    assertEquals(
        asList("'any_top'", "'float_pos'", "'any_bottom'"),
        this.collectSolutions(indexedProgram, "idx(42.0, V).", "V")
    );
    assertEquals(
        asList("'any_top'", "'nil_list'", "'any_bottom'"),
        this.collectSolutions(indexedProgram, "idx([], V).", "V")
    );
    assertEquals(
        asList("'any_top'", "'list_12'", "'any_bottom'"),
        this.collectSolutions(indexedProgram, "idx([1, 2], V).", "V")
    );
    assertEquals(
        asList("'any_top'", "'struct_f1'", "'any_bottom'"),
        this.collectSolutions(indexedProgram, "idx(f(a), V).", "V")
    );
    assertEquals(
        asList("'any_top'", "'struct_f2'", "'any_bottom'"),
        this.collectSolutions(indexedProgram, "idx(f(a, b), V).", "V")
    );

    assertEquals(
        "['front_42','any_top','any_bottom','back_42']",
        this.proveSingle(
            indexedProgram,
            "asserta(idx(42, front_42)), assertz(idx(42, back_42)),"
                + " retract(idx(42, int_pos)), findall(V, idx(42, V), L).",
            "L"
        )
    );
  }

  @Test
  void testNonBacktrackableGlobalStoreSurvivesFailureAndClonesTerms() {
    try (final JProlContext context = this.makeTestContext(
        ctx -> {
        },
        new DefaultKeyValueTermStore("nb-store-test")
    )) {
      context.consult(new StringReader(
          "count_hits(Total) :-"
              + "  nb_setval(hits, 0),"
              + "  ( between(1, 5, _),"
              + "    nb_getval(hits, Cur),"
              + "    Next is Cur + 1,"
              + "    nb_setval(hits, Next),"
              + "    fail"
              + "  ; nb_getval(hits, Total)"
              + "  )."
      ));

      final JProlChoicePoint countCp = context.makeChoicePoint("count_hits(Total).");
      assertNotNull(countCp.prove());
      assertEquals("5", getVarAsText(countCp, "Total"));
      assertNull(countCp.prove());

      final JProlChoicePoint cloneIsolationCp = context.makeChoicePoint(
          "nb_setval(template, box(Free)),"
              + " nb_getval(template, box(a)),"
              + " nb_getval(template, box(Second)),"
              + " var(Second),"
              + " nb_delete(template)."
      );
      assertNotNull(cloneIsolationCp.prove());
      assertNull(cloneIsolationCp.prove());

      assertThrows(
          ProlExistenceErrorException.class,
          () -> context.makeChoicePoint("nb_getval(template, _).").prove()
      );
    }
  }

  @Test
  void testPrologLevelTriggersOnAssertRetractAndAbolish() {
    final String triggerProgram =
        ":- dynamic([watched/1, audit/2])."
            + "on_watch(Sig, Kind) :- assertz(audit(Sig, Kind)).";

    assertEquals(
        "['watched/1' - 'added','watched/1' - 'added','watched/1' - 'removed','watched/1' - 'abolished']",
        this.proveSingle(
            triggerProgram,
            "addtrigger(watched/1, [assert], on_watch(_, added)),"
                + " addtrigger(watched/1, [retract], on_watch(_, removed)),"
                + " addtrigger(watched/1, [abolish], on_watch(_, abolished)),"
                + " asserta(watched(1)),"
                + " assertz(watched(2)),"
                + " once(retract(watched(_))),"
                + " abolish(watched/1),"
                + " addtrigger(watched/1),"
                + " \\+ addtrigger(watched/1),"
                + " findall(S - K, audit(S, K), Log).",
            "Log"
        )
    );
  }

  @Test
  void testVariantStructuralEquivalencePreservesUnboundVariables() {
    this.checkOnce("f(X, g(X, Y)) =@= f(A, g(A, B)), var(X), var(Y), var(A), var(B).", true);
    this.checkOnce("f(X, g(X, X)) =@= f(A, g(A, B)), var(X), var(A), var(B).", false);
    this.checkOnce("X = Y, f(X, Y) @= f(A, A).", true);
    this.checkOnce("X = Y, f(X, Y) @= f(A, B).", false);
  }

  @Test
  void testHigherOrderBagofAndSetofWithMultipleFreeKeysAndExistentialChains() {
    final String staff =
        "rec(eng, dev, alice, 5). rec(eng, dev, bob, 3). rec(eng, dev, carol, 5)."
            + "rec(eng, qa, dave, 4). rec(ops, sre, eve, 7).";

    final List<String> groups = new ArrayList<>();
    try (final JProlContext context = this.makeContextAndConsult(staff)) {
      final JProlChoicePoint cp = context.makeChoicePoint(
          "setof(Years, Who^rec(Dept, Role, Who, Years), Levels), Row = Dept - Role - Levels."
      );
      while (cp.prove() != null) {
        groups.add(getVarAsText(cp, "Row"));
      }
    }
    assertEquals(
        asList("'eng' - 'dev' - [3,5]", "'eng' - 'qa' - [4]", "'ops' - 'sre' - [7]"),
        groups
    );
  }
}
