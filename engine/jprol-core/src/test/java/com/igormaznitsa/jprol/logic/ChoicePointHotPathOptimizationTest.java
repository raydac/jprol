package com.igormaznitsa.jprol.logic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlChoicePointStackOverflowException;
import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.trace.JProlContextListener;
import com.igormaznitsa.jprol.trace.TraceEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

@Timeout(20)
class ChoicePointHotPathOptimizationTest extends AbstractJProlTest {

  @Test
  void testTraceEventsWithConjunctionsAndBacktracking() {
    final List<String> events = new ArrayList<>();
    try (final JProlContext context = this.makeContextAndConsult(
        "step(1). step(2). pair(X, Y) :- step(X), step(Y), X < Y.")) {
      context.setSystemFlag(JProlSystemFlag.TRACE, Terms.TRUE);
      context.addContextListener(new JProlContextListener() {
        @Override
        public void onChoicePointTraceEvent(
            final JProlContext source,
            final JProlChoicePoint choicePoint,
            final TraceEvent event
        ) {
          events.add(event.name() + ':' + choicePoint.getGoalTerm().getText());
        }
      });

      final JProlChoicePoint cp = context.makeChoicePoint("pair(A, B).");
      assertNotNull(cp.prove());
      assertEquals("1", getVarAsText(cp, "A"));
      assertEquals("2", getVarAsText(cp, "B"));
      assertNull(cp.prove());

      assertTrue(events.contains("CALL:pair"));
      assertTrue(events.contains("CALL:step"));
      assertTrue(events.contains("EXIT:step"));
      assertTrue(events.contains("FAIL:<"));
    }
  }

  @Test
  void testPredefinedVariablesResetOnBacktrackingAndFailure() {
    try (final JProlContext context = this.makeContextAndConsult(
        "pick(1, a). pick(1, b). pick(2, c).")) {
      final JProlChoicePoint cp = context.makeChoicePoint(
          "pick(K, V).",
          null,
          Collections.singletonMap("K", Terms.newLong(1L)));

      assertNotNull(cp.prove());
      assertEquals("'a'", getVarAsText(cp, "V"));
      assertNotNull(cp.prove());
      assertEquals("'b'", getVarAsText(cp, "V"));
      assertNull(cp.prove());
      assertEquals("K", cp.findVar("K").orElseThrow().getText());
      assertEquals("1", getVarAsText(cp, "K"));
    }
  }

  @Test
  void testSharedVariableNonlinearClauseHeadMatching() {
    assertEquals(
        Arrays.asList("1", "3"),
        this.collectSolutions(
            "same(X, X). check(A, B, A) :- same(A, B).",
            "member(A-B, [1-1, 1-2, 3-3, 4-5]), check(A, B, R).",
            "R"));

    assertEquals(
        Collections.singletonList("'ok'"),
        this.collectSolutions(
            "pair_match(f(X, g(X)), h(X)).",
            "pair_match(f(Y, g(a)), h(Y)), Y == a, R = ok.",
            "R"));

    this.checkOnce("pair_match(f(X, g(X)), h(X)).", "pair_match(f(a, g(b)), _).", false);
  }

  @Test
  void testManyVariablesInClauseHeadExceedingSmallMapThreshold() {
    final String rule =
        "wide(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,Sum) :- "
            + "Sum is V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20.";
    assertEquals(
        "210",
        this.proveSingle(rule, "wide(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,S).",
            "S"));
  }

  @Test
  void testDeepListOperationsDoNotOverflowJavaStack() {
    final int size = 5000;
    final List<Term> elements = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      elements.add(Terms.newVar("X" + i));
    }
    final TermList longList = TermList.listOf(elements);

    final Map<String, ?> named = longList.findAllNamedVariables();
    assertEquals(size, named.size());

    final long streamedVars = longList.variables().count();
    assertEquals(size, streamedVars);

    final Term cloned = longList.makeClone();
    assertTrue(longList.isUnifiableWith(cloned));
    assertTrue(longList.unifyWith(cloned));

    final List<Term> ints1 = new ArrayList<>(size);
    final List<Term> ints2 = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      ints1.add(Terms.newLong(i));
      ints2.add(Terms.newLong(i == size - 1 ? i + 1 : i));
    }
    final TermList groundList1 = TermList.listOf(ints1);
    final TermList groundList2 = TermList.listOf(ints2);
    assertTrue(groundList1.isGround());
    try (final JProlContext context = this.makeTestContext()) {
      final JProlChoicePoint cp = context.makeChoicePoint("true.");
      assertTrue(cp.compare(groundList1, groundList2) < 0);
      assertEquals(0, cp.compare(groundList1, groundList1.makeClone()));
    }
  }

  @Test
  void testReusableProveStackSupportsNestedFindallAndNonTailMaxDepth() {
    assertEquals(
        "[[1,2],[2,3],[3,4]]",
        this.proveSingle(
            "base(1). base(2). base(3). step(X, Y) :- Y is X + 1. step(X, Y) :- Y is X + 2.",
            "findall(Sub, (base(X), findall(Y, step(X, Y), Sub)), All),"
                + " All = [[2,3],[3,4],[4,5]], R = [[1,2],[2,3],[3,4]].",
            "R"));

    try (final JProlContext context = this.makeContextAndConsult("non_tail :- non_tail, true.")) {
      context.setSystemFlag(JProlSystemFlag.MAX_PROVE_STACK_DEPTH, Terms.newLong(32L));
      final JProlChoicePoint cp = context.makeChoicePoint("non_tail.");
      assertThrows(ProlChoicePointStackOverflowException.class, cp::prove);
    }
  }

  @Test
  void testStreamOrderPreservedOnNestedStructures() {
    final Term term = Terms.newStruct(
        Terms.newAtom("root"),
        TermList.listOf(Terms.newVar("A"), Terms.newLong(1L)),
        Terms.newStruct(Terms.newAtom("leaf"), Terms.newVar("B")));
    final String joined = term.stream().map(Term::toSrcString).collect(Collectors.joining(","));
    assertEquals("'root','.',A,'.',1,'.','leaf',B", joined);
  }

  @Test
  void testTailRecursionRunsInConstantProveStackDepth() {
    final String program =
        "count(N, N) :- !. count(I, N) :- I1 is I + 1, count(I1, N)."
            + "range(H, H, [H]) :- !. range(L, H, [L|T]) :- L1 is L + 1, range(L1, H, T)."
            + "sum([], A, A). sum([H|T], A, S) :- A1 is A + H, sum(T, A1, S)."
            +
            "countdown(N, Acc, R) :- N > 0, !, N1 is N - 1, Acc1 is Acc + N, countdown(N1, Acc1, R)."
            + "countdown(_, Acc, Acc)."
            + "ev(0) :- !. ev(N) :- N > 0, M is N - 1, od(M)."
            + "od(N) :- N > 0, M is N - 1, ev(M)."
            +
            "branch_loop(N, Acc, R) :- (N > 0 -> N1 is N - 1, Acc1 is Acc + 1, branch_loop(N1, Acc1, R) ; R = Acc)."
            + "wrap(0, f(done)) :- !. wrap(N, f(Y)) :- N1 is N - 1, wrap(N1, f(Y)).";

    try (final JProlContext context = this.makeContextAndConsult(program)) {
      context.setSystemFlag(JProlSystemFlag.MAX_PROVE_STACK_DEPTH, Terms.newLong(16L));

      assertNotNull(context.makeChoicePoint("count(0, 50000).").prove());

      final JProlChoicePoint rangeSum =
          context.makeChoicePoint("range(1, 20000, L), sum(L, 0, S).");
      assertNotNull(rangeSum.prove());
      assertEquals("200010000", getVarAsText(rangeSum, "S"));

      final JProlChoicePoint countdown = context.makeChoicePoint("countdown(20000, 0, R).");
      assertNotNull(countdown.prove());
      assertEquals("200010000", getVarAsText(countdown, "R"));
      assertNull(countdown.prove());

      assertNotNull(context.makeChoicePoint("ev(30000).").prove());
      assertNull(context.makeChoicePoint("od(30000).").prove());

      final JProlChoicePoint branch = context.makeChoicePoint("branch_loop(30000, 0, R).");
      assertNotNull(branch.prove());
      assertEquals("30000", getVarAsText(branch, "R"));

      final JProlChoicePoint wrapped = context.makeChoicePoint("wrap(20000, f(Out)).");
      assertNotNull(wrapped.prove());
      assertEquals("'done'", getVarAsText(wrapped, "Out"));
    }
  }

  @Test
  void testTailRecursiveBacktrackingGeneratorInConstantStackDepth() {
    final String generator =
        "gen_from(N, Max, N) :- N =< Max."
            + "gen_from(N, Max, X) :- N < Max, N1 is N + 1, gen_from(N1, Max, X).";

    try (final JProlContext context = this.makeContextAndConsult(generator)) {
      context.setSystemFlag(JProlSystemFlag.MAX_PROVE_STACK_DEPTH, Terms.newLong(16L));
      final JProlChoicePoint cp =
          context.makeChoicePoint("findall(X, gen_from(1, 12000, X), L), length(L, Len).");
      assertNotNull(cp.prove());
      assertEquals("12000", getVarAsText(cp, "Len"));
    }
  }

  @Test
  void testCutScopePreservedAcrossTailReplacedFrames() {
    assertEquals(
        Arrays.asList("1", "99", "2"),
        this.collectSolutions(
            "outer(X) :- mid(X). outer(2). mid(X) :- !, leaf(X). leaf(1). leaf(99).",
            "outer(X).",
            "X"));

    assertEquals(
        Arrays.asList("1", "2"),
        this.collectSolutions(
            "outer(X) :- mid(X). outer(2). mid(X) :- !, leaf(X). mid(88). leaf(1).",
            "outer(X).",
            "X"));

    assertEquals(
        Arrays.asList("1", "2"),
        this.collectSolutions(
            "outer(X) :- mid(X). outer(2). mid(X) :- leaf(X). leaf(1) :- !. leaf(99).",
            "outer(X).",
            "X"));

    assertEquals(
        Collections.singletonList("1"),
        this.collectSolutions(
            "outer(X) :- !, mid(X). outer(2). mid(X) :- leaf(X). leaf(1).",
            "outer(X).",
            "X"));

    assertEquals(
        Arrays.asList("7", "777"),
        this.collectSolutions(
            "src(1). src(2). tail_mul(Y, R) :- R is Y * 7. outer(X) :- src(Y), !!, tail_mul(Y, X). outer(777).",
            "outer(X).",
            "X"));
  }

  @Test
  void testDeeplyLeftNestedConjunctionsPreserveAllContinuations() {
    try (final JProlContext context = this.makeTestContext()) {
      final JProlChoicePoint cp = context.makeChoicePoint(
          "(((A = 1, (B = 2, C = 3)), (D = 4, E = 5)), F is A + B + C + D + E).");
      assertNotNull(cp.prove());
      assertEquals("15", getVarAsText(cp, "F"));
      assertFalse(cp.prove() != null);
    }
  }

  @Test
  void testUnifyTrialGrowthAndCrossSideTransitiveVariableAliasing() {
    final String nonlinearTwelveVars =
        "nl12(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,"
            + "V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12, matched).";

    assertEquals(
        "'matched'",
        this.proveSingle(
            nonlinearTwelveVars,
            "nl12(1,2,3,4,5,6,7,8,9,10,11,12, 1,2,3,4,5,6,7,8,9,10,11,12, R).",
            "R"
        )
    );
    this.checkOnce(
        nonlinearTwelveVars,
        "nl12(1,2,3,4,5,6,7,8,9,10,11,12, 1,2,3,4,5,6,7,8,9,10,11,99, _).",
        false
    );

    final String transitiveAlias =
        "alias_match(X, X, f(g(Y), g(a)), Y)."
            + "alias_fail(X, X, f(g(a), g(b))).";
    assertEquals(
        "'a' - g('a')",
        this.proveSingle(
            transitiveAlias,
            "alias_match(A, B, f(A, B), Y), A == B, R = Y - A.",
            "R"
        )
    );
    this.checkOnce(transitiveAlias, "alias_fail(A, B, f(A, B)).", false);
  }

  @Test
  void testGoalClonerOverflowBeyondSixteenUnboundGoalVariablesWithPartialSharing() {
    final String multiClauseNonTail =
        "bind20([A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20],"
            + " shared(const_tree(1, 2)), first) :-"
            + "  A1 = 10, A10 = 20, A17 = 30, A20 = 40, check_tag(first)."
            + "bind20([A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20],"
            + " shared(const_tree(1, 2)), second) :-"
            + "  A1 = 100, A18 = 200, A20 = 300, check_tag(second)."
            + "check_tag(second).";

    assertEquals(
        "[100,200,300] - true",
        this.proveSingle(
            multiClauseNonTail,
            "Vars = [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20],"
                + " bind20(Vars, shared(const_tree(1, 2)), second),"
                + " (var(X10), var(X17) -> Clean = true ; Clean = leaked),"
                + " R = [X1, X18, X20] - Clean.",
            "R"
        )
    );
  }

  @Test
  void testThreeWayMutualTailRecursionMatchesBetweenNormalAndTraceModes() {
    final String threeWay =
        "mod3_step(0, Acc, Acc) :- !."
            + "mod3_step(N, Acc, Out) :-"
            + "  Rem is N mod 3,"
            + "  ( Rem =:= 0 -> s0(N, Acc, Out)"
            + "  ; Rem =:= 1 -> s1(N, Acc, Out)"
            + "  ; s2(N, Acc, Out)"
            + "  )."
            + "s0(N, Acc, Out) :- N1 is N - 1, Acc1 is Acc + 3, mod3_step(N1, Acc1, Out)."
            + "s1(N, Acc, Out) :- N1 is N - 1, Acc1 is Acc + 1, mod3_step(N1, Acc1, Out)."
            + "s2(N, Acc, Out) :- N1 is N - 1, Acc1 is Acc + 2, mod3_step(N1, Acc1, Out).";

    try (final JProlContext fastCtx = this.makeContextAndConsult(threeWay)) {
      fastCtx.setSystemFlag(JProlSystemFlag.MAX_PROVE_STACK_DEPTH, Terms.newLong(16L));
      final JProlChoicePoint deepCp = fastCtx.makeChoicePoint("mod3_step(24000, 0, Out).");
      assertNotNull(deepCp.prove());
      assertEquals("48000", getVarAsText(deepCp, "Out"));
      assertNull(deepCp.prove());

      final JProlChoicePoint smallFast = fastCtx.makeChoicePoint("mod3_step(60, 0, Out).");
      assertNotNull(smallFast.prove());
      final String fastValue = getVarAsText(smallFast, "Out");

      try (final JProlContext traceCtx = this.makeContextAndConsult(threeWay)) {
        traceCtx.setSystemFlag(JProlSystemFlag.TRACE, Terms.TRUE);
        final JProlChoicePoint smallTrace = traceCtx.makeChoicePoint("mod3_step(60, 0, Out).");
        assertNotNull(smallTrace.prove());
        assertEquals(fastValue, getVarAsText(smallTrace, "Out"));
      }
    }
  }

  @Test
  void testCutCollapseInTailPositionPreservesGrandparentChoicePoints() {
    final String program =
        "seed(10). seed(20). seed(30)."
            + "wrapper(X, Y) :- cut_worker(X, 3, Y)."
            +
            "cut_worker(Acc, N, Out) :- N > 0, !, N1 is N - 1, Acc1 is Acc + 1, cut_worker(Acc1, N1, Out)."
            + "cut_worker(Out, _, Out)."
            + "pipeline(R) :- seed(S), wrapper(S, W), W > 15, R is W * 2.";

    assertEquals(
        Arrays.asList("46", "66"),
        this.collectSolutions(program, "pipeline(R).", "R")
    );
  }
}
