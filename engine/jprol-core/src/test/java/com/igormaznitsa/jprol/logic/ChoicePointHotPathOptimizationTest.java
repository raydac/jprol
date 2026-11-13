package com.igormaznitsa.jprol.logic;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
  void testReusableProveStackSupportsNestedFindallAndMaxDepth() {
    assertEquals(
        "[[1,2],[2,3],[3,4]]",
        this.proveSingle(
            "base(1). base(2). base(3). step(X, Y) :- Y is X + 1. step(X, Y) :- Y is X + 2.",
            "findall(Sub, (base(X), findall(Y, step(X, Y), Sub)), All),"
                + " All = [[2,3],[3,4],[4,5]], R = [[1,2],[2,3],[3,4]].",
            "R"));

    try (final JProlContext context = this.makeContextAndConsult("inf :- inf.")) {
      context.setSystemFlag(JProlSystemFlag.MAX_PROVE_STACK_DEPTH, Terms.newLong(32L));
      final JProlChoicePoint cp = context.makeChoicePoint("inf.");
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
}
