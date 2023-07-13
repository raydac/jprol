package com.igormaznitsa.jprol.libs;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import java.util.List;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

class JProlRegexLibraryTest extends AbstractJProlTest {

  @Test
  void testRegexSplitOk() {
    assertRegexSplit("\\s*[a-zA-Z]+\\s*", "a + b - c * d / e < f > g >= h <= i == j",
        emptyList(), asList("", "+", "-", "*", "/", "<", ">", ">=", "<=", "=="));
    assertRegexSplit("-", "016-78967",
        emptyList(), asList("016", "78967"));
    assertRegexSplit("\\r?\\n|\\r", "Line1\r\nLine2\r\nLine3",
        emptyList(), asList("Line1", "Line2", "Line3"));
    assertRegexSplit("A", "1A2A3A4A5a6",
        emptyList(), asList("1", "2", "3", "4", "5a6"));
    assertRegexSplit("A", "1A2A3A4A5a6",
        singletonList("CASE_INSENSITIVE"), asList("1", "2", "3", "4", "5", "6"));
  }

  @Test
  void testRegexSplitError() {
    assertThrowsExactly(ProlDomainErrorException.class,
        () -> assertRegexSplit("+++++", "abc", emptyList(), emptyList()));
    assertThrowsExactly(ProlDomainErrorException.class,
        () -> assertRegexSplit("A", "1A2A3A4", singletonList("HAHA"), emptyList()));
  }

  private void assertRegexSplit(final String regex, final String text, final List<String> flags,
                                final List<String> expected) {
    final JProlContext context = this.makeTestContext();
    final PredicateInvoker invoker =
        context.findAllPredicateInvokersForSignature("regex_split/4").stream().findFirst()
            .orElseThrow(() -> new IllegalStateException("Can't find regex_split/4 in libraries"));
    final Term goal = Terms.newStruct(Terms.newAtom("regex_split"),
        new Term[] {Terms.newAtom(regex), Terms.newAtom(text), Terms.newVar("X"),
            TermList.asList(flags.stream().map(Terms::newAtom).collect(Collectors.toList()))},
        invoker);
    final JProlChoicePoint choicePoint = new JProlChoicePoint(goal, context);
    final Term result = choicePoint.prove();
    assertNotNull(result, "Must have prove result");
    final TermList splitResult = choicePoint.findVar("X").get().findNonVarOrSame();
    final List<String> splitAsList = splitResult.streamChildren().map(Term::getText).collect(
        Collectors.toList());
    assertEquals(expected, splitAsList);
  }

  private JProlContext makeTestContext() {
    return this.prepareContext("", new JProlCoreLibrary(), new JProlRegexLibrary());
  }


}