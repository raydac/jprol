package com.igormaznitsa.jprol.libs;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
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
  void testRegexFind() {
    assertRegexFind("(a*b)(foo)", "aabfooaabfooabfoob",
        emptyList(), asList("aab", "foo", "aab", "foo", "ab", "foo"));
    assertRegexFind("(foo)", "aabdsasdasdfdfsdf",
        emptyList(), emptyList());
    assertRegexFind("hello", "djaskhaadfashellofsdfdsfsd",
        emptyList(), singletonList("hello"));
    assertRegexFind("hello", "djaskhaadfasHellofsdfdsfsd",
        emptyList(), emptyList());
    assertRegexFind("hello", "djaskhaadfasHellofsdfdsfsd",
        singletonList("case_insensitive"), singletonList("Hello"));

    assertThrowsExactly(ProlDomainErrorException.class,
        () -> assertRegexFind("hello", "djaskhaadfasHellofsdfdsfsd",
            singletonList("case"), singletonList("Hello")));
  }

  @Test
  void testRegexReplaceAll() {
    assertRegexReplaceAll("\"\"\\]", "[\"aaa\",\"bbb\"\"]", "\"]", "[\"aaa\",\"bbb\"]");
    assertRegexReplaceAll("(?<=\")\\d+\\.\\s*", "[\"1.aaa\",\"2.bbb\"]", "", "[\"aaa\",\"bbb\"]");
    assertRegexReplaceAll("(?<=\")\"(?=\\])|(?<=\")\\d+\\.\\s*", "[\"1.aaa\",\"2.bbb\"\"]", "",
        "[\"aaa\",\"bbb\"]");
  }

  @Test
  void testRegexMatches() {
    assertRegexMatches("\\d", "abc", emptyList(), false);
    assertRegexMatches("\\d", "1", emptyList(), true);
    assertRegexMatches("\\d", "4443", emptyList(), false);
    assertRegexMatches("\\d", "2233abc", emptyList(), false);
    assertRegexMatches("\\D*", "nondigit", emptyList(), true);
    assertRegexMatches(".*Homini.*", "homa homini lupus est", singletonList("case_insensitive"),
        true);

    assertThrowsExactly(ProlDomainErrorException.class,
        () -> assertRegexMatches(".*Homini.*", "homa homini lupus est", singletonList("case"),
            true));
  }

  @Test
  void testRegexSplit() {
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
    final TermList splitResult = choicePoint.findVar("X").get().tryGround();
    final List<String> splitAsList = splitResult.streamChildren().map(Term::getText).collect(
        Collectors.toList());
    assertEquals(expected, splitAsList);
  }

  private void assertRegexMatches(final String regex, final String text, final List<String> flags,
                                  final boolean expected) {
    final JProlContext context = this.makeTestContext();
    final PredicateInvoker invoker =
        context.findAllPredicateInvokersForSignature("regex_matches/3").stream().findFirst()
            .orElseThrow(
                () -> new IllegalStateException("Can't find regex_matches/3 in libraries"));
    final Term goal = Terms.newStruct(Terms.newAtom("regex_matches"),
        new Term[] {Terms.newAtom(regex), Terms.newAtom(text),
            TermList.asList(flags.stream().map(Terms::newAtom).collect(Collectors.toList()))},
        invoker);
    final JProlChoicePoint choicePoint = new JProlChoicePoint(goal, context);
    final Term result = choicePoint.prove();
    if (expected) {
      assertNotNull(result);
    } else {
      assertNull(result);
    }
  }

  private void assertRegexFind(final String regex, final String text, final List<String> flags,
                               final List<String> expected) {
    final JProlContext context = this.makeTestContext();
    final PredicateInvoker invoker =
        context.findAllPredicateInvokersForSignature("regex_find/4").stream().findFirst()
            .orElseThrow(() -> new IllegalStateException("Can't find regex_find/4 in libraries"));
    final Term goal = Terms.newStruct(Terms.newAtom("regex_find"),
        new Term[] {Terms.newAtom(regex), Terms.newAtom(text), Terms.newVar("X"),
            TermList.asList(flags.stream().map(Terms::newAtom).collect(Collectors.toList()))},
        invoker);
    final JProlChoicePoint choicePoint = new JProlChoicePoint(goal, context);
    final Term result = choicePoint.prove();
    assertNotNull(result, "Must have prove result");
    final TermList splitResult = choicePoint.findVar("X").get().tryGround();
    final List<String> splitAsList = splitResult.streamChildren().map(Term::getText).collect(
        Collectors.toList());
    assertEquals(expected, splitAsList);
  }

  private void assertRegexReplaceAll(final String regex, final String text,
                                     final String replacement,
                                     final String expectedResult) {
    final JProlContext context = this.makeTestContext();
    final PredicateInvoker invoker =
        context.findAllPredicateInvokersForSignature("regex_replace_all/4").stream().findFirst()
            .orElseThrow(
                () -> new IllegalStateException("Can't find regex_replace_all/4 in libraries"));
    final Term goal = Terms.newStruct(Terms.newAtom("regex_replace_all"),
        new Term[] {Terms.newAtom(regex), Terms.newAtom(text), Terms.newAtom(replacement),
            Terms.newVar("X")},
        invoker);
    final JProlChoicePoint choicePoint = new JProlChoicePoint(goal, context);
    final Term result = choicePoint.prove();
    assertNotNull(result, "Must have prove result");
    final Term foundResult = choicePoint.findVar("X").get().tryGround();
    assertEquals(expectedResult, foundResult.getText());
  }

  private JProlContext makeTestContext() {
    return this.prepareContext("", new JProlCoreLibrary(), new JProlRegexLibrary());
  }


}