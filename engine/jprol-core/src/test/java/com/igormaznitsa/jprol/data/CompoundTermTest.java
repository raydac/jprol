package com.igormaznitsa.jprol.data;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

class CompoundTermTest {

  @Test
  void testReplaceVarInDeepStructure() {
    TermList list = TermList.listOf(
        Terms.newAtom("a"),
        TermList.listOf(Terms.newAtom("b"),
            Terms.newStruct("aaa", new Term[] {TermList.listOf(Terms.newVar("X"))},
                SourcePosition.UNKNOWN))
    );

    list = (TermList) list.replaceVar("X", Terms.newAtom("replaced"));
    assertEquals("['a',['b',aaa(['replaced'])]]", list.toSrcString());
  }

  @Test
  void testList_streamChildren() {
    final TermList list = TermList.listOf(
        Terms.newAtom("a"),
        Terms.newAtom("b"),
        Terms.newStruct(Terms.newAtom("s"),
            new Term[] {TermList.listOf(Terms.TRUE, Terms.FALSE)}),
        Terms.newAtom("c")
    );
    assertEquals("'a';'b';s(['true','false']);'c'",
        list.streamChildren().map(Term::toSrcString).collect(Collectors.joining(";")));
    assertEquals("",
        NULL_LIST.streamChildren().map(Term::toSrcString).collect(Collectors.joining(";")));
  }

  @Test
  void testList_stream() {
    final TermList list = TermList.listOf(Terms.newAtom("a"), Terms.newAtom("b"),
        Terms.newStruct(Terms.newAtom("s"),
            new Term[] {TermList.listOf(Terms.TRUE, Terms.FALSE)}), Terms.newAtom("c"));
    assertEquals("'.';'a';'.';'b';'.';'s';'.';'true';'.';'false';'.';'.';'c';'.'",
        list.stream().map(Term::toSrcString).collect(Collectors.joining(";")));
  }

  @Test
  void testStruct_stream() {
    final TermStruct struct = Terms.newStruct(
        Terms.newAtom("abc"),
        Terms.newVar("T"),
        Terms.newAnonymousVar(),
        Terms.newStruct(Terms.newLong(1234), new Term[] {NULL_LIST}),
        TermList.listOf(Terms.TRUE, Terms.FALSE));
    assertEquals("'abc';T;_;1234;'.';'.';'true';'.';'false';'.'",
        struct.stream().map(Term::toSrcString).collect(Collectors.joining(";")));
  }

  @Test
  void testStruct_streamChildren() {
    final TermStruct struct = Terms.newStruct(
        Terms.newAtom("abc"),
        Terms.newVar("T"),
        Terms.newAnonymousVar(),
        Terms.newStruct(Terms.newLong(1234), new Term[] {NULL_LIST}),
        TermList.listOf(Terms.TRUE, Terms.FALSE));
    assertEquals("T;_;1234([]);['true','false']",
        struct.streamChildren().map(Term::toSrcString).collect(Collectors.joining(";")));
  }

}