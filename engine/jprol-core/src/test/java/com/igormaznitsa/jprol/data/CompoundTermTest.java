package com.igormaznitsa.jprol.data;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

class CompoundTermTest {

  @Test
  void testList_streamChildren() {
    final TermList list = TermList.asTermList(
        Terms.newAtom("a"),
        Terms.newAtom("b"),
        Terms.newStruct(Terms.newAtom("s"),
            new Term[] {TermList.asTermList(Terms.TRUE, Terms.FALSE)}),
        Terms.newAtom("c")
    );
    assertEquals("'a';'b';s(['true','false']);'c'",
        list.streamChildren().map(Term::toSrcString).collect(Collectors.joining(";")));
    assertEquals("",
        Terms.NULL_LIST.streamChildren().map(Term::toSrcString).collect(Collectors.joining(";")));
  }

  @Test
  void testList_stream() {
    final TermList list = TermList.asTermList(Terms.newAtom("a"), Terms.newAtom("b"),
        Terms.newStruct(Terms.newAtom("s"),
            new Term[] {TermList.asTermList(Terms.TRUE, Terms.FALSE)}), Terms.newAtom("c"));
    assertEquals("'.';'a';'.';'b';'.';'s';'.';'true';'.';'false';'.';'.';'c';'.'",
        list.stream().map(Term::toSrcString).collect(Collectors.joining(";")));
  }

  @Test
  void testStruct_stream() {
    final TermStruct struct = Terms.newStruct(
        Terms.newAtom("abc"),
        new Term[] {
            Terms.newVar("T"),
            Terms.newVar(),
            Terms.newStruct(Terms.newLong(1234), new Term[] {Terms.NULL_LIST}),
            TermList.asTermList(Terms.TRUE, Terms.FALSE)}
    );
    assertEquals("'abc';T;_;1234;'.';'.';'true';'.';'false';'.'",
        struct.stream().map(Term::toSrcString).collect(Collectors.joining(";")));
  }

  @Test
  void testStruct_streamChildren() {
    final TermStruct struct = Terms.newStruct(
        Terms.newAtom("abc"),
        new Term[] {
            Terms.newVar("T"),
            Terms.newVar(),
            Terms.newStruct(Terms.newLong(1234), new Term[] {Terms.NULL_LIST}),
            TermList.asTermList(Terms.TRUE, Terms.FALSE)}
    );
    assertEquals("T;_;1234([]);['true','false']",
        struct.streamChildren().map(Term::toSrcString).collect(Collectors.joining(";")));
  }

}