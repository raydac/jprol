package com.igormaznitsa.prol.data;

import com.igormaznitsa.prol.libraries.PredicateProcessor;

public final class Terms {
  public static final TermInteger INT_ONE = new TermInteger(1);
  public static final TermInteger INT_ZERO = new TermInteger(0);
  public static final TermInteger INT_MINUS_ONE = new TermInteger(-1);
  public static final Term LIST_FUNCTOR = new Term(".");
  public static final TermList NULL_LIST = new TermList();

  public static Term newAtom(final String text) {
    if (".".equals(text)) {
      return LIST_FUNCTOR;
    } else {
      return new Term(text);
    }
  }

  public static TermFloat newFloat(final String text) {
    return new TermFloat(text);
  }

  public static TermFloat newFloat(final float value) {
    return new TermFloat(value);
  }

  public static TermInteger newInt(final String text) {
    return new TermInteger(text);
  }

  public static TermInteger newInt(final int value) {
    switch (value) {
      case -1:
        return INT_MINUS_ONE;
      case 0:
        return INT_ZERO;
      case 1:
        return INT_ONE;
      default:
        return new TermInteger(value);
    }
  }

  public static TermList newList(final Term term) {
    return new TermList(term);
  }

  public static TermList newList(final Term head, final Term tail) {
    return new TermList(head, tail);
  }

  public static Var newVar(final String name) {
    return new Var(name);
  }

  public static Var newVar() {
    return new Var();
  }

  public static TermStruct newStruct(final Term functor) {
    return new TermStruct(functor);
  }

  public static TermStruct newStruct(final String functor, final Term[] elements) {
    return new TermStruct(functor, elements);
  }

  public static TermStruct newStruct(final Term functor, final Term[] elements) {
    return new TermStruct(functor, elements);
  }

  public static TermStruct newStruct(final Term functor, final Term[] elements, final PredicateProcessor processor) {
    return new TermStruct(functor, elements, processor);
  }


}
