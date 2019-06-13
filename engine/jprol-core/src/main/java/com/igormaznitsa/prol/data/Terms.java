package com.igormaznitsa.prol.data;

import com.igormaznitsa.prol.libraries.PredicateProcessor;

public final class Terms {
  public static final TermLong INT_ONE = new TermLong(1L);
  public static final TermLong INT_ZERO = new TermLong(0L);
  public static final TermLong INT_MINUS_ONE = new TermLong(-1L);
  public static final Term LIST_FUNCTOR = new Term(".");
  public static final TermList NULL_LIST = new TermList();

  public static Term newAtom(final String text) {
    if (".".equals(text)) {
      return LIST_FUNCTOR;
    } else {
      return new Term(text);
    }
  }

  public static TermDouble newDouble(final String text) {
    return new TermDouble(text);
  }

  public static TermDouble newDouble(final double value) {
    return new TermDouble(value);
  }

  public static TermLong newLong(final String text) {
    return new TermLong(text);
  }

  public static TermLong newLong(final long value) {
    if (value == 0L) {
      return INT_ZERO;
    } else if (value == 1L) {
      return INT_ONE;
    } else if (value == -1L) {
      return INT_MINUS_ONE;
    } else {
      return new TermLong(value);
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
