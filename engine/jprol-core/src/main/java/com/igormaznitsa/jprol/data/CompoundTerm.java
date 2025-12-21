package com.igormaznitsa.jprol.data;

import java.util.Spliterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public abstract class CompoundTerm extends Term implements Iterable<Term> {
  CompoundTerm(final String text, final Object payload, final SourcePosition sourcePosition) {
    super(text, payload, sourcePosition);
  }

  public Stream<Term> streamChildren() {
    return StreamSupport.stream(this.spliteratorChildren(), false);
  }

  public abstract Spliterator<Term> spliteratorChildren();
}
