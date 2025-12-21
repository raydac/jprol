package com.igormaznitsa.jprol.data;

import java.util.stream.Stream;

public abstract class SpecialTerm extends Term {
  public SpecialTerm(final String text, final SourcePosition sourcePosition) {
    super(text, sourcePosition);
  }

  @Override
  public Stream<Term> stream() {
    throw new UnsupportedOperationException("Not supported for such kind of term");
  }

  @Override
  public boolean dryUnifyTo(final Term target) {
    throw new UnsupportedOperationException("Not supported for such kind of term");
  }

  @Override
  public boolean unifyTo(final Term other) {
    throw new UnsupportedOperationException("Not supported for such kind of term");
  }
}
