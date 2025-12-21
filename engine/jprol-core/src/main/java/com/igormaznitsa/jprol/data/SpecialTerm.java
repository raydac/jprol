package com.igormaznitsa.jprol.data;

import java.util.stream.Stream;

public abstract class SpecialTerm extends Term {
  public SpecialTerm(final String text, final SourcePosition sourcePosition) {
    super(text, sourcePosition);
  }

  protected static <T> T throwUnsupportedException() {
    throw new UnsupportedOperationException("Unsupported in special terms");
  }

  @Override
  public final Stream<Term> stream() {
    return throwUnsupportedException();
  }

  @Override
  public final boolean dryUnifyTo(final Term target) {
    return throwUnsupportedException();
  }

  @Override
  public final boolean unifyTo(final Term other) {
    return throwUnsupportedException();
  }
}
