package com.igormaznitsa.jprol.exceptions;

import com.igormaznitsa.jprol.data.SourcePosition;

/**
 * Exception shows that a JProl context process expectedly aborted.
 *
 * @since 3.0.0
 */
public class ProlAbortExecutionException extends ProlInterruptException {

  private final long status;

  public ProlAbortExecutionException() {
    super("Aborted.", null, null);
    this.status = 0L;
  }

  public ProlAbortExecutionException(final long status) {
    super("Aborted.", null, null);
    this.status = status;
  }

  public ProlAbortExecutionException(final String cause, final long status,
                                     final SourcePosition sourcePosition) {
    super(cause, null, sourcePosition);
    this.status = status;
  }

  public long getStatus() {
    return this.status;
  }

}
