package com.igormaznitsa.jprol.exceptions;

/**
 * Exception shows that a JProl context process successfully aborted.
 *
 * @since 3.0.0
 */
public class ProlAbortExecutionException extends ProlHaltExecutionException {

  public ProlAbortExecutionException() {
    this("Aborted", 0L);
  }

  public ProlAbortExecutionException(final long status) {
    this("Aborted", status);
  }

  public ProlAbortExecutionException(final String cause, final long status) {
    super(cause, status);
  }

}
