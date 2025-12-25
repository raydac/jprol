package com.igormaznitsa.jprol.exceptions;

public class ProlArgumentValidationException extends ProlException {
  public ProlArgumentValidationException(final Throwable cause) {
    this(null, cause);
  }

  public ProlArgumentValidationException(final String message, final Throwable cause) {
    super(message, cause);
  }

  public ProlArgumentValidationException(final String message) {
    this(message, null);
  }

  public ProlArgumentValidationException() {
    this("Detected argument violating rules");
  }
}
