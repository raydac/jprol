package com.igormaznitsa.jprol.exceptions;

import com.igormaznitsa.jprol.data.SourcePosition;

/**
 * Root abstract exception for context thread interruption exceptions.
 */
public abstract class ProlInterruptException extends ProlException {

  private final SourcePosition sourcePosition;

  public ProlInterruptException(String message, Throwable cause, SourcePosition sourcePosition) {
    super(message, cause);
    this.sourcePosition = sourcePosition == null ? SourcePosition.UNKNOWN : sourcePosition;
  }

  public SourcePosition getSourcePosition() {
    return this.sourcePosition;
  }
}
