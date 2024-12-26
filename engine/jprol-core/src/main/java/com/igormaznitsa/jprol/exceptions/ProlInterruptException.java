package com.igormaznitsa.jprol.exceptions;

import com.igormaznitsa.jprol.data.SourcePosition;

public abstract class ProlInterruptException extends ProlException {
  public ProlInterruptException(String message, Throwable cause) {
    super(message, cause);
  }

  public SourcePosition getSourcePosition() {
    return SourcePosition.UNKNOWN;
  }
}
