package com.igormaznitsa.jprol.logic;

public final class NamedLockOwnerException extends IllegalStateException {

  private static final long serialVersionUID = 1L;

  public NamedLockOwnerException(final String message) {
    super(message);
  }
}
