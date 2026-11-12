package com.igormaznitsa.jprol.exceptions;

import com.igormaznitsa.jprol.logic.JProlContext;

/**
 * Thrown if JProl context has caught some exception non-processed on its level.
 *
 * @since 3.2.0
 */
public final class ProlContextUncaughtException extends ProlException {

  private final JProlContext source;

  public ProlContextUncaughtException(final JProlContext source, final String message,
                                      final Throwable cause) {
    super(message, cause);
    this.source = source;
  }

  public JProlContext getSource() {
    return this.source;
  }

  @Override
  public String toString() {
    return "ProlContextUncaughtException{" + "source=" + source + ",cause=" + this.getCause() + '}';
  }
}
