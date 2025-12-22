package com.igormaznitsa.jprol.exceptions;

import java.io.IOException;

/**
 * Runtime wrapper for IOException
 *
 * @since 2.3.0
 */
public class RuntimeIOException extends RuntimeException {

  public RuntimeIOException(final IOException exception) {
    super(exception.getMessage(), exception);
  }

  public RuntimeIOException(final String message, final IOException exception) {
    super(message, exception);
  }

}
