package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;

/**
 * Minimal JProl library for the JSR 223 layer: defines logical names for console I/O routed through
 * {@link com.igormaznitsa.jprol.logic.io.IoResourceProvider} ({@value #WRITER_USER}, {@value #WRITER_ERR}, {@value #READER_USER}).
 *
 * @since 3.0.0
 */
public class JProlJsr223BootstrapLibrary extends AbstractJProlLibrary {

  /**
   * Alias for System.out
   */
  public static final String WRITER_USER = "user";

  /**
   * Alias for System.err
   */
  public static final String WRITER_ERR = "err";

  /**
   * Alias for System.in
   */
  public static final String READER_USER = "user";

  /**
   * Registers this library under the name {@code jprol-jsr223-bootstrap-lib}.
   */
  public JProlJsr223BootstrapLibrary() {
    super("jprol-jsr223-bootstrap-lib");
  }

}
