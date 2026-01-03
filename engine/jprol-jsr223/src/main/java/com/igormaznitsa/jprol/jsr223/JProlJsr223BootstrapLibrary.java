package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;

/**
 * Bootstrap JProl library to be used in JSR 223 engine wrapper.
 *
 * @since 2.2.2
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

  public JProlJsr223BootstrapLibrary() {
    super("jprol-jsr223-bootstrap-lib");
  }

}
