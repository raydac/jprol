/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prol.exceptions;

/**
 * The class describes the root exception for all exceptions which can be thrown
 * by a Prol engne in diffirent situations. It extends the RuntimeException so
 * it doesn't need try..catch in any cause but only if you want to catch it.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlException extends RuntimeException {
  private static final long serialVersionUID = -6503064090950753871L;

  /**
   * A constructor
   *
   * @param cause the root cause to throw the exception
   */
  public ProlException(final Throwable cause) {
    super(cause);
  }

  /**
   * A constructor allows to set a String message
   *
   * @param message the message which describes the situation
   * @param cause the root cause to throw the exception
   */
  public ProlException(final String message, final Throwable cause) {
    super(message, cause);
  }

  /**
   * A constructor
   *
   * @param message the message which describes the situation
   */
  public ProlException(final String message) {
    super(message);
  }

  /**
   * A constructor
   */
  public ProlException() {
    super();
  }

}
