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
 * The class describes a Prol exception which will be thrown if there is any
 * error during the goal processing on the parsing or logical level.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlWrongGoalException extends ProlException {
  private static final long serialVersionUID = 9055754857879445135L;

  /**
   * A constructor
   */
  public ProlWrongGoalException() {
    super();
  }

  /**
   * A constructor
   *
   * @param message the text message describing the exception
   */
  public ProlWrongGoalException(final String message) {
    super(message);
  }

  /**
   * A constructor
   *
   * @param message the text message describing the exception
   * @param cause a java throwable object which is the cause of the exception
   */
  public ProlWrongGoalException(final String message, final Throwable cause) {
    super(message, cause);
  }

  /**
   * A constructor
   *
   * @param cause a java throwable object which is the cause of the exception
   */
  public ProlWrongGoalException(final Throwable cause) {
    super(cause);
  }

}
