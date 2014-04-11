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
 * The class describes the exception which will be thrown by a Prol engine when
 * the halt/1 predicate has been found. The status contains the numeric value
 * from the predicate.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlHaltExecutionException extends ProlException {
  private static final long serialVersionUID = -8265930292043474634L;

  /**
   * The variable contains the numeric status of the halt operation
   */
  private final int status;

  /**
   * A constructor, the status will be set in 0.
   */
  public ProlHaltExecutionException() {
    super("The program is halted.");
    this.status = 0;
  }

  /**
   * A constructor
   *
   * @param status the numeric status of the halt operation
   */
  public ProlHaltExecutionException(final int status) {
    super("The program is halted.");
    this.status = status;
  }

  /**
   * A constructor
   *
   * @param cause a string describes the cause, can be null
   * @param status the status code of the situation
   */
  public ProlHaltExecutionException(final String cause, final int status) {
    super(cause);
    this.status = status;
  }

  /**
   * Get the status of the halt operation
   *
   * @return the status of the halt operation as an integer
   */
  public int getStatus() {
    return this.status;
  }
}
