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

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;

/**
 * The class describes a instantiation prolog exception. There shall be an
 * Instantiation Error when an argument or one of its components is a variable,
 * and an instantiated argument or component is required. This error should
 * therefor be raised whenever an argument of mode "+", or " @ " is encountered.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlInstantiationErrorException extends ProlAbstractCatcheableException {
  private static final long serialVersionUID = -5157739502740121566L;

  /**
   * The constant is shared between instances of the class and contains the term
   * with the string of the error
   */
  private static final Term TERM_ERROR = new Term("instantiation_error");

  /**
   * A constructor
   *
   * @param culprit the culprit term
   * @param cause the java throwable object which was the cause of the error
   */
  public ProlInstantiationErrorException(final Term culprit, final Throwable cause) {
    super(culprit, cause);
  }

  /**
   * A constructor
   *
   * @param message a string message describes the situation
   * @param culprit the culprit term
   * @param cause the java throwable object which was the cause of the error
   */
  public ProlInstantiationErrorException(final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
  }

  /**
   * A constructor
   *
   * @param message a string message describes the situation
   * @param culprit the culprit term
   */
  public ProlInstantiationErrorException(final String message, final Term culprit) {
    super(message, culprit);
  }

  /**
   * A constructor
   *
   * @param culprit the culprit term
   */
  public ProlInstantiationErrorException(final Term culprit) {
    super(culprit);
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    final TermStruct result = new TermStruct(TERM_ERROR);
    result.setCarriedObject(this);
    return result;
  }
}
