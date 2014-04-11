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
 * The class describes the exception whicwill be thrown if there is any
 * arithmetic exception in an evaluable predicate. There shall be an Evaluation
 * Error when the operands of an evaluable functor has an exceptional value.
 * Inside of the prol engine the exception just override standard Java
 * arithmetic exceptions so the error situation for every exception will be
 * described like Java exception but not as it is defined in ISO prolog
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlEvaluationErrorException extends ProlAbstractCatcheableException {
  private static final long serialVersionUID = -8773616695972049425L;

  /**
   * The constant contains the value shared between all instances of the
   * exception
   */
  private static final Term TERM_ERROR = new Term("evaluation_error");

  /**
   * The variable contains the string describes the error situation
   */
  private final String error;

  /**
   * A constructor
   *
   * @param error the error situation describer
   * @param culprit the culprit term
   * @param cause the throwable cause of the exception
   */
  public ProlEvaluationErrorException(final String error, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.error = error;
  }

  /**
   * A constructor
   *
   * @param error the error situation describer
   * @param message a human situation describer
   * @param culprit the culprit term
   * @param cause the throwable cause of the exception
   */
  public ProlEvaluationErrorException(final String error, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.error = error;
  }

  /**
   * A constructor
   *
   * @param error the error situation describer
   * @param message a human situation describer
   * @param culprit the culprit term
   */
  public ProlEvaluationErrorException(final String error, final String message, final Term culprit) {
    super(message, culprit);
    this.error = error;
  }

  /**
   * A constructor
   *
   * @param error the error situation describer
   * @param culprit the culprit term
   */
  public ProlEvaluationErrorException(final String error, final Term culprit) {
    super(culprit);
    this.error = error;
  }

  /**
   * Get the situation describer
   *
   * @return the situation describer as a String
   */
  public String getError() {
    return this.error;
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    final TermStruct result = new TermStruct(TERM_ERROR, new Term[]{this.error == null ? UNDEFINED : new Term(this.error)});
    result.setCarriedObject(this);
    return result;
  }
}
