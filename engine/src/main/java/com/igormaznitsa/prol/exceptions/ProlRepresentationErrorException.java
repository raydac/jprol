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
 * The class describes the prolog representation_error exception. There shall be
 * a Representation Error when an implementation defined limit has been
 * breached.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlRepresentationErrorException extends ProlAbstractCatcheableException {
  private static final long serialVersionUID = -4337637438675029939L;

  /**
   * The constant has the common element for all instances of the exception
   */
  private static final Term TERM_ERROR = new Term("representation_error");
  /**
   * The variable contains the error flag
   */
  private final String flag;

  /**
   * A constructor
   *
   * @param flag the error flag
   * @param culprit the culprit term
   * @param cause the java throwable exception which is the cause for the error
   */
  public ProlRepresentationErrorException(final String flag, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.flag = flag;
  }

  /**
   * A constructor
   *
   * @param flag the error flag
   * @param message a text message describing the situation
   * @param culprit the culprit term
   * @param cause the java throwable exception which is the cause for the error
   */
  public ProlRepresentationErrorException(final String flag, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.flag = flag;
  }

  /**
   * A constructor
   *
   * @param flag the error flag
   * @param message a text message describing the situation
   * @param culprit the culprit term
   */
  public ProlRepresentationErrorException(final String flag, final String message, final Term culprit) {
    super(message, culprit);
    this.flag = flag;
  }

  /**
   * A constructor
   *
   * @param flag the error flag
   * @param culprit the culprit term
   */
  public ProlRepresentationErrorException(final String flag, final Term culprit) {
    super(culprit);
    this.flag = flag;
  }

  /**
   * Get the error flag of the exception
   *
   * @return the error flag of the exception as a String object
   */
  public String getFlag() {
    return flag;
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    final TermStruct result = new TermStruct(TERM_ERROR, new Term[]{this.flag == null ? UNDEFINED : new Term(this.flag)});
    result.setCarriedObject(this);
    return result;
  }
}
