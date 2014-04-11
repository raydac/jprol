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
 * The class describes the existence_error prolog exception. There shall be an
 * Existence Error when the object on which an operation is to be performed does
 * not exist.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlExistenceErrorException extends ProlAbstractCatcheableException {
  private static final long serialVersionUID = 8133227498750254779L;

  /**
   * The constant contains the shared term value between all instances of the
   * class
   */
  private static final Term ERROR_TERM = new Term("existence_error");

  /**
   * The variable contains the object type value
   */
  private final String objectType;

  /**
   * A constructor
   *
   * @param objectType the type of the object which can't be found
   * @param culprit the culprit term
   * @param cause the java throwable cause of the exception
   */
  public ProlExistenceErrorException(final String objectType, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.objectType = objectType;
  }

  /**
   * A constructor
   *
   * @param objectType the type of the object which can't be found
   * @param message a text message describes the situation
   * @param culprit the culprit term
   * @param cause the java throwable cause of the exception
   */
  public ProlExistenceErrorException(final String objectType, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.objectType = objectType;
  }

  /**
   * A constructor
   *
   * @param objectType the type of the object which can't be found
   * @param message a text message describes the situation
   * @param culprit the culprit term
   */
  public ProlExistenceErrorException(final String objectType, final String message, final Term culprit) {
    super(message, culprit);
    this.objectType = objectType;
  }

  /**
   * A constructor
   *
   * @param objectType the type of the object which can't be found
   * @param culprit the culprit term
   */
  public ProlExistenceErrorException(final String objectType, final Term culprit) {
    super(culprit);
    this.objectType = objectType;
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return ERROR_TERM;
  }

  @Override
  public TermStruct getAsStruct() {
    final TermStruct term = new TermStruct(objectType, new Term[]{objectType == null ? UNDEFINED : new Term(objectType), getCulprit() == null ? UNDEFINED : getCulprit()});
    term.setCarriedObject(this);
    return term;
  }

}
