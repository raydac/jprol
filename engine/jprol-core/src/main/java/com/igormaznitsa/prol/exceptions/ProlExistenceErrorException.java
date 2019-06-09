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

public class ProlExistenceErrorException extends ProlAbstractCatcheableException {

  private static final long serialVersionUID = 8133227498750254779L;

  private static final Term ERROR_TERM = new Term("existence_error");

  private final String objectType;

  public ProlExistenceErrorException(final String objectType, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.objectType = objectType == null ? UNDEFINED.getText() : objectType;
  }

  public ProlExistenceErrorException(final String objectType, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.objectType = objectType == null ? UNDEFINED.getText() : objectType;
  }

  public ProlExistenceErrorException(final String objectType, final String message, final Term culprit) {
    super(message, culprit);
    this.objectType = objectType == null ? UNDEFINED.getText() : objectType;
  }

  public ProlExistenceErrorException(final String objectType, final Term culprit) {
    super(culprit);
    this.objectType = objectType == null ? UNDEFINED.getText() : objectType;
  }

  @Override
  public Term getErrorTerm() {
    return ERROR_TERM;
  }

  @Override
  public TermStruct getAsStruct() {
    return new TermStruct(ERROR_FUNCTOR, new Term[] {ERROR_TERM, new Term(this.objectType), getCulprit()});
  }

}
