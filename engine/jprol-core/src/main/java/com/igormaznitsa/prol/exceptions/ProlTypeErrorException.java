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

import static com.igormaznitsa.prol.data.TermList.asTermList;
import static com.igormaznitsa.prol.data.Terms.newAtom;

public class ProlTypeErrorException extends ProlAbstractCatcheableException {

  private static final long serialVersionUID = -3258214534404942716L;

  private static final Term TERM_ERROR = newAtom("type_error");

  private final String validType;

  public ProlTypeErrorException(final String validType, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.validType = validType == null ? UNDEFINED.getText() : validType;
  }

  public ProlTypeErrorException(final String validType, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.validType = validType == null ? UNDEFINED.getText() : validType;
  }

  public ProlTypeErrorException(final String validType, final String message, final Term culprit) {
    super(message, culprit);
    this.validType = validType == null ? UNDEFINED.getText() : validType;
  }

  public ProlTypeErrorException(final String validType, final Term culprit) {
    super(culprit);
    this.validType = validType == null ? UNDEFINED.getText() : validType;
  }

  public String getValidType() {
    return validType;
  }


  @Override
  public Term getErrorTerm() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    return this.makeErrorStruct(TERM_ERROR, asTermList(newAtom(this.validType), getCulprit()));
  }

}
