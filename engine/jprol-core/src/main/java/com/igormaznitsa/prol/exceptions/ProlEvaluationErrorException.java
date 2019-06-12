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
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.data.TermStruct;

import static com.igormaznitsa.prol.data.Terms.newAtom;

public class ProlEvaluationErrorException extends ProlAbstractCatcheableException {

  private static final long serialVersionUID = -8773616695972049425L;

  private static final Term TERM_ERROR = newAtom("evaluation_error");

  private final String error;

  public ProlEvaluationErrorException(final String error, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.error = error == null ? UNDEFINED.getText() : error;
  }

  public ProlEvaluationErrorException(final String error, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.error = error == null ? UNDEFINED.getText() : error;
  }

  public ProlEvaluationErrorException(final String error, final String message, final Term culprit) {
    super(message, culprit);
    this.error = error == null ? UNDEFINED.getText() : error;
  }

  public ProlEvaluationErrorException(final String error, final Term culprit) {
    super(culprit);
    this.error = error;
  }

  public String getError() {
    return this.error;
  }


  @Override
  public Term getErrorTerm() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    return this.makeErrorStruct(TERM_ERROR, TermList.asTermList(newAtom(this.error), this.getCulprit()));
  }
}
