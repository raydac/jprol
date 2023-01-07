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

package com.igormaznitsa.jprol.exceptions;

import static com.igormaznitsa.jprol.data.Terms.newAtom;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;

public class ProlRepresentationErrorException extends ProlAbstractCatchableException {

  private static final long serialVersionUID = -4337637438675029939L;

  private static final Term TERM_ERROR = newAtom("representation_error");
  private final String errorCode;

  public ProlRepresentationErrorException(final String errorCode, final Term culprit,
                                          final Throwable cause) {
    super(culprit, cause);
    this.errorCode = errorCode == null ? UNDEFINED.getText() : errorCode;
  }

  public ProlRepresentationErrorException(final String errorCode, final String message,
                                          final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.errorCode = errorCode == null ? UNDEFINED.getText() : errorCode;
  }

  public ProlRepresentationErrorException(final String errorCode, final String message,
                                          final Term culprit) {
    super(message, culprit);
    this.errorCode = errorCode == null ? UNDEFINED.getText() : errorCode;
  }

  public ProlRepresentationErrorException(final String flag, final Term culprit) {
    super(culprit);
    this.errorCode = flag == null ? UNDEFINED.getText() : flag;
  }

  public String getErrorCode() {
    return errorCode;
  }

  @Override
  public Term getErrorTerm() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    return this.makeErrorStruct(TERM_ERROR,
        TermList.asTermList(newAtom(this.errorCode), this.getCulprit()));
  }
}
