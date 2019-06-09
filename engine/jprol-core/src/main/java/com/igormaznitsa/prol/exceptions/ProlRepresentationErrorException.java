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

public class ProlRepresentationErrorException extends ProlAbstractCatcheableException {

  private static final long serialVersionUID = -4337637438675029939L;

  private static final Term TERM_ERROR = new Term("representation_error");
  private final String flag;

  public ProlRepresentationErrorException(final String flag, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.flag = flag;
  }

  public ProlRepresentationErrorException(final String flag, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.flag = flag;
  }

  public ProlRepresentationErrorException(final String flag, final String message, final Term culprit) {
    super(message, culprit);
    this.flag = flag;
  }

  public ProlRepresentationErrorException(final String flag, final Term culprit) {
    super(culprit);
    this.flag = flag;
  }

  public String getFlag() {
    return flag;
  }


  @Override
  public Term getErrorTerm() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    return new TermStruct("error", new Term[] {TERM_ERROR, this.flag == null ? UNDEFINED : new Term(this.flag)});
  }
}
