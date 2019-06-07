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

public class ProlDomainErrorException extends ProlAbstractCatcheableException {

  private static final long serialVersionUID = -7481773982226534683L;

  private static final Term ERROR_TERM = new Term("domain_error");

  private final String validDomain;

  public ProlDomainErrorException(final String validDomain, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.validDomain = validDomain;
  }

  public ProlDomainErrorException(final String validDomain, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.validDomain = validDomain;
  }

  public ProlDomainErrorException(final String validDomain, final String message, final Term culprit) {
    super(message, culprit);
    this.validDomain = validDomain;
  }

  public ProlDomainErrorException(final String validDomain, final Term culprit) {
    super(culprit);
    this.validDomain = validDomain;
  }

  public String getValidDomain() {
    return this.validDomain;
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return ERROR_TERM;
  }

  @Override
  public TermStruct getAsStruct() {
    return new TermStruct(ERROR_TERM, new Term[] {validDomain == null ? UNDEFINED : new Term(validDomain), getCulprit() == null ? UNDEFINED : getCulprit()});
  }
}
