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

public class ProlDomainErrorException extends ProlAbstractCatchableException {

  private static final Term ERROR_TERM = newAtom("domain_error");

  private final String validDomain;

  public ProlDomainErrorException(final String validDomain, final Term culprit,
                                  final Throwable cause) {
    super(culprit, cause);
    this.validDomain = validDomain == null ? UNDEFINED.getText() : validDomain;
  }

  public ProlDomainErrorException(final String validDomain, final String message,
                                  final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.validDomain = validDomain == null ? UNDEFINED.getText() : validDomain;
  }

  public ProlDomainErrorException(final String validDomain, final String message,
                                  final Term culprit) {
    super(message, culprit);
    this.validDomain = validDomain == null ? UNDEFINED.getText() : validDomain;
  }

  public ProlDomainErrorException(final String validDomain, final Term culprit) {
    super(culprit);
    this.validDomain = validDomain == null ? UNDEFINED.getText() : validDomain;
  }

  @Override
  public String getMessage() {
    return String.format("%s: detected = %s, expected = %s",
        (super.getMessage() == null ? "domain_error" : super.getMessage()),
        this.getCulprit().getText(), (this.validDomain == null ? "not defined" : this.validDomain));
  }

  public String getValidDomain() {
    return this.validDomain;
  }

  @Override
  public Term getErrorTerm() {
    return ERROR_TERM;
  }

  @Override
  public TermStruct getAsStruct() {
    return this.makeErrorStruct(ERROR_TERM,
        TermList.asList(TermList.asList(newAtom(this.validDomain), this.getCulprit())));
  }
}
