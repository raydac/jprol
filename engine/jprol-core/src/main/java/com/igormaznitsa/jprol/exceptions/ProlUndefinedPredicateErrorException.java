/*
 * Copyright 2019 Igor Maznitsa (http://www.igormaznitsa.com).
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

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;

import static com.igormaznitsa.jprol.data.Terms.newAtom;

public class ProlUndefinedPredicateErrorException extends ProlAbstractCatcheableException {

  private static final long serialVersionUID = -748177398222600683L;

  private static final Term ERROR_TERM = newAtom("undefined_predicate");

  public ProlUndefinedPredicateErrorException(final String signature) {
    super(Terms.newAtom(signature));
  }

  @Override
  public Term getErrorTerm() {
    return ERROR_TERM;
  }

  @Override
  public TermStruct getAsStruct() {
    return this.makeErrorStruct(ERROR_TERM, TermList.asTermList(ERROR_TERM, this.getCulprit()));
  }

  @Override
  public String getMessage() {
    return "Undefined predicate: " + this.getCulprit().getText();
  }
}
