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

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;

import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newStruct;

public abstract class ProlAbstractCatcheableException extends ProlException {

  static final Term UNDEFINED = newAtom("<undefined>");
  private static final String ERROR_FUNCTOR = "error";
  private static final long serialVersionUID = 6911111912695145529L;
  private final Term culprit;

  public ProlAbstractCatcheableException(final Term culprit) {
    this.culprit = culprit;
  }

  public ProlAbstractCatcheableException(final String message, final Term culprit) {
    super(message);
    this.culprit = culprit == null ? UNDEFINED : culprit.makeCloneAndVarBound();
  }

  public ProlAbstractCatcheableException(final String message, final Term culprit, final Throwable cause) {
    super(message, cause);
    this.culprit = culprit == null ? UNDEFINED : culprit.makeCloneAndVarBound();
  }

  public ProlAbstractCatcheableException(final Term culprit, final Throwable cause) {
    super(cause);
    this.culprit = culprit == null ? UNDEFINED : culprit.makeCloneAndVarBound();
  }

  public abstract Term getErrorTerm();

  public Term getCulprit() {
    return this.culprit.makeClone();
  }

  protected TermStruct makeErrorStruct(final Term formal, final Term context) {
    return newStruct(ERROR_FUNCTOR, new Term[] {formal, context});
  }

  public abstract TermStruct getAsStruct();
}
