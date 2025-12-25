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
import static com.igormaznitsa.jprol.data.Terms.newStruct;

import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.utils.lazy.LazyMap;

public abstract class ProlAbstractCatchableException extends ProlException {

  static final Term UNDEFINED = newAtom("<undefined>");
  private static final String ERROR_FUNCTOR = "error";
  private final Term culprit;
  private final SourcePosition sourcePosition;

  public ProlAbstractCatchableException(final Term culprit) {
    this(culprit, culprit.getSourcePosition());
  }

  public ProlAbstractCatchableException(final Term culprit, final SourcePosition sourcePosition) {
    this(null, culprit, null, sourcePosition);
  }

  public ProlAbstractCatchableException(final String message, final Term culprit) {
    this(message, culprit, null, null);
  }

  public ProlAbstractCatchableException(
      final String message,
      final Term culprit,
      final Throwable cause,
      final SourcePosition sourcePosition
  ) {
    super(message, cause);
    this.culprit =
        culprit == null ? UNDEFINED : culprit.cloneAndReplaceVariableByValue(new LazyMap<>());
    if (sourcePosition == null) {
      this.sourcePosition = this.culprit.getSourcePosition();
    } else {
      this.sourcePosition = sourcePosition;
    }
  }

  public ProlAbstractCatchableException(final Term culprit, final Throwable cause) {
    this(null, culprit, cause, null);
  }

  public SourcePosition getSourcePosition() {
    return this.sourcePosition;
  }

  public abstract Term getErrorTerm();

  public Term getCulprit() {
    return this.culprit.makeClone();
  }

  protected TermStruct makeErrorStruct(final Term formal, final Term context) {
    return newStruct(ERROR_FUNCTOR, new Term[] {formal, context}, SourcePosition.UNKNOWN);
  }

  public abstract TermStruct getAsStruct();

  @Override
  public String toString() {
    return this.getClass().getSimpleName() + '{'
        + "culprit=" + this.culprit
        + ", sourcePosition=" + this.sourcePosition
        + (this.getMessage() == null ? "" : ", message=\"" + this.getMessage() + '\"') + '}';
  }
}
