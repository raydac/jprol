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

public abstract class ProlAbstractCatcheableException extends ProlException {

  public static final Term UNDEFINED = new Term("<undefined>");
  private static final long serialVersionUID = 6911111912695145529L;
  private final Term culprit;

  public ProlAbstractCatcheableException(final Term culprit) {
    this.culprit = culprit;
  }

  public ProlAbstractCatcheableException(final String message, final Term culprit) {
    super(message);
    this.culprit = culprit;
  }

  public ProlAbstractCatcheableException(final String message, final Term culprit, final Throwable cause) {
    super(message, cause);
    this.culprit = culprit;
  }

  public ProlAbstractCatcheableException(final Term culprit, final Throwable cause) {
    super(cause);
    this.culprit = culprit;
  }

  public Term getCulprit() {
    return culprit;
  }

  public abstract Term getFunctorForErrorStruct();

  public abstract TermStruct getAsStruct();
}
