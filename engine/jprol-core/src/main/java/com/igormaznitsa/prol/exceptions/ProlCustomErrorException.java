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

public class ProlCustomErrorException extends ProlAbstractCatcheableException {

  private static final long serialVersionUID = -4720280738591345468L;

  private final Term error;

  public ProlCustomErrorException(final Term error, final Term culprit) {
    super(culprit);
    if (error == null) {
      throw new IllegalArgumentException("Error must not be null");
    }
    this.error = error;
  }

  @Override
  public Term getErrorTerm() {
    return this.error;
  }

  @Override
  public TermStruct getAsStruct() {
    return new TermStruct("error", new Term[] {this.error, this.getCulprit() == null ? UNDEFINED : this.getCulprit()});
  }

}
