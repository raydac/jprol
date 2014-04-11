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

/**
 * The exception is a catcheable exception and it is mainly used to describe a
 * non standard exceptions
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlCustomErrorException extends ProlAbstractCatcheableException {
  private static final long serialVersionUID = -4720280738591345468L;
  
  /**
   * The variable contains the term describes the error situation
   */
  private final Term error;

  /**
   * A constructor
   *
   * @param error the term describes the error situation, can be any type, must
   * not be null
   * @param culprit the culprit term
   */
  public ProlCustomErrorException(final Term error, final Term culprit) {
    super(culprit);
    if (error == null) {
      throw new IllegalArgumentException("Error must not be null");
    }
    this.error = error;
  }

  /**
   * Get the term describes the error situation
   *
   * @return the term as a Term object, must not be null
   */
  public Term getError() {
    return this.error;
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return getAsStruct().getFunctor();
  }

  @Override
  public TermStruct getAsStruct() {
    TermStruct result = null;
    if (error.getTermType() == Term.TYPE_STRUCT) {
      result = (TermStruct) error;
    }
    else {
      result = new TermStruct(error);
    }
    result.setCarriedObject(this);
    return result;
  }

}
