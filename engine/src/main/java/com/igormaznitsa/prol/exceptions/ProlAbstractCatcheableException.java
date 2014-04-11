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
 * The class describes a Prol Exception which can be catched with the catch/3
 * predicate and it can be throws with the throw/1 predicate. It's an abstract
 * class so it can't have any direct instance.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.exceptions.ProlException
 */
public abstract class ProlAbstractCatcheableException extends ProlException {
  private static final long serialVersionUID = 6911111912695145529L;

  /**
   * The term should be used for any undefined parameter of an exception
   */
  public static final Term UNDEFINED = new Term("<undefined>");

  /**
   * The term which is the culprit for the exception
   */
  private final Term culprit;

  /**
   * Get the term culprit of the exception
   *
   * @return the term is being the culprit for the exception, it can be null
   */
  public Term getCulprit() {
    return culprit;
  }

  /**
   * A constructor
   *
   * @param culprit the term culprit for the exception
   */
  public ProlAbstractCatcheableException(final Term culprit) {
    this.culprit = culprit;
  }

  /**
   * A constructor
   *
   * @param message The message describes the cause of the exception
   * @param culprit the term culprit for the exception
   */
  public ProlAbstractCatcheableException(final String message, final Term culprit) {
    super(message);
    this.culprit = culprit;
  }

  /**
   * A constructor
   *
   * @param message The message describes the cause of the exception
   * @param culprit the term culprit for the exception
   * @param cause the cause exception
   */
  public ProlAbstractCatcheableException(final String message, final Term culprit, final Throwable cause) {
    super(message, cause);
    this.culprit = culprit;
  }

  /**
   * A constructor
   *
   * @param culprit the term culprit for the exception
   * @param cause the cause exception
   */
  public ProlAbstractCatcheableException(final Term culprit, final Throwable cause) {
    super(cause);
    this.culprit = culprit;
  }

  /**
   * Get the functor of the error structure describes the exception in a prol
   * engine
   *
   * @return the functor as a Term, must not be null
   */
  public abstract Term getFunctorForErrorStruct();

  /**
   * Get the structure describes the exception in a prol engine
   *
   * @return the structure as a TermStruct object, must not be null
   */
  public abstract TermStruct getAsStruct();
}
