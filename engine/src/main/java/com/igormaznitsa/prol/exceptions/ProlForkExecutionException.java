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
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.TermStruct;
import java.util.Arrays;

/**
 * The class describes a fork exception. It will be thrown during fork/1
 * predicate execution, if any thread throws an exception.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlForkExecutionException extends ProlAbstractCatcheableException {
  private static final long serialVersionUID = 3401072948341099491L;

  /**
   * The constant contains the shared term value between all instances of the
   * class
   */
  private static final Term ERROR_TERM = new Term("fork_error");

  /**
   * The variable contains throwables getted during fork execution
   */
  private final Throwable[] throwables;

  /**
   * Get the throwables getted during the fork execution
   *
   * @return array of throwables, will not be null in any case but can be empty
   */
  public Throwable[] getThrowables() {
    return throwables;
  }

  /**
   * A constructor
   *
   * @param culprit the culprit term
   * @param causes the array of throwables getted furing fork execution
   */
  public ProlForkExecutionException(final Term culprit, final Throwable[] causes) {
    this("Error during a fork thread " + (causes != null ? Arrays.toString(causes) : "[]") + '\'', culprit, causes);
  }

  /**
   * A constructor
   *
   * @param message a text message describes the situation
   * @param culprit the culprit term
   * @param causes the array of throwables getted furing fork execution
   */
  public ProlForkExecutionException(final String message, final Term culprit, final Throwable[] causes) {
    super(message, culprit);
    this.throwables = causes == null ? new Throwable[0] : causes;
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return ERROR_TERM;
  }

  @Override
  public TermStruct getAsStruct() {
    final TermStruct result = new TermStruct(ERROR_TERM, new Term[]{new TermInteger(throwables.length)});
    result.setCarriedObject(this);
    return result;
  }

  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder(super.toString());
    builder.append('[');
    for (int li = 0; li < throwables.length; li++) {
      if (li > 0) {
        builder.append(',');
      }
      builder.append(builder);
    }
    builder.append(']');
    return builder.toString();
  }

}
