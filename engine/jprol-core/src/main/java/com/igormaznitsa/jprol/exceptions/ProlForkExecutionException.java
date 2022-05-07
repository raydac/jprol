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
import static com.igormaznitsa.jprol.data.Terms.newLong;


import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import java.util.Arrays;

public class ProlForkExecutionException extends ProAbstractCatchableException {

  private static final long serialVersionUID = 3401072948341099491L;

  private static final Term ERROR_TERM = newAtom("fork_error");

  private final Throwable[] throwableArray;

  public ProlForkExecutionException(final Term culprit, final Throwable[] causes) {
    this("Error during a fork thread " + (causes != null ? Arrays.toString(causes) : "[]") + '\'',
        culprit, causes);
  }

  public ProlForkExecutionException(final String message, final Term culprit,
                                    final Throwable[] causes) {
    super(message, culprit);
    this.throwableArray = causes == null ? new Throwable[0] : causes;
  }

  public Throwable[] getThrowableArray() {
    return this.throwableArray.clone();
  }

  @Override
  public TermStruct getAsStruct() {
    return makeErrorStruct(ERROR_TERM, newLong(throwableArray.length));
  }

  @Override
  public Term getErrorTerm() {
    return ERROR_TERM;
  }

  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder(super.toString());
    builder.append('[');
    for (int li = 0; li < throwableArray.length; li++) {
      if (li > 0) {
        builder.append(',');
      }
      builder.append(builder);
    }
    builder.append(']');
    return builder.toString();
  }

}
