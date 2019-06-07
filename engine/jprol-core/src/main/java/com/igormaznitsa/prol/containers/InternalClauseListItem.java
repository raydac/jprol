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

package com.igormaznitsa.prol.containers;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import lombok.Data;

@Data
public final class InternalClauseListItem {

  private final TermStruct clause;
  private final Term keyTerm;
  private final boolean fact;
  private InternalClauseListItem previous;
  private InternalClauseListItem next;

  protected InternalClauseListItem(final InternalClauseListItem prev, final InternalClauseListItem next, final TermStruct clause) {
    super();
    previous = prev;
    if (prev != null) {
      prev.setNext(this);
    }
    this.next = next;
    if (next != null) {
      next.setPrevious(this);
    }
    this.clause = clause;
    if (clause.isFunctorLikeRuleDefinition()) {
      fact = false;
      keyTerm = clause.getElement(0);
    } else {
      fact = true;
      keyTerm = clause;
    }
  }


  public final void remove() {
    if (previous != null) {
      previous.setNext(this.getNext());
    }
    if (next != null) {
      next.setPrevious(this.getPrevious());
    }
  }

  @Override
  public String toString() {
    return keyTerm.toString();
  }
}
