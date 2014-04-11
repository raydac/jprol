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

/**
 * The Inside class descibes a list item
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.containers.KnowledgeBaseInsideClauseList
 */
public final class InsideClauseListItem {

  /**
   * The link to the previous element, can be null
   */
  private InsideClauseListItem previous;
  /**
   * The link to the next element, can be null
   */
  private InsideClauseListItem next;
  /**
   * The clause which is saved by the element
   */
  private final TermStruct clause;
  /**
   * The key term for the saved clause
   */
  private final Term keyTerm;

  /**
   * This flag shows that clause is just a fact and doesn't have rule body
   */
  private final boolean fact;

  /**
   * To get the key term for saved clause
   *
   * @return the key term for the saved clause
   */
  public final Term getKeyTerm() {
    return keyTerm;
  }

  /**
   * To get the saved clause
   *
   * @return the saved clause
   */
  public final TermStruct getClause() {
    return clause;
  }

  /**
   * The constructor
   *
   * @param prev the previous item in the list, can be null
   * @param next the next item in the list, can be null
   * @param clause the clause which will be saved into the list item, must not
   * be null
   */
  protected InsideClauseListItem(final InsideClauseListItem prev, final InsideClauseListItem next, final TermStruct clause) {
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
    }
    else {
      fact = true;
      keyTerm = clause;
    }
  }

  /**
   * Returns the value shows that the list item contains a fact (not a rule)
   *
   * @return true if the list item contains a fact, else false
   */
  public final boolean isFact() {
    return fact;
  }

  /**
   * Get the previuys list item
   *
   * @return the previous list item if it exists else null
   */
  public final InsideClauseListItem getPrevious() {
    return previous;
  }

  /**
   * Set the previous list element
   *
   * @param value the previous list element, can be null
   */
  public final void setPrevious(final InsideClauseListItem value) {
    previous = value;
  }

  /**
   * Get the next list item
   *
   * @return the next list item if it exists else null
   */
  public final InsideClauseListItem getNext() {
    return next;
  }

  /**
   * Set the next item
   *
   * @param value the new next value, can be null
   */
  public final void setNext(final InsideClauseListItem value) {
    next = value;
  }

  /**
   * To remove the list element from the list
   */
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
