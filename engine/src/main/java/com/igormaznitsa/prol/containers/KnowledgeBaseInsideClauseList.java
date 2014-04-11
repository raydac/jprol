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
import java.io.PrintWriter;

/**
 * A special system class which allows to save clauses as a list
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
final class KnowledgeBaseInsideClauseList {

  /**
   * The link to the first list element, can be null (only if the last is null
   * too)
   */
  private InsideClauseListItem first;
  /**
   * The link to the last list element, can be null (only if the first is null
   * too)
   */
  private InsideClauseListItem last;

  /**
   * The constructor
   */
  KnowledgeBaseInsideClauseList() {
    super();
    first = null;
    last = null;
  }

  /**
   * To assert a clause as the A-Clause into the list
   *
   * @param struct the clause to be inserted into the list as the first element
   * (must not be null)
   * @return true if the operation is successfull else false
   */
  boolean asserta(final TermStruct struct) {
    first = new InsideClauseListItem(null, first, struct);
    if (last == null) {
      last = first;
    }
    return true;
  }

  /**
   * To assert a clause as the Z-Clause into the list
   *
   * @param struct the struct to be inserted into the list as the last element
   * (must not be null)
   * @return true if the operation is successfull else false
   */
  boolean assertz(final TermStruct struct) {
    last = new InsideClauseListItem(last, null, struct);
    if (first == null) {
      first = last;
    }
    return true;
  }

  /**
   * To retract the first found clause which compatible with a template
   *
   * @param template the template to find a clause, must not be null
   * @return true if the operation is successfull else false
   */
  boolean retracta(final TermStruct template) {
    InsideClauseListItem container = null;

    while ((container = findDirect(template, container)) != null) {
      container.remove();
      if (first == container) {
        first = container.getNext();
      }
      return true;
    }

    return false;
  }

  /**
   * To retract the last found clause which compatible with a template
   *
   * @param template the template to find a clause, must not be null
   * @return true if the operation is successfull else false
   */
  boolean retractz(final TermStruct template) {
    InsideClauseListItem container = null;

    while ((container = findBack(template, container)) != null) {
      container.remove();
      if (last == container) {
        last = container.getPrevious();
      }

      return true;
    }
    return false;
  }

  /**
   * To retract all found clauses which compatible with a template
   *
   * @param template the template to find clauses, must not be null
   * @return true if the operation is successfull else false
   */
  int retractall(final TermStruct template) {
    InsideClauseListItem container = null;
    int result = 0;

    while ((container = findDirect(template, container)) != null) {
      container.remove();
      if (first == container) {
        first = container.getNext();
      }
      if (last == container) {
        last = container.getPrevious();
      }
      result++;
    }
    return result;
  }

  /**
   * Calculate the size of the list
   *
   * @return the list size as integer
   */
  int size() {
    int size = 0;

    if (first == null) {
      return 0;
    }

    InsideClauseListItem curContainer = first;
    while (curContainer != null) {
      curContainer = curContainer.getNext();
      size++;
    }
    return size;
  }

  /**
   * Find next compatible clause with a template
   *
   * @param template the template to find a compatible clause
   * @param sinceContainer the item container which will be used as the first
   * for the search
   * @return the first found container contains a potentially compatible clause
   * or null if it has not been found
   */
  InsideClauseListItem findDirect(final TermStruct template, InsideClauseListItem sinceContainer) {

    if (sinceContainer == null) {
      sinceContainer = first;
    }
    else {
      sinceContainer = sinceContainer.getNext();
    }

    InsideClauseListItem result = null;

    while (sinceContainer != null) {

      final Term keyTerm = sinceContainer.getKeyTerm();

      if (keyTerm.equWithoutSet(template)) {
        // looks similar but we need to check a bit deeper to be ensure
        if (template.makeClone().Equ(keyTerm.makeClone())) {
          result = sinceContainer;
          break;
        }
      }
      sinceContainer = sinceContainer.getNext();

    }
    return result;
  }

  /**
   * Find next compatible clause with a template
   *
   * @param template the template to find a compatible clause
   * @param sinceContainer the item container which will be used as the first
   * for the search
   * @return the first found container contains a compatible clause or null if
   * it has not been found
   */
  InsideClauseListItem findBack(final TermStruct template, InsideClauseListItem sinceContainer) {
    if (sinceContainer == null) {
      sinceContainer = last;
    }
    else {
      sinceContainer = sinceContainer.getPrevious();
    }

    InsideClauseListItem result = null;

    while (sinceContainer != null) {
      final Term keyterm = sinceContainer.getKeyTerm();
      if (keyterm.equWithoutSet(template)) {
        // check deeper because equWithoutSet is not precise for speed
        if (keyterm.makeClone().Equ(template.makeClone())) {
          result = sinceContainer;
          break;
        }
      }
      sinceContainer = sinceContainer.getPrevious();
    }
    return result;
  }

  /**
   * Write current saved clauses as a prolog source into the writer
   *
   * @param writer the write which will be used to out clauses, must not be null
   */
  void write(final PrintWriter writer) {
    if (writer == null) {
      throw new NullPointerException("Writer is null");
    }
    InsideClauseListItem item = first;
    while (true) {
      if (item == null) {
        break;
      }
      writer.print(item.getClause().getSourceLikeRepresentation());
      writer.println('.');
      item = item.getNext();
    }
    writer.println();
  }

  /**
   * Make a copy of the list, it copies only structure.
   *
   * @return the copy of the clause list
   */
  public KnowledgeBaseInsideClauseList makeCopy() {
    final KnowledgeBaseInsideClauseList copy = new KnowledgeBaseInsideClauseList();

    InsideClauseListItem current = first;

    while (true) {
      if (current == null) {
        break;
      }
      else {
        copy.assertz(current.getClause());
        current = current.getNext();
      }
    }

    return copy;
  }
}
