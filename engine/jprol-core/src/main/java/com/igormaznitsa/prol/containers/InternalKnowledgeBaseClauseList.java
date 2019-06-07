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
import java.util.ArrayList;
import java.util.List;

final class InternalKnowledgeBaseClauseList {

  private InternalClauseListItem first;
  private InternalClauseListItem last;

  InternalKnowledgeBaseClauseList() {
    super();
    first = null;
    last = null;
  }

  boolean asserta(final TermStruct struct) {
    first = new InternalClauseListItem(null, first, struct);
    if (last == null) {
      last = first;
    }
    return true;
  }

  boolean assertz(final TermStruct struct) {
    last = new InternalClauseListItem(last, null, struct);
    if (first == null) {
      first = last;
    }
    return true;
  }

  boolean retracta(final TermStruct template) {
    InternalClauseListItem container = null;

    boolean result = false;

    while (!Thread.currentThread().isInterrupted()) {
      if ((container = findDirect(template, container)) == null) {
        break;
      }
      container.remove();
      if (first == container) {
        first = container.getNext();
      }
      result = true;
      break;
    }

    return result;
  }

  boolean retractz(final TermStruct template) {
    InternalClauseListItem container = null;

    while (!Thread.currentThread().isInterrupted()) {
      if ((container = findBack(template, container)) == null) {
        break;
      }
      container.remove();
      if (last == container) {
        last = container.getPrevious();
      }

      return true;
    }
    return false;
  }

  int retractall(final TermStruct template) {
    InternalClauseListItem container = null;
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

  int size() {
    int size = 0;

    if (first == null) {
      return 0;
    }

    InternalClauseListItem curContainer = first;
    while (curContainer != null) {
      curContainer = curContainer.getNext();
      size++;
    }
    return size;
  }

  public List<TermStruct> asList() {
    final List<TermStruct> result = new ArrayList<>();

    InternalClauseListItem current = this.first;
    while (current != null) {
      result.add(current.getClause());
      current = current.getNext();
    }

    return result;
  }

  InternalClauseListItem findDirect(final TermStruct template, InternalClauseListItem sinceContainer) {

    if (sinceContainer == null) {
      sinceContainer = first;
    } else {
      sinceContainer = sinceContainer.getNext();
    }

    InternalClauseListItem result = null;

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

  InternalClauseListItem findBack(final TermStruct template, InternalClauseListItem sinceContainer) {
    if (sinceContainer == null) {
      sinceContainer = last;
    } else {
      sinceContainer = sinceContainer.getPrevious();
    }

    InternalClauseListItem result = null;

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

  void write(final PrintWriter writer) {
    if (writer == null) {
      throw new NullPointerException("Writer is null");
    }
    InternalClauseListItem item = first;
    while (!Thread.currentThread().isInterrupted()) {
      if (item == null) {
        break;
      }
      writer.write(String.format("%s.%n", item.getClause().getSourceLikeRepresentation()));
      item = item.getNext();
    }
    writer.println();
  }

  public InternalKnowledgeBaseClauseList makeCopy() {
    final InternalKnowledgeBaseClauseList copy = new InternalKnowledgeBaseClauseList();

    InternalClauseListItem current = first;

    while (!Thread.currentThread().isInterrupted()) {
      if (current == null) {
        break;
      } else {
        copy.assertz(current.getClause());
        current = current.getNext();
      }
    }

    return copy;
  }
}
