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

import com.igormaznitsa.prol.data.TermStruct;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

class MemoryClauseIterator implements ClauseIterator {

  private final Iterator<KnowledgeBaseItem> iterator;
  private final TermStruct search;
  private final ClauseIteratorType type;
  private KnowledgeBaseItem next;

  MemoryClauseIterator(
      final ClauseIteratorType type,
      final List<KnowledgeBaseItem> list,
      final TermStruct search
  ) {
    this.type = type;
    this.iterator = list.iterator();
    this.search = (TermStruct) search.makeClone();
    this.next = findNext();
  }

  private static boolean isFact(final KnowledgeBaseItem item) {
    return !item.isRightPartPresented() && item.getKeyTerm().isGround();
  }

  private static boolean isRule(final KnowledgeBaseItem item) {
    return item.isRightPartPresented() || !item.getKeyTerm().isGround();
  }

  @Override
  public ClauseIteratorType getType() {
    return this.type;
  }

  @Override
  public boolean hasNext() {
    return this.next != null;
  }

  public TermStruct getTemplate() {
    return this.search;
  }

  private KnowledgeBaseItem findNext() {

    KnowledgeBaseItem nextItem = null;

    while (this.iterator.hasNext() && nextItem == null) {
      switch (this.type) {
        case ANY: {
          final KnowledgeBaseItem nextKb = this.iterator.next();
          if (nextKb.getKeyTerm().dryUnifyTo(this.search)) {
            if (this.search.makeClone().unifyTo(nextKb.getKeyTerm().makeClone())) {
              nextItem = nextKb;
            }
          }
        }
        break;
        case FACTS: {
          final KnowledgeBaseItem nextKb = this.iterator.next();
          if (isFact(nextKb) && nextKb.getKeyTerm().dryUnifyTo(this.search)) {
            if (this.search.makeClone().unifyTo(nextKb.getKeyTerm().makeClone())) {
              nextItem = nextKb;
            }
          }
        }
        break;
        case RULES: {
          final KnowledgeBaseItem nextKb = this.iterator.next();
          if (isRule(nextKb) && nextKb.getKeyTerm().dryUnifyTo(this.search)) {
            if (this.search.makeClone().unifyTo(nextKb.getKeyTerm().makeClone())) {
              nextItem = nextKb;
            }
          }
        }
        break;
        default:
          throw new Error("Unexpected type: " + this.type);
      }
    }
    return nextItem;
  }

  public KnowledgeBaseItem nextItem() {
    if (this.next == null) {
      throw new NoSuchElementException();
    }
    final KnowledgeBaseItem result = this.next;
    this.next = findNext();
    return result;
  }

  @Override
  public TermStruct next() {
    final KnowledgeBaseItem item = this.nextItem();
    return (TermStruct) item.getClause().makeClone();
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException("Removing not supported for clause iterator");
  }

  @Override
  public void cut() {
    this.next = null;
  }

  @Override
  public String toString() {
    return "ClauseIterator{template=" + this.search + '}';
  }
}
