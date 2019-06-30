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

package com.igormaznitsa.prol.kbase.inmemory;

import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.kbase.ClauseIterator;
import com.igormaznitsa.prol.kbase.ClauseIteratorType;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

public class InMemoryClauseIterator implements ClauseIterator {

  private final Iterator<InMemoryItem> iterator;
  private final TermStruct search;
  private final ClauseIteratorType type;
  private InMemoryItem next;

  InMemoryClauseIterator(
      final ClauseIteratorType type,
      final List<InMemoryItem> list,
      final TermStruct search
  ) {
    this.type = type;
    this.iterator = list.iterator();
    this.search = (TermStruct) search.makeClone();
    this.next = findNext();
  }

  private static boolean isFact(final InMemoryItem item) {
    return !item.isRightPartPresented() && item.getKeyTerm().isGround();
  }

  private static boolean isRule(final InMemoryItem item) {
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

  private boolean canBeUnifiedWithPattern(final InMemoryItem item) {
    if (item.isKeyContainsLinkedVars()) {
      return this.search.makeClone().unifyTo(item.getKeyTerm().makeClone());
    } else {
      return this.search.dryUnifyTo(item.getKeyTerm());
    }
  }

  private InMemoryItem findNext() {

    InMemoryItem nextItem = null;

    while (this.iterator.hasNext() && nextItem == null) {
      final InMemoryItem nextKb = this.iterator.next();
      switch (this.type) {
        case ANY: {
          if (canBeUnifiedWithPattern(nextKb)) {
            nextItem = nextKb;
          }
        }
        break;
        case FACTS: {
          if (isFact(nextKb) && this.canBeUnifiedWithPattern(nextKb)) {
            nextItem = nextKb;
          }
        }
        break;
        case RULES: {
          if (isRule(nextKb) && this.canBeUnifiedWithPattern(nextKb)) {
            nextItem = nextKb;
          }
        }
        break;
        default:
          throw new Error("Unexpected type: " + this.type);
      }
    }
    return nextItem;
  }

  public InMemoryItem nextItem() {
    if (this.next == null) {
      throw new NoSuchElementException();
    }
    final InMemoryItem result = this.next;
    this.next = findNext();
    return result;
  }

  @Override
  public TermStruct next() {
    final InMemoryItem item = this.nextItem();
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
