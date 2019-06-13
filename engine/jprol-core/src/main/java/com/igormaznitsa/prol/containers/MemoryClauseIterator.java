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

import java.util.NoSuchElementException;

class MemoryClauseIterator implements ClauseIterator {

  protected final InternalKnowledgeBaseClauseList predicateList;
  protected final TermStruct searchTemplate;
  protected final ClauseIteratorType type;
  protected InternalClauseListItem foundNext;

  public MemoryClauseIterator(
      final ClauseIteratorType type,
      final InternalKnowledgeBaseClauseList list,
      final TermStruct searchTemplate
  ) {
    this.type = type;
    this.predicateList = list;
    this.searchTemplate = (TermStruct) searchTemplate.makeClone();
    this.foundNext = lookForNext(null);
  }

  private static boolean isFact(final InternalClauseListItem item) {
    return !item.isRightPartPresented() && item.getKeyTerm().isGround();
  }

  private static boolean isRule(final InternalClauseListItem item) {
    return item.isRightPartPresented() || !item.getKeyTerm().isGround();
  }

  @Override
  public ClauseIteratorType getType() {
    return this.type;
  }

  @Override
  public boolean hasNext() {
    return this.foundNext != null;
  }

  public TermStruct getTemplate() {
    return this.searchTemplate;
  }

  protected InternalClauseListItem lookForNext(final InternalClauseListItem startPosition) {
    InternalClauseListItem result = startPosition == null ? null : startPosition;

    while (!Thread.currentThread().isInterrupted()) {
      result = this.predicateList.searchForward(this.searchTemplate, result);
      if (result == null) {
        break;
      } else {
        switch (this.type) {
          case ANY:
            break;
          case FACTS: {
            if (!isFact(result)) {
              result = null;
            }
          }
          break;
          case RULES: {
            if (!isRule(result)) {
              result = null;
            }
          }
          break;
          default:
            throw new Error("Unexpected type: " + this.type);
        }
      }
      if (result != null) {
        break;
      }
    }
    return result;
  }

  @Override
  public TermStruct next() {
    if (this.foundNext == null) {
      throw new NoSuchElementException();
    }
    final TermStruct result = (TermStruct) foundNext.getClause().makeClone();
    this.foundNext = lookForNext(this.foundNext);
    return result;
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException("Removing not supported for clause iterator");
  }

  @Override
  public void cut() {
    this.foundNext = null;
  }

  @Override
  public String toString() {
    return "ClauseIterator{template=" + this.searchTemplate + '}';
  }
}
