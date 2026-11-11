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

package com.igormaznitsa.jprol.kbase.inmemory;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.kbase.IteratorType;
import com.igormaznitsa.jprol.kbase.inmemory.items.InMemoryItem;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

public final class InMemoryClauseIterator implements CloseableIterator<TermStruct> {

  private final Iterator<InMemoryItem> iterator;
  private final Term search;
  private final IteratorType type;
  private InMemoryItem next;

  public InMemoryClauseIterator(
      final IteratorType type,
      final List<InMemoryItem> list,
      final TermStruct search
  ) {
    this(type, list.iterator(), search);
  }

  public InMemoryClauseIterator(
      final IteratorType type,
      final List<InMemoryItem> list,
      final Term search
  ) {
    this(type, list.iterator(), search);
  }

  InMemoryClauseIterator(
      final IteratorType type,
      final Iterator<InMemoryItem> items,
      final Term search
  ) {
    this.search = search;
    this.type = type;
    this.iterator = items;
    this.next = this.findNext();
  }

  @Override
  public boolean hasNext() {
    return this.next != null;
  }

  public Term getTemplate() {
    return this.search;
  }

  @Override
  public void close() {

  }

  private InMemoryItem findNext() {

    InMemoryItem result = null;

    while (this.iterator.hasNext() && result == null) {
      final InMemoryItem nextItem = this.iterator.next();
      if (!this.accepts(nextItem)
          || FirstArgIndex.incompatibleFirstArg(nextItem.getHead(), this.search)
          || !nextItem.matches(this.search)) {
        continue;
      }
      result = nextItem;
    }
    return result;
  }

  private boolean accepts(final InMemoryItem item) {
    switch (this.type) {
      case ANY:
        return true;
      case FACTS:
        return item.isFact();
      case RULES:
        return item.isRule();
      default:
        throw new Error("Unexpected type: " + this.type);
    }
  }

  InMemoryItem nextItem() {
    if (this.next == null) {
      throw new NoSuchElementException();
    }
    final InMemoryItem result = this.next;
    this.next = this.findNext();
    return result;
  }

  @Override
  public TermStruct next() {
    final InMemoryItem item = this.nextItem();
    final TermStruct clause = item.getClause();
    return item.isFact() ? clause : (TermStruct) clause.makeClone();
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException("Removing not supported for clause iterator");
  }

  @Override
  public String toString() {
    return InMemoryClauseIterator.class.getSimpleName() + "{template=" + this.search + '}';
  }
}
