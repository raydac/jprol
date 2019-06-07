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
  protected final TermStruct template;
  protected InternalClauseListItem lastFound;

  public MemoryClauseIterator(final InternalKnowledgeBaseClauseList list, final TermStruct template) {
    predicateList = list;
    this.template = (TermStruct) template.makeClone();
    lastFound = findFirstElement();
  }

  @Override
  public boolean hasNext() {
    return lastFound != null;
  }

  public TermStruct getTemplate() {
    return template;
  }

  protected InternalClauseListItem findFirstElement() {
    return predicateList.findDirect(template, null);
  }

  @Override
  public TermStruct next() {
    if (lastFound == null) {
      throw new NoSuchElementException();
    }

    final TermStruct result = (TermStruct) lastFound.getClause().makeClone();
    lastFound = predicateList.findDirect(template, lastFound);

    return result;
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException("Not supported.");
  }

  @Override
  public void cut() {
    lastFound = null;
  }

  @Override
  public String toString() {
    return "ClauseIterator{template=" + template + '}';
  }
}
