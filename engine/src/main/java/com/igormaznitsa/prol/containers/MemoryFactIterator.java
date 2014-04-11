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

/**
 * This class implements an iterator allows search only facts in a knowledge
 * base
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
final class MemoryFactIterator extends MemoryClauseIterator implements FactIterator {

  /**
   * The constructor
   *
   * @param list the inside knowledge base list to be used by the iterator, must
   * not be null
   * @param template the template which will be used to find facts, must not be
   * null
   */
  public MemoryFactIterator(final KnowledgeBaseInsideClauseList list, final TermStruct template) {
    super(list, template);
  }

  @Override
  protected InsideClauseListItem findFirstElement() {
    InsideClauseListItem firstitem = null;

    while (true) {
      firstitem = predicateList.findDirect(template, firstitem);
      if (firstitem == null || firstitem.isFact()) {
        break;
      }
    }

    return firstitem;
  }

  @Override
  public TermStruct next() {
    if (lastFound == null) {
      throw new NoSuchElementException();
    }

    final TermStruct result = (TermStruct) lastFound.getClause().makeClone();

    while (true) {
      lastFound = predicateList.findDirect(template, lastFound);
      if (lastFound == null || lastFound.isFact()) {
        break;
      }
    }

    return result;
  }
}
