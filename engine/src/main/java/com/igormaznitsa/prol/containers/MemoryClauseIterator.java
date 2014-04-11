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
 * The class describes an iterator which allows to take sequently structures at
 * a knowledge base clause list and compare them with the template
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitrsa.com)
 * @see com.igormaznitsa.prol.containers.KnowledgeBaseInsideClauseList
 * @see com.igormaznitsa.prol.data.TermStruct
 */
class MemoryClauseIterator implements ClauseIterator {

  /**
   * The variable contains the knowledge base clause list which is being used by
   * the iterator
   */
  protected final KnowledgeBaseInsideClauseList predicateList;

  /**
   * The variable contains the template which is being used by the iterator to
   * find the next value
   */
  protected final TermStruct template;

  /**
   * The variable contains the last found value by the iterator
   */
  protected InsideClauseListItem lastFound;

  /**
   * The constructor
   *
   * @param list the knowledge base clause list for which we want to create the
   * iterator, must not be null
   * @param template the template to be used to find values in the clause list,
   * must not be null
   */
  public MemoryClauseIterator(final KnowledgeBaseInsideClauseList list, final TermStruct template) {
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

  /**
   * Inside function to find the first element for the iterator, it is called
   * from the constructor
   *
   * @return the first clause list item or null if it is not found
   */
  protected InsideClauseListItem findFirstElement() {
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

  /**
   * End the work of the iterator, null will be returned in next request
   */
  @Override
  public void cut() {
    lastFound = null;
  }

  @Override
  public String toString() {
    return "ClauseIterator{template=" + template + '}';
  }
}
