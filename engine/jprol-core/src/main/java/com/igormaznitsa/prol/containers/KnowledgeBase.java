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

import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.logic.ProlContext;

import java.io.PrintWriter;
import java.util.Iterator;
import java.util.List;

/**
 * The interface describes a prol knowledge base which can be used by a
 * ProlContext
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see ProlContext
 */
public interface KnowledgeBase {

  /**
   * To get the knowledge base identifier
   *
   * @return the identifier as String
   */
  String getVocabularyId();

  void addOperators(Operator[] operators);

  Operator getOperatorForTypeAndName(String name, int type);

  boolean removeOperator(String name, int type);

  void addOperator(Operator operator);

  OperatorContainer findOperatorForName(String name);

  boolean hasOperatorStartsWith(String str);

  void write(PrintWriter writer);

  FactIterator getFactIterator(TermStruct template);

  RuleIterator getRuleIterator(TermStruct template);

  /**
   * Get a clause iterator for a template
   *
   * @param template the template to find clauses in the base (must not be null)
   * @return a ClauseIterator object if the template is found in the base, else
   * null
   * @see ClauseIterator
   */
  ClauseIterator getClauseIterator(TermStruct template);

  List<TermStruct> findAllForSignature(final String signature);

  boolean assertZ(TermStruct clause);

  boolean assertA(TermStruct clause);

  boolean retractAll(TermStruct clause);

  boolean retractA(TermStruct clause);

  boolean retractZ(final TermStruct clause);

  void abolish(final String signature);

  /**
   * To get the iterator of all defined operators at the base
   *
   * @return the iterator of the inside operator table
   */
  Iterator<OperatorContainer> getOperatorIterator();

  /**
   * Make copy of the knowledge base, it means that there will be new knowledge
   * base object contains the snapshot of the current knowledge base state. It
   * will copy only structure but new knowledge base will have the same term and
   * rule objects as the etalon
   *
   * @param context the context which will use the copied knowledge base, must
   * not be null
   * @return new knowledge base instance as the snapshot
   */
  KnowledgeBase makeCopy(ProlContext context);
}
