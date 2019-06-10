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
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.logic.ProlContext;

import java.io.PrintWriter;
import java.util.Iterator;
import java.util.List;

public interface KnowledgeBase {

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

  ClauseIterator getClauseIterator(TermStruct template);

  List<TermStruct> findAllForPredicateIndicator(final Term predicateIndicator);

  List<TermStruct> findAllForSignature(final String signature);

  boolean assertZ(TermStruct clause);

  boolean assertA(TermStruct clause);

  boolean retractAll(TermStruct clause);

  boolean retractA(TermStruct clause);

  boolean retractZ(final TermStruct clause);

  void abolish(final String signature);

  Iterator<OperatorContainer> getOperatorIterator();

  KnowledgeBase makeCopy(ProlContext context);
}
