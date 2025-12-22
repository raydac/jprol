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

package com.igormaznitsa.jprol.kbase;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermOperatorContainer;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

public interface KnowledgeBase {

  /**
   * Get text id of the knowledge base.
   *
   * @return the text id of the knowledge base, must not be null
   */
  String getId();

  boolean removeOperator(String name, OpAssoc type);

  void addOperator(JProlContext context, TermOperator operator);

  TermOperatorContainer findOperatorForName(JProlContext context, String name);

  boolean hasOperatorStartsWith(JProlContext context, String str);

  CloseableIterator<TermStruct> iterate(IteratorType type, TermStruct template,
                                        BiConsumer<String, Term> unknownSignatureConsumer);

  CloseableIterator<TermStruct> iterate(String signature,
                                        Consumer<String> unknownSignatureConsumer);

  CloseableIterator<TermStruct> iterateSignatures(TermStruct indicator);

  CloseableIterator<TermOperator> makeOperatorIterator();

  boolean assertZ(JProlContext context, TermStruct clause);

  boolean assertA(JProlContext context, TermStruct clause);

  boolean retractAll(JProlContext context, TermStruct clause);

  boolean retractA(JProlContext context, TermStruct clause);

  boolean retractZ(JProlContext context, TermStruct clause);

  boolean abolish(JProlContext context, String signature);

  /**
   * Make copy of the knowledge base with all internal states.
   *
   * @return copy of the knowledge base
   */
  KnowledgeBase makeCopy();

  /**
   * To check that the knowledge base is concurrent one and can be used in multithread environment.
   *
   * @return true if concurrent one, false otherwise
   */
  boolean isConcurrent();

  /**
   * Clear internal states of the knowledge base.
   *
   * @since 2.3.0
   */
  void clear();
}
