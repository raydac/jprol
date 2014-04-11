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
  public String getVocabularyId();

  /**
   * Add operators from an Operator array
   *
   * @param operators an Operator array, must not be null
   */
  public void addOperators(Operator[] operators);

  /**
   * To get an operator for its name and type
   *
   * @param name the operator name, must not be null
   * @param type the operator type
   * @return the found operator or null if not found
   * @see com.igormaznitsa.prol.data.Operator
   */
  public Operator getOperatorForTypeAndName(String name, int type);

  /**
   * Remove an operator from the knowledge base, can be used only for dynamic
   * operators which were not added through annotations
   *
   * @param name the operator name, must not be null
   * @param type the operator type
   * @return true if the operator had been found and removed, else false
   * @throws java.lang.SecurityException will be thrown if it is an operator
   * which was not dynamically added
   * @see com.igormaznitsa.prol.data.Operator
   */
  public boolean removeOperator(String name, int type);

  /**
   * Add an operator into the knowledge base
   *
   * @param operator an operator, must not be null
   * @see com.igormaznitsa.prol.data.Operator
   * @throws java.lang.SecurityException will be thrown if there is such
   * operator already or there is an operator looks like new one
   */
  public void addOperator(Operator operator);

  /**
   * Find an operator container for name
   *
   * @param name the name of operator, must not be null
   * @return null if an operator container is not found or the found operator
   * container
   * @see com.igormaznitsa.prol.containers.OperatorContainer
   */
  public OperatorContainer findOperatorForName(String name);

  /**
   * Check if there is an operator at the base which one starts with a string
   *
   * @param str the string to find an operator, must not be null
   * @return true if there is an operator starts with the string else false
   */
  public boolean hasOperatorStartsWith(String str);

  /**
   * To out all dynamic value of the knowledge into writer in the prolog source
   * format
   *
   * @param writer the writer to be used for the out, must not be null
   */
  public void write(PrintWriter writer);

  /**
   * Get a fact iterator for a template
   *
   * @param template the template to be used for the finding process (must not
   * be null)
   * @return a FactIterator instance if the base contains data for the template,
   * else null
   * @see FactIterator
   */
  public FactIterator getFactIterator(TermStruct template);

  /**
   * Get a rule iterator for the base
   *
   * @param template the template to be used to find rules in the base (must not
   * be null)
   * @return a RuleIterator instance if the base contains data for the template,
   * else null
   * @see RuleIterator
   */
  public RuleIterator getRuleIterator(TermStruct template);

  /**
   * Get a clause iterator for a template
   *
   * @param template the template to find clauses in the base (must not be null)
   * @return a ClauseIterator object if the template is found in the base, else
   * null
   * @see ClauseIterator
   */
  public ClauseIterator getClauseIterator(TermStruct template);

  /**
   * Add a clause as Z-clause into the knowledge base
   *
   * @param clause the clause to be added, must not be null
   * @return true if the clause has been added successfully, else true
   */
  public boolean assertZ(TermStruct clause);

  /**
   * Add a clause as A-clause into the knowledge base
   *
   * @param clause the clause to be added, must not be null
   * @return true if the clause has been added successfully, else true
   */
  public boolean assertA(TermStruct clause);

  /**
   * Retract all clauses from the knowledge base compatible with the key clause
   *
   * @param clause the key clause to remove compatible clauses from the base,
   * must not be null
   * @return true if there is found compatible clauses and removed, else false
   */
  public boolean retractAll(TermStruct clause);

  /**
   * Remove the first found compatible clause for a key clause
   *
   * @param clause the key clause, must not be null
   * @return true if there is found and removed clause, else false
   */
  public boolean retractA(TermStruct clause);

  /**
   * Remove the last found compatible clause for a key clause
   *
   * @param clause the key clause, must not be null
   * @return true if there is found and removed clause, else false
   */
  public boolean retractZ(final TermStruct clause);

  /**
   * Remove all clauses from the base with a key signature
   *
   * @param signature the signature (as an example "predicate/4") to be used for
   * removing operation
   */
  public void abolish(final String signature);

  /**
   * To get the iterator of all defined operators at the base
   *
   * @return the iterator of the inside operator table
   */
  public Iterator<OperatorContainer> getOperatorIterator();

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
  public KnowledgeBase makeCopy(ProlContext context);
}
