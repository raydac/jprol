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

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.utils.Utils;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The class implements the knowledge base for a Prol engine
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class MemoryKnowledgeBase implements KnowledgeBase {

  /**
   * The variable contains the base identifier
   */
  private final String basedId;
  /**
   * The table contains defined operators
   */
  private final Map<String, OperatorContainer> operatorTable = new HashMap<String, OperatorContainer>();
  /**
   * The table contains defined predicates
   */
  private final Map<String, KnowledgeBaseInsideClauseList> predicateTable = new HashMap<String, KnowledgeBaseInsideClauseList>();
  /**
   * The link to the context which is the owner of the knowledge base
   */
  private final ProlContext context;
  private final ReentrantLock operatorLocker = new ReentrantLock();
  private final ReentrantLock predicateLocker = new ReentrantLock();

  /**
   * The constructor
   *
   * @param context the context-owner, must not be null
   * @param baseId the identifier of the base, must not be null
   */
  public MemoryKnowledgeBase(final ProlContext context, final String baseId) {
    if (baseId == null || context == null) {
      throw new IllegalArgumentException("One from the erguments is null");
    }
    this.basedId = baseId;
    this.context = context;
  }

  /**
   * Auxiliary constructor to make new knowledge base as a snapshot of existent
   * knowledge base
   *
   * @param context the content for new knowledge base
   * @param baseId the base id for new knowledge base
   * @param etalon the etalon knowledge base
   */
  private MemoryKnowledgeBase(final ProlContext context, final String baseId, final MemoryKnowledgeBase etalon) {
    this.basedId = baseId;
    this.context = context;
    etalon.operatorLocker.lock();
    try {
      etalon.predicateLocker.lock();
      try {
        for (final Entry<String, OperatorContainer> item : etalon.operatorTable.entrySet()) {
          operatorTable.put(item.getKey(), item.getValue().makeCopy());
        }
        for (final Entry<String, KnowledgeBaseInsideClauseList> item : etalon.predicateTable.entrySet()) {
          predicateTable.put(item.getKey(), item.getValue().makeCopy());
        }
      }
      finally {
        etalon.predicateLocker.unlock();
      }
    }
    finally {
      etalon.operatorLocker.unlock();
    }
  }

  @Override
  public String getVocabularyId() {
    return basedId;
  }

  @Override
  public void addOperators(final Operator[] operators) {
    operatorLocker.lock();
    try {
      for (int li = 0; li < operators.length; li++) {
        addOperator(operators[li]);
      }
    }
    finally {
      operatorLocker.unlock();
    }
  }

  @Override
  public Operator getOperatorForTypeAndName(final String name, final int type) {
    OperatorContainer opContainer = null;
    operatorLocker.lock();
    try {
      opContainer = operatorTable.get(name);
    }
    finally {
      operatorLocker.unlock();
    }
    Operator result = null;
    if (opContainer != null) {
      result = opContainer.getForTypePrecisely(type);
    }
    return result;
  }

  @Override
  public boolean removeOperator(final String name, final int type) {
    OperatorContainer opContainer;
    operatorLocker.lock();
    try {
      opContainer = operatorTable.get(name);
    }
    finally {
      operatorLocker.unlock();
    }

    boolean result = false;

    if (opContainer != null) {
      if (opContainer.isSystem()) {
        throw new SecurityException("Attemption to remove a system operator");
      }
      else {
        result = opContainer.removeOperatorForType(type);
      }
    }
    return result;
  }

  @Override
  public void addOperator(final Operator operator) {
    final String operatorName = operator.getText();

    final ReentrantLock lockerOp = operatorLocker;

    lockerOp.lock();
    try {
      if (context.isSystemOperator(operator.getText())) {
        throw new SecurityException("Attemption to override a system operator [" + operator.getText() + ']');
      }

      OperatorContainer list = operatorTable.get(operatorName);
      if (list == null) {
        list = new OperatorContainer(operator);
        operatorTable.put(operatorName, list);
      }
      else {
        if (!list.setOperator(operator)) {
          throw new SecurityException("Such or a compatible operator is already presented [" + operatorName + ']');
        }
      }
    }
    finally {
      lockerOp.unlock();
    }
  }

  @Override
  public OperatorContainer findOperatorForName(final String name) {
    final OperatorContainer systemOperator = context.getSystemOperatorForName(name);
    OperatorContainer result = null;

    if (systemOperator == null) {

      final ReentrantLock lockerOp = operatorLocker;

      lockerOp.lock();
      try {
        result = operatorTable.get(name);
      }
      finally {
        lockerOp.unlock();
      }
    }
    else {
      result = systemOperator;
    }
    return result;
  }

  @Override
  public boolean hasOperatorStartsWith(final String str) {
    boolean result = false;
    if (context.hasSystemOperatorStartsWith(str)) {
      result = true;
    }
    else {
      final ReentrantLock lockerOp = operatorLocker;

      lockerOp.lock();
      try {
        final Iterator<String> operators = operatorTable.keySet().iterator();
        while (operators.hasNext()) {
          if (operators.next().startsWith(str)) {
            result = true;
            break;
          }
        }
      }
      finally {
        lockerOp.unlock();
      }
    }
    return result;
  }

  @Override
  public void write(final PrintWriter writer) {
    if (writer == null) {
      throw new IllegalArgumentException("Writer must not be null");
    }

    final ReentrantLock lockerOp = operatorLocker;
    final ReentrantLock lockerPred = predicateLocker;

    lockerOp.lock();
    try {
      // write operators
      final Iterator<OperatorContainer> operators = operatorTable.values().iterator();
      while (operators.hasNext()) {
        final OperatorContainer container = operators.next();
        container.write(writer);
      }
      writer.println();
    }
    finally {
      lockerOp.unlock();
    }

    // write predicates
    lockerPred.lock();
    try {
      final Iterator<KnowledgeBaseInsideClauseList> predicates = predicateTable.values().iterator();
      while (predicates.hasNext()) {
        final KnowledgeBaseInsideClauseList list = predicates.next();
        list.write(writer);
      }
    }
    finally {
      lockerPred.unlock();
    }
  }

  /**
   * Assert new clause ino the knowledge base
   *
   * @param clause the clause to be added
   * @param asFirst if true then the clause will be made as the first in the
   * list of similar clauses, else as the last
   * @return true if the clause has been added successfully, else false
   * @throws com.igormaznitsa.prol.exceptions.ProlKnowledgeBaseException if such
   * clause is incompatible with the knowledge base
   */
  private boolean assertClause(final TermStruct clause, final boolean asFirst) {
    try {
      String uid = null;
      if (clause.isFunctorLikeRuleDefinition()) {
        // it's clause
        // get left part
        Term leftPart = clause.getElement(0);
        if (leftPart.getTermType() == Term.TYPE_ATOM) {
          leftPart = new TermStruct(leftPart);
          clause.setElement(0, leftPart);
        }
        uid = leftPart.getSignature();
      }
      else {
        // it's just structure
        uid = clause.getSignature();
      }

      boolean result = false;

      final ReentrantLock lockerPred = predicateLocker;
      lockerPred.lock();
      try {
        KnowledgeBaseInsideClauseList list = predicateTable.get(uid);
        if (list == null) {
          // it's new
          list = new KnowledgeBaseInsideClauseList();
          predicateTable.put(uid, list);
        }

        result = asFirst ? list.asserta(clause) : list.assertz(clause);
      }
      finally {
        lockerPred.unlock();
      }

      // notify triggers if they are presented
      if (result && context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_ASSERT)) {
        context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_ASSERT);
      }

      return result;

    }
    catch (IllegalArgumentException ex) {
      throw new ProlKnowledgeBaseException("You can't add such atom into the base [" + clause.getSourceLikeRepresentation() + ']', ex);
    }
  }

  @Override
  public FactIterator getFactIterator(final TermStruct template) {
    final String uid = template.getSignature();

    final ReentrantLock lockerPred = predicateLocker;

    lockerPred.lock();
    try {
      final KnowledgeBaseInsideClauseList list = predicateTable.get(uid);
      FactIterator result = null;
      if (list != null) {
        result = new MemoryFactIterator(list, template);
      }
      return result;
    }
    finally {
      lockerPred.unlock();
    }
  }

  @Override
  public RuleIterator getRuleIterator(final TermStruct template) {
    final String uid = template.getSignature();

    final ReentrantLock lockerPred = predicateLocker;
    lockerPred.lock();
    try {
      final KnowledgeBaseInsideClauseList list = predicateTable.get(uid);

      RuleIterator result = null;

      if (list != null) {
        result = new MemoryRuleIterator(list, template);
      }
      return result;
    }
    finally {
      lockerPred.unlock();
    }
  }

  @Override
  public ClauseIterator getClauseIterator(final TermStruct template) {
    final String uid = template.getSignature();
    final ReentrantLock lockerPred = predicateLocker;

    lockerPred.lock();
    try {
      final KnowledgeBaseInsideClauseList list = predicateTable.get(uid);

      ClauseIterator result = null;

      if (list != null) {
        result = new MemoryClauseIterator(list, template);
      }
      return result;
    }
    finally {
      lockerPred.unlock();
    }
  }

  @Override
  public boolean assertZ(final TermStruct clause) {
    return assertClause(clause, false);
  }

  @Override
  public boolean assertA(final TermStruct clause) {
    return assertClause(clause, true);
  }

  @Override
  public boolean retractAll(final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isFunctorLikeRuleDefinition()) {
      // it's a clause
      struct = (TermStruct) struct.getElement(0);
    }

    final ReentrantLock lockerPred = predicateLocker;

    boolean result = false;
    String uid = null;

    lockerPred.lock();
    try {
      uid = struct.getSignature();
      final KnowledgeBaseInsideClauseList list = predicateTable.get(uid);

      if (list != null) {
        result = list.retractall(struct) != 0;
        if (result && list.size() == 0) {
          // delete from base
          predicateTable.remove(uid);
        }
      }

    }
    finally {
      lockerPred.unlock();
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public boolean retractA(final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isFunctorLikeRuleDefinition()) {
      // it's a clause
      struct = (TermStruct) struct.getElement(0);
    }

    final ReentrantLock lockerPred = predicateLocker;
    boolean result = false;
    String uid = null;

    lockerPred.lock();
    try {
      uid = struct.getSignature();
      final KnowledgeBaseInsideClauseList list = predicateTable.get(uid);

      if (list != null) {
        result = list.retracta(struct);
        if (result && list.size() == 0) {
          // delete from base
          predicateTable.remove(uid);
        }
      }

    }
    finally {
      lockerPred.unlock();
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public boolean retractZ(final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isFunctorLikeRuleDefinition()) {
      // it's a clause
      struct = (TermStruct) struct.getElement(0);
    }

    boolean result = false;
    String uid = null;

    final ReentrantLock lockerPred = predicateLocker;
    lockerPred.lock();
    try {
      uid = struct.getSignature();
      final KnowledgeBaseInsideClauseList list = predicateTable.get(uid);

      if (list != null) {
        result = list.retractz(struct);
        if (result && list.size() == 0) {
          // delete from base
          predicateTable.remove(uid);
        }
      }
    }
    finally {
      lockerPred.unlock();
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public void abolish(final String signature) {
    final ReentrantLock lockerPred = predicateLocker;

    boolean result = false;

    final String normalizedUID = Utils.normalizeSignature(signature);
    if (normalizedUID == null) {
      throw new IllegalArgumentException("Wrong signature format \'" + signature + '\'');
    }

    lockerPred.lock();
    try {
      result = predicateTable.remove(normalizedUID) != null;
    }
    finally {
      lockerPred.unlock();
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(normalizedUID, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(normalizedUID, ProlTriggerType.TRIGGER_RETRACT);
    }
  }

  @Override
  public Iterator<OperatorContainer> getOperatorIterator() {
    final ReentrantLock lockerOp = operatorLocker;

    lockerOp.lock();
    try {
      return operatorTable.values().iterator();
    }
    finally {
      lockerOp.unlock();
    }
  }

  @Override
  public KnowledgeBase makeCopy(final ProlContext context) {
    return new MemoryKnowledgeBase(context, basedId + "_copy", this);
  }
}
