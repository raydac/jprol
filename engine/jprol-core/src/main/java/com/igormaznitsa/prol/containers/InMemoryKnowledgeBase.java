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

import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.utils.Utils;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.PrintWriter;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import static com.igormaznitsa.prol.data.TermType.ATOM;
import static com.igormaznitsa.prol.data.Terms.newStruct;
import static java.lang.Integer.parseInt;

public final class InMemoryKnowledgeBase implements KnowledgeBase {

  private final String knowledgeBaseId;
  private final Map<String, TermOperatorContainer> operatorTable = new HashMap<>();
  private final Map<String, InternalKnowledgeBaseClauseList> predicateTable = new HashMap<>();
  private final ReentrantLock operatorLocker = new ReentrantLock();
  private final ReentrantLock predicateLocker = new ReentrantLock();

  public InMemoryKnowledgeBase(final String id) {
    if (id == null) {
      throw new NullPointerException("Id must not be null");
    }
    this.knowledgeBaseId = id;
  }

  private InMemoryKnowledgeBase(final String baseId, final InMemoryKnowledgeBase etalon) {
    this.knowledgeBaseId = baseId;
    etalon.operatorLocker.lock();
    try {
      etalon.predicateLocker.lock();
      try {
        for (final Entry<String, TermOperatorContainer> item : etalon.operatorTable.entrySet()) {
          operatorTable.put(item.getKey(), item.getValue().makeCopy());
        }
        for (final Entry<String, InternalKnowledgeBaseClauseList> item : etalon.predicateTable.entrySet()) {
          predicateTable.put(item.getKey(), item.getValue().makeCopy());
        }
      } finally {
        etalon.predicateLocker.unlock();
      }
    } finally {
      etalon.operatorLocker.unlock();
    }
  }

  @Override
  public String getId() {
    return this.knowledgeBaseId;
  }

  @Override
  public boolean removeOperator(final String name, final OpAssoc type) {
    TermOperatorContainer opContainer;
    operatorLocker.lock();
    try {
      opContainer = operatorTable.get(name);
    } finally {
      operatorLocker.unlock();
    }

    boolean result = false;

    if (opContainer != null) {
        result = opContainer.removeOperatorForType(type);
    }
    return result;
  }

  @Override
  public void addOperator(final ProlContext context, final TermOperator operator) {
    final String operatorName = operator.getText();

    final ReentrantLock lockerOp = operatorLocker;

    lockerOp.lock();
    try {
      if (context.isSystemOperator(operator.getText())) {
        throw new SecurityException("Attemption to override a system operator [" + operator.getText() + ']');
      }

      TermOperatorContainer list = operatorTable.get(operatorName);
      if (list == null) {
        list = new TermOperatorContainer(operator);
        operatorTable.put(operatorName, list);
      } else {
        if (!list.setOperator(operator)) {
          throw new SecurityException("Such or a compatible operator is already presented [" + operatorName + ']');
        }
      }
    } finally {
      lockerOp.unlock();
    }
  }

  @Override
  public TermOperatorContainer findOperatorForName(final ProlContext context, final String name) {
    final TermOperatorContainer systemOperator = context.getSystemOperatorForName(name);
    TermOperatorContainer result;

    if (systemOperator == null) {

      final ReentrantLock lockerOp = operatorLocker;

      lockerOp.lock();
      try {
        result = operatorTable.get(name);
      } finally {
        lockerOp.unlock();
      }
    } else {
      result = systemOperator;
    }
    return result;
  }

  @Override
  public boolean hasOperatorStartsWith(final ProlContext context, final String str) {
    boolean result = false;
    if (context.hasSystemOperatorStartsWith(str)) {
      result = true;
    } else {
      final ReentrantLock lockerOp = operatorLocker;

      lockerOp.lock();
      try {
        for (String s : operatorTable.keySet()) {
          if (s.startsWith(str)) {
            result = true;
            break;
          }
        }
      } finally {
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
      for (TermOperatorContainer container : operatorTable.values()) {
        container.write(writer);
      }
      writer.println();
    } finally {
      lockerOp.unlock();
    }

    // write predicates
    lockerPred.lock();
    try {
      for (InternalKnowledgeBaseClauseList list : predicateTable.values()) {
        list.write(writer);
      }
    } finally {
      lockerPred.unlock();
    }
  }

  private boolean assertClause(final ProlContext context, final TermStruct clause, final boolean asFirst) {
    try {
      final String uid;
      if (clause.isClause()) {
        Term leftPart = clause.getElement(0);
        if (leftPart.getTermType() == ATOM) {
          leftPart = newStruct(leftPart);
          clause.setElement(0, leftPart);
        }
        uid = leftPart.getSignature();
      } else {
        uid = clause.getSignature();
      }

      boolean result;

      final ReentrantLock lockerPred = predicateLocker;
      lockerPred.lock();
      try {
        InternalKnowledgeBaseClauseList list = predicateTable.get(uid);
        if (list == null) {
          // it's new
          list = new InternalKnowledgeBaseClauseList();
          predicateTable.put(uid, list);
        }

        result = asFirst ? list.asserta(clause) : list.assertz(clause);
      } finally {
        lockerPred.unlock();
      }

      // notify triggers if they are presented
      if (result && context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_ASSERT)) {
        context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_ASSERT);
      }

      return result;

    } catch (IllegalArgumentException ex) {
      throw new ProlKnowledgeBaseException("You can't add such atom into the base [" + clause.toSrcString() + ']', ex);
    }
  }

  @Override
  public ClauseIterator getClauseIterator(final ClauseIteratorType type, final TermStruct template) {
    final String uid = template.getSignature();

    final ReentrantLock lockerPred = this.predicateLocker;

    lockerPred.lock();
    try {
      final InternalKnowledgeBaseClauseList list = this.predicateTable.get(uid);

      ClauseIterator result = null;

      if (list != null) {
        result = new MemoryClauseIterator(type, list, template);
      }
      return result;
    } finally {
      lockerPred.unlock();
    }
  }

  @Override
  public List<TermStruct> findAllForPredicateIndicator(final Term predicateIndicator) {
    this.predicateLocker.lock();
    try {
      return this.predicateTable.keySet()
          .stream()
          .map(key -> {
            final int index = key.lastIndexOf('/');
            return newStruct(Utils.SIGNATURE_OPERATOR,
                new Term[] {
                    Terms.newAtom(key.substring(0, index)),
                    Terms.newLong(parseInt(key.substring(index + 1)))
                });
          })
          .filter(predicateIndicator::dryUnifyTo)
          .collect(Collectors.toList());
    } finally {
      this.predicateLocker.unlock();
    }
  }

  @Override
  public List<TermStruct> findAllForSignature(final String signature) {
    this.predicateLocker.lock();
    try {
      final InternalKnowledgeBaseClauseList list = this.predicateTable.get(signature);

      if (list == null) {
        return Collections.emptyList();
      } else {
        return list.asList();
      }
    } finally {
      this.predicateLocker.unlock();
    }
  }


  @Override
  public boolean assertZ(final ProlContext context, final TermStruct clause) {
    return assertClause(context, clause, false);
  }

  @Override
  public boolean assertA(final ProlContext context, final TermStruct clause) {
    return assertClause(context, clause, true);
  }

  @Override
  public boolean retractAll(final ProlContext context, final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isClause()) {
      // it's a clause
      struct = struct.getElement(0);
    }

    final ReentrantLock lockerPred = predicateLocker;

    boolean result = false;
    String uid;

    lockerPred.lock();
    try {
      uid = struct.getSignature();
      final InternalKnowledgeBaseClauseList list = predicateTable.get(uid);

      if (list != null) {
        result = list.retractall(struct) != 0;
        if (result && list.size() == 0) {
          // delete from base
          predicateTable.remove(uid);
        }
      }

    } finally {
      lockerPred.unlock();
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public boolean retractA(final ProlContext context, final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isClause()) {
      // it's a clause
      struct = struct.getElement(0);
    }

    final ReentrantLock lockerPred = predicateLocker;
    boolean result = false;
    String uid;

    lockerPred.lock();
    try {
      uid = struct.getSignature();
      final InternalKnowledgeBaseClauseList list = predicateTable.get(uid);

      if (list != null) {
        result = list.retracta(struct);
        if (result && list.size() == 0) {
          // delete from base
          predicateTable.remove(uid);
        }
      }

    } finally {
      lockerPred.unlock();
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public boolean retractZ(final ProlContext context, final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isClause()) {
      // it's a clause
      struct = struct.getElement(0);
    }

    boolean result = false;
    String uid;

    final ReentrantLock lockerPred = predicateLocker;
    lockerPred.lock();
    try {
      uid = struct.getSignature();
      final InternalKnowledgeBaseClauseList list = predicateTable.get(uid);

      if (list != null) {
        result = list.retractz(struct);
        if (result && list.size() == 0) {
          // delete from base
          predicateTable.remove(uid);
        }
      }
    } finally {
      lockerPred.unlock();
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public void abolish(final ProlContext context, final String signature) {
    final ReentrantLock lockerPred = predicateLocker;

    boolean result;

    final String normalizedUID = Utils.normalizeSignature(signature);
    if (normalizedUID == null) {
      throw new IllegalArgumentException("Wrong signature format \'" + signature + '\'');
    }

    lockerPred.lock();
    try {
      result = predicateTable.remove(normalizedUID) != null;
    } finally {
      lockerPred.unlock();
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(normalizedUID, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(normalizedUID, ProlTriggerType.TRIGGER_RETRACT);
    }
  }

  @Override
  public Iterator<TermOperatorContainer> getOperatorIterator() {
    final ReentrantLock lockerOp = operatorLocker;

    lockerOp.lock();
    try {
      return operatorTable.values().iterator();
    } finally {
      lockerOp.unlock();
    }
  }

  @Override
  public KnowledgeBase makeCopy() {
    return new InMemoryKnowledgeBase(knowledgeBaseId + "_copy", this);
  }
}
