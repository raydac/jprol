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

package com.igormaznitsa.prol.kbase.inmemory;

import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.prol.kbase.IteratorType;
import com.igormaznitsa.prol.kbase.KnowledgeBase;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.utils.CloseableIterator;
import com.igormaznitsa.prol.utils.Utils;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.PrintWriter;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

import static com.igormaznitsa.prol.data.TermType.ATOM;
import static com.igormaznitsa.prol.data.Terms.newStruct;
import static com.igormaznitsa.prol.utils.Utils.makeCloseableIterator;
import static java.lang.Integer.parseInt;
import static java.util.Objects.requireNonNull;

public final class InMemoryKnowledgeBase implements KnowledgeBase {

  private final String knowledgeBaseId;
  private final Map<String, TermOperatorContainer> operatorTable = new ConcurrentHashMap<>();
  private final Map<String, List<InMemoryItem>> predicateTable = new ConcurrentHashMap<>();

  public InMemoryKnowledgeBase(final String id) {
    this.knowledgeBaseId = requireNonNull(id, "Id must not be null");
  }

  private InMemoryKnowledgeBase(final String baseId, final InMemoryKnowledgeBase etalon) {
    this.knowledgeBaseId = baseId;
    for (final Entry<String, TermOperatorContainer> item : etalon.operatorTable.entrySet()) {
      operatorTable.put(item.getKey(), item.getValue().makeCopy());
    }
    etalon.predicateTable.forEach((key, value) -> this.predicateTable.put(key, makeClone(value)));
  }

  private static List<InMemoryItem> makeClone(final List<InMemoryItem> src) {
    final List<InMemoryItem> result = new CopyOnWriteArrayList<>();
    src.stream().map(InMemoryItem::makeClone).forEach(result::add);
    return result;
  }

  @Override
  public String getId() {
    return this.knowledgeBaseId;
  }

  @Override
  public boolean removeOperator(final String name, final OpAssoc type) {
    TermOperatorContainer opContainer;
    opContainer = this.operatorTable.get(name);

    boolean result = false;

    if (opContainer != null) {
      result = opContainer.removeOperatorForType(type);
    }
    return result;
  }

  @Override
  public void addOperator(final ProlContext context, final TermOperator operator) {
    final String operatorName = operator.getText();
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
  }

  @Override
  public TermOperatorContainer findOperatorForName(final ProlContext context, final String name) {
    final TermOperatorContainer systemOperator = context.getSystemOperatorForName(name);
    TermOperatorContainer result;

    if (systemOperator == null) {
      result = operatorTable.get(name);
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
      for (String s : operatorTable.keySet()) {
        if (s.startsWith(str)) {
          result = true;
          break;
        }
      }
    }
    return result;
  }

  @Override
  public void write(final PrintWriter writer) {
    if (writer == null) {
      throw new IllegalArgumentException("Writer must not be null");
    }

    // write operators
    final Iterator<TermOperatorContainer> operators = this.makeOperatorIterator();
    while (operators.hasNext()) {
      operators.next().write(writer);
    }
    writer.println();

    // write predicates
    this.predicateTable.values().stream().peek(x -> writer.println()).flatMap(Collection::stream).forEach(x -> x.write(writer));
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

      final List<InMemoryItem> list = predicateTable.computeIfAbsent(uid, x -> new CopyOnWriteArrayList<>());
      if (asFirst) {
        list.add(0, new InMemoryItem(clause));
      } else {
        list.add(new InMemoryItem(clause));
      }
      // notify triggers if they are presented
      if (context.hasRegisteredTriggersForSignature(uid, ProlTriggerType.TRIGGER_ASSERT)) {
        context.notifyTriggersForSignature(uid, ProlTriggerType.TRIGGER_ASSERT);
      }

      return true;

    } catch (IllegalArgumentException ex) {
      throw new ProlKnowledgeBaseException("You can't add such atom into the base [" + clause.toSrcString() + ']', ex);
    }
  }

  @Override
  public CloseableIterator<TermStruct> iterate(final IteratorType type, final TermStruct template) {
    final String uid = template.getSignature();

    final List<InMemoryItem> list = this.predicateTable.get(uid);

    CloseableIterator<TermStruct> result = null;

    if (list != null) {
      result = new InMemoryClauseIterator(type, list, template);
    }
    return result;
  }

  @Override
  public CloseableIterator<TermStruct> iterateSignatures(final TermStruct indicator) {
    return makeCloseableIterator(this.predicateTable.keySet()
        .stream()
        .map(key -> {
          final int index = key.lastIndexOf('/');
          return newStruct(Utils.SIGNATURE_OPERATOR,
              new Term[] {
                  Terms.newAtom(key.substring(0, index)),
                  Terms.newLong(parseInt(key.substring(index + 1)))
              });
        })
        .filter(indicator::dryUnifyTo)
        .collect(Collectors.toList()).iterator(), () -> {
    });
  }

  @Override
  public CloseableIterator<TermStruct> iterate(final String signature) {
    final List<InMemoryItem> list = this.predicateTable.get(signature);

    if (list == null) {
      return makeCloseableIterator(Collections.emptyIterator(), () -> {
      });
    } else {
      final Iterator<InMemoryItem> items = list.iterator();
      return new CloseableIterator<TermStruct>() {
        @Override
        public void close() {

        }

        @Override
        public boolean hasNext() {
          return items.hasNext();
        }

        @Override
        public TermStruct next() {
          return items.next().getClause();
        }
      };
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

    boolean result = false;

    final String signature = struct.getSignature();
    final List<InMemoryItem> list = this.predicateTable.get(signature);

    if (list != null) {
      result = internalRetractAll(list, struct);
      if (result && list.isEmpty()) {
        // delete from base
        this.predicateTable.remove(signature);
      }
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(signature, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(signature, ProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  private boolean internalRetractAll(final List<InMemoryItem> list, final TermStruct clause) {
    final InMemoryClauseIterator iterator = new InMemoryClauseIterator(IteratorType.ANY, list, clause);
    final List<InMemoryItem> toRemove = new ArrayList<>();
    while (iterator.hasNext()) {
      toRemove.add(iterator.nextItem());
    }
    return list.removeAll(toRemove);
  }

  private boolean internalRetractA(final List<InMemoryItem> list, final TermStruct clause) {
    final InMemoryClauseIterator iterator = new InMemoryClauseIterator(IteratorType.ANY, list, clause);
    if (iterator.hasNext()) {
      final InMemoryItem item = iterator.nextItem();
      return list.remove(item);
    } else {
      return false;
    }
  }

  private boolean internalRetractZ(final List<InMemoryItem> list, final TermStruct clause) {
    final InMemoryClauseIterator iterator = new InMemoryClauseIterator(IteratorType.ANY, list, clause);
    InMemoryItem toRemove = null;
    while (iterator.hasNext()) {
      toRemove = iterator.nextItem();
    }
    return toRemove != null && list.remove(toRemove);
  }

  @Override
  public boolean retractA(final ProlContext context, final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isClause()) {
      // it's a clause
      struct = struct.getElement(0);
    }

    boolean result = false;
    final String signature = struct.getSignature();
    final List<InMemoryItem> list = predicateTable.get(signature);

    if (list != null) {
      result = internalRetractA(list, struct);
      if (result && list.isEmpty()) {
        // delete from base
        predicateTable.remove(signature);
      }
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(signature, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(signature, ProlTriggerType.TRIGGER_RETRACT);
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
    String signature;
    signature = struct.getSignature();
    final List<InMemoryItem> list = this.predicateTable.get(signature);

    if (list != null) {
      result = internalRetractZ(list, struct);
      if (result && list.isEmpty()) {
        // delete from base
        this.predicateTable.remove(signature);
      }
    }

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(signature, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(signature, ProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public void abolish(final ProlContext context, final String signature) {
    boolean result;

    final String normalSignature = Utils.normalizeSignature(signature);
    if (normalSignature == null) {
      throw new IllegalArgumentException("Wrong signature format \'" + signature + '\'');
    }

    result = predicateTable.remove(normalSignature) != null;

    // notify triggers if they are presented
    if (result && context.hasRegisteredTriggersForSignature(normalSignature, ProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(normalSignature, ProlTriggerType.TRIGGER_RETRACT);
    }
  }

  @Override
  public CloseableIterator<TermOperatorContainer> makeOperatorIterator() {
    return new CloseableIterator<TermOperatorContainer>() {
      final Iterator<TermOperatorContainer> wrapped = operatorTable.values().iterator();

      @Override
      public void close() {

      }

      @Override
      public boolean hasNext() {
        return this.wrapped.hasNext();
      }

      @Override
      public TermOperatorContainer next() {
        return this.wrapped.next();
      }
    };
  }

  @Override
  public KnowledgeBase makeCopy() {
    return new InMemoryKnowledgeBase(knowledgeBaseId + "_copy", this);
  }
}
