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

package com.igormaznitsa.jprol.kbase.inmemory;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.utils.Utils.makeCloseableIterator;
import static java.lang.Integer.parseInt;
import static java.util.Objects.requireNonNull;


import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermOperatorContainer;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.jprol.kbase.IteratorType;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.kbase.inmemory.items.InMemoryItem;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggerType;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import com.igormaznitsa.jprol.utils.OperatorIterator;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.Utils;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Consumer;
import java.util.stream.Collectors;

public final class InMemoryKnowledgeBase implements KnowledgeBase {

  private final String knowledgeBaseId;
  private final Map<String, TermOperatorContainer> operatorTable = new ConcurrentHashMap<>();
  private final Map<String, List<InMemoryItem>> predicateTable = new ConcurrentHashMap<>();

  public InMemoryKnowledgeBase(final String id) {
    this.knowledgeBaseId = requireNonNull(id, "Id must not be null");
  }

  private InMemoryKnowledgeBase(final String baseId, final InMemoryKnowledgeBase base) {
    this.knowledgeBaseId = baseId;
    for (final Entry<String, TermOperatorContainer> item : base.operatorTable.entrySet()) {
      operatorTable.put(item.getKey(), item.getValue().makeCopy());
    }
    base.predicateTable.forEach((key, value) -> this.predicateTable.put(key, makeClone(value)));
  }

  private static List<InMemoryItem> makeClone(final List<InMemoryItem> src) {
    return new CopyOnWriteArrayList<>(src);
  }

  public long printStateAsSrc(final PrintWriter writer) {
    return this.predicateTable.entrySet().stream()
        .peek(e -> writer.println(String.format("%n%% signature '%s'", e.getKey())))
        .flatMap(e -> e.getValue().stream())
        .peek(i -> writer.println(String.format("%s.", i.getClause().toSrcString())))
        .count();
  }

  public Map<String, List<InMemoryItem>> getStorage() {
    return Collections.unmodifiableMap(this.predicateTable);
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
  public void addOperator(final JProlContext context, final TermOperator operator) {
    final String operatorName = operator.getText();
    if (context.isSystemOperator(operator.getText())) {
      throw new SecurityException(
          "Attempt to override a system operator [" + operator.getText() + ']');
    }

    TermOperatorContainer list = this.operatorTable.get(operatorName);
    if (list == null) {
      list = new TermOperatorContainer(operator);
      this.operatorTable.put(operatorName, list);
    } else {
      if (!list.setOperator(operator)) {
        throw new SecurityException(
            "Either such one or compatible operator already presented [" + operatorName + ']');
      }
    }
  }

  @Override
  public TermOperatorContainer findOperatorForName(final JProlContext context, final String name) {
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
  public boolean hasOperatorStartsWith(final JProlContext context, final String str) {
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
  public CloseableIterator<TermOperator> makeOperatorIterator() {
    return new OperatorIterator(this.operatorTable.values().iterator());
  }

  private boolean assertClause(final JProlContext context, final TermStruct clause,
                               final boolean asFirst) {
    try {
      final String uid;
      if (clause.isClause()) {
        Term leftPart = clause.getElement(0).findNonVarOrSame();
        final Term rightPart =
            clause.getArity() == 2 ? clause.getElement(1).findNonVarOrSame() : null;

        if (rightPart != null) {
          if (rightPart.getTermType() == VAR) {
            if (!leftPart.hasVariableWithName(rightPart.getText())) {
              throw new ProlInstantiationErrorException(
                  "Arguments are not sufficiently instantiated: " + rightPart, clause);
            }
          } else {
            ProlAssertions.assertCallable(rightPart);
          }
        }

        if (leftPart.getTermType() == ATOM) {
          leftPart = newStruct(leftPart);
          clause.setElement(0, leftPart);
        }
        uid = leftPart.getSignature();
      } else {
        uid = clause.getSignature();
      }

      final List<InMemoryItem> list =
          this.predicateTable.computeIfAbsent(uid, x -> new CopyOnWriteArrayList<>());
      if (asFirst) {
        list.add(0, InMemoryItem.fromClause(clause));
      } else {
        list.add(InMemoryItem.fromClause(clause));
      }
      // notify triggers if they are presented
      if (context.hasRegisteredTriggersForSignature(uid, JProlTriggerType.TRIGGER_ASSERT)) {
        context.notifyTriggersForSignature(uid, JProlTriggerType.TRIGGER_ASSERT);
      }

      return true;

    } catch (IllegalArgumentException ex) {
      throw new ProlKnowledgeBaseException(
          "You can't add such atom into the base [" + clause.toSrcString() + ']', ex);
    }
  }

  @Override
  public CloseableIterator<TermStruct> iterate(
      final IteratorType type,
      final TermStruct template,
      final Consumer<String> unknownPredicateConsumer
  ) {
    final String uid = template.getSignature();
    final List<InMemoryItem> list = this.predicateTable.get(uid);
    if (list == null) {
      unknownPredicateConsumer.accept(uid);
    }
    return new InMemoryClauseIterator(type, list == null ? Collections.emptyList() : list,
        template);
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
  public CloseableIterator<TermStruct> iterate(
      final String signature,
      final Consumer<String> unknownPredicateConsumer) {
    final List<InMemoryItem> list = this.predicateTable.get(signature);

    if (list == null) {
      unknownPredicateConsumer.accept(signature);
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
  public boolean assertZ(final JProlContext context, final TermStruct clause) {
    return assertClause(context, clause, false);
  }

  @Override
  public boolean assertA(final JProlContext context, final TermStruct clause) {
    return assertClause(context, clause, true);
  }

  @Override
  public boolean retractAll(final JProlContext context, final TermStruct clause) {
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
    if (result &&
        context.hasRegisteredTriggersForSignature(signature, JProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(signature, JProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  private boolean internalRetractAll(final List<InMemoryItem> list, final TermStruct clause) {
    try(final InMemoryClauseIterator iterator =
        new InMemoryClauseIterator(IteratorType.ANY, list, clause)) {
      final List<InMemoryItem> toRemove = new ArrayList<>();
      while (iterator.hasNext()) {
        toRemove.add(iterator.nextItem());
      }
      return list.removeAll(toRemove);
    }
  }

  private boolean internalRetractA(final List<InMemoryItem> list, final TermStruct clause) {
    try(final InMemoryClauseIterator iterator =
        new InMemoryClauseIterator(IteratorType.ANY, list, clause)) {
      if (iterator.hasNext()) {
        final InMemoryItem item = iterator.nextItem();
        return list.remove(item);
      } else {
        return false;
      }
    }
  }

  private boolean internalRetractZ(final List<InMemoryItem> list, final TermStruct clause) {
    try(final InMemoryClauseIterator iterator =
        new InMemoryClauseIterator(IteratorType.ANY, list, clause)) {
      InMemoryItem toRemove = null;
      while (iterator.hasNext()) {
        toRemove = iterator.nextItem();
      }
      return toRemove != null && list.remove(toRemove);
    }
  }

  @Override
  public boolean retractA(final JProlContext context, final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isClause()) {
      final Term head = struct.getElement(0).findNonVarOrSame();
      ProlAssertions.assertStruct(head);
      struct = (TermStruct) head;
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
    if (result &&
        context.hasRegisteredTriggersForSignature(signature, JProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(signature, JProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public boolean retractZ(final JProlContext context, final TermStruct clause) {
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
    if (result &&
        context.hasRegisteredTriggersForSignature(signature, JProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(signature, JProlTriggerType.TRIGGER_RETRACT);
    }

    return result;
  }

  @Override
  public void abolish(final JProlContext context, final String signature) {
    boolean result;

    final String normalSignature = Utils.normalizeSignature(signature);
    if (normalSignature == null) {
      throw new IllegalArgumentException("Wrong signature format '" + signature + '\'');
    }

    result = predicateTable.remove(normalSignature) != null;

    if (result && context
        .hasRegisteredTriggersForSignature(normalSignature, JProlTriggerType.TRIGGER_RETRACT)) {
      context.notifyTriggersForSignature(normalSignature, JProlTriggerType.TRIGGER_RETRACT);
    }
  }

  @Override
  public KnowledgeBase makeCopy() {
    return new InMemoryKnowledgeBase(knowledgeBaseId + "_copy", this);
  }
}
