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
import static com.igormaznitsa.jprol.utils.ProlUtils.makeCloseableIterator;
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
import com.igormaznitsa.jprol.utils.ProlUtils;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public abstract class AbstractInMemoryKnowledgeBase implements KnowledgeBase {

  protected final String knowledgeBaseId;
  protected final Map<String, TermOperatorContainer> operatorTable;
  protected final Map<String, List<InMemoryItem>> predicateTable;
  protected final Map<String, List<InMemoryItem>> unmodifablePredicateTable;
  protected final Supplier<List<InMemoryItem>> itemListSupplier;

  public AbstractInMemoryKnowledgeBase(
      final String baseId,
      final AbstractInMemoryKnowledgeBase base,
      final Supplier<Map<String, TermOperatorContainer>> operatorTableSupplier,
      final Supplier<Map<String, List<InMemoryItem>>> predicateTableSupplier,
      final Supplier<List<InMemoryItem>> itermListSupplier) {
    this.knowledgeBaseId = requireNonNull(baseId, "Knowledge base id must not be null");

    this.operatorTable = operatorTableSupplier.get();
    this.predicateTable = predicateTableSupplier.get();
    this.itemListSupplier = itermListSupplier;
    this.unmodifablePredicateTable = Collections.unmodifiableMap(this.predicateTable);

    if (base != null) {
      for (final Entry<String, TermOperatorContainer> item : base.operatorTable.entrySet()) {
        operatorTable.put(item.getKey(), (TermOperatorContainer) item.getValue().makeClone());
      }
      base.predicateTable.forEach((key, value) -> {
        final List<InMemoryItem> list = itermListSupplier.get();
        list.addAll(value);
        this.predicateTable.put(key, list);
      });
    }
  }

  @Override
  public void clear() {
    this.operatorTable.clear();
    this.predicateTable.clear();
  }

  public void printStateAsSource(final PrintWriter writer) {
    this.predicateTable.entrySet().stream()
        .flatMap(e -> {
          writer.println(String.format("%n%% signature '%s'", e.getKey()));
          return e.getValue().stream();
        })
        .forEach(i -> writer.println(String.format("%s.", i.getClause().toSrcString())));
  }

  public Map<String, List<InMemoryItem>> getStorage() {
    return this.unmodifablePredicateTable;
  }

  @Override
  public String getId() {
    return this.knowledgeBaseId;
  }

  @Override
  public boolean removeOperator(final String name, final OpAssoc type) {
    TermOperatorContainer container = this.operatorTable.get(name);
    boolean result = false;
    if (container != null) {
      final TermOperatorContainer newContainer = container.removeType(type);
      if (newContainer != container) {
        this.operatorTable.put(name, newContainer);
        result = true;
      }
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

    TermOperatorContainer container = this.operatorTable.get(operatorName);
    if (container == null) {
      container = TermOperatorContainer.makeFor(operator, operator.getSourcePosition());
      this.operatorTable.put(operatorName, container);
    } else {
      container = container.makeFor(operator);
      if (container == null) {
        throw new IllegalStateException(
            "Attempt to redefine operator for type already presented in container: " + operator);
      } else {
        this.operatorTable.put(operatorName, container);
      }
    }
  }

  @Override
  public TermOperatorContainer findOperatorForName(final JProlContext context, final String name) {
    final TermOperatorContainer systemOperator = context.findSystemOperatorForName(name);
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

  private boolean assertClause(
      final JProlContext context,
      final TermStruct clause,
      final boolean asFirst
  ) {
    try {
      final String uid;
      if (clause.isClause()) {
        Term leftPart = clause.getArgumentAt(0).tryGround();
        final Term rightPart =
            clause.getArity() == 2 ? clause.getArgumentAt(1).tryGround() : null;

        if (rightPart != null) {
          if (rightPart.getTermType() == VAR) {
            if (!leftPart.containsNamedVariable(rightPart.getText())) {
              throw new ProlInstantiationErrorException(
                  "Arguments are not sufficiently instantiated: " + rightPart, clause);
            }
          } else {
            ProlAssertions.assertCallable(rightPart);
          }
        }

        if (leftPart.getTermType() == ATOM) {
          leftPart = newStruct(leftPart);
          clause.setArgumentAt(0, leftPart);
        }
        uid = leftPart.getSignature();
      } else {
        uid = clause.getSignature();
      }

      final List<InMemoryItem> list =
          this.predicateTable.computeIfAbsent(uid, x -> this.itemListSupplier.get());
      if (asFirst) {
        list.add(0, InMemoryItem.fromClause(clause));
      } else {
        list.add(InMemoryItem.fromClause(clause));
      }
      // notify triggers if they are presented
      if (context.hasTrigger(uid, JProlTriggerType.TRIGGER_ASSERT)) {
        context.notifyTriggersForSignature(uid, clause, JProlTriggerType.TRIGGER_ASSERT);
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
      final BiConsumer<String, Term> unknownPredicateConsumer
  ) {
    final String uid = template.getSignature();
    final List<InMemoryItem> list = this.predicateTable.get(uid);
    if (list == null) {
      unknownPredicateConsumer.accept(uid, template);
    }
    return new InMemoryClauseIterator(type, list == null ? List.of() : list,
        template);
  }

  @Override
  public CloseableIterator<TermStruct> iterateSignatures(final TermStruct indicator) {
    return makeCloseableIterator(this.predicateTable.keySet()
        .stream()
        .map(key -> {
          final int index = key.lastIndexOf('/');
          return newStruct(ProlUtils.SIGNATURE_OPERATOR,
              new Term[] {
                  Terms.newAtom(key.substring(0, index)),
                  Terms.newLong(parseInt(key.substring(index + 1)))
              });
        })
        .filter(indicator::isUnifiableWith)
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
      return new CloseableIterator<>() {
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
    return this.assertClause(context, clause, false);
  }

  @Override
  public boolean assertA(final JProlContext context, final TermStruct clause) {
    return this.assertClause(context, clause, true);
  }

  @Override
  public boolean retractAll(final JProlContext context, final TermStruct clause) {
    TermStruct struct = clause;
    if (struct.isClause()) {
      // it's a clause
      struct = struct.getArgumentAt(0);
    }

    boolean result = false;

    final String signature = struct.getSignature();
    final List<InMemoryItem> list = this.predicateTable.get(signature);

    if (list != null) {
      if (context.hasTrigger(signature, JProlTriggerType.TRIGGER_RETRACT)) {
        result = this.internalRetractAll(list, struct, x ->
            context.notifyTriggersForSignature(signature, x.getHead(),
                JProlTriggerType.TRIGGER_RETRACT));
      } else {
        result = this.internalRetractAll(list, struct, x -> {
        });
      }
      if (result && list.isEmpty()) {
        // delete from base
        this.predicateTable.remove(signature);
      }
    }

    return result;
  }

  private boolean internalRetractAll(
      final List<InMemoryItem> list,
      final TermStruct clause,
      final Consumer<InMemoryItem> removeConsumer
  ) {
    final List<InMemoryItem> foundForRemove = new ArrayList<>();
    boolean result;
    try (InMemoryClauseIterator iterator = new InMemoryClauseIterator(IteratorType.ANY, list,
        clause)) {
      while (iterator.hasNext()) {
        foundForRemove.add(iterator.nextItem());
      }
      result = list.removeAll(foundForRemove);
    }
    foundForRemove.forEach(removeConsumer);
    return result;
  }

  private InMemoryItem internalRetractA(final List<InMemoryItem> list, final TermStruct clause) {
    try (final InMemoryClauseIterator iterator =
             new InMemoryClauseIterator(IteratorType.ANY, list, clause)) {
      if (iterator.hasNext()) {
        final InMemoryItem item = iterator.nextItem();
        list.remove(item);
        return item;
      } else {
        return null;
      }
    }
  }

  private InMemoryItem internalRetractZ(final List<InMemoryItem> list, final TermStruct clause) {
    try (final InMemoryClauseIterator iterator =
             new InMemoryClauseIterator(IteratorType.ANY, list, clause)) {
      InMemoryItem foundLastItem = null;
      while (iterator.hasNext()) {
        foundLastItem = iterator.nextItem();
      }

      if (foundLastItem == null) {
        return null;
      } else {
        list.remove(foundLastItem);
        return foundLastItem;
      }
    }
  }

  @Override
  public boolean retractA(final JProlContext context, final TermStruct clause) {
    return this.makeRetract(context, clause, this::internalRetractA);
  }

  @Override
  public boolean retractZ(final JProlContext context, final TermStruct clause) {
    return this.makeRetract(context, clause, this::internalRetractZ);
  }

  private boolean makeRetract(final JProlContext context, final TermStruct clause,
                              final BiFunction<List<InMemoryItem>, TermStruct, InMemoryItem> searchFunction) {
    TermStruct struct = clause;
    if (struct.isClause()) {
      final Term head = struct.getArgumentAt(0).tryGround();
      ProlAssertions.assertStruct(head);
      struct = (TermStruct) head;
    }

    InMemoryItem result = null;
    final String signature = struct.getSignature();
    final List<InMemoryItem> list = this.predicateTable.get(signature);

    if (list != null) {
      result = searchFunction.apply(list, struct);
      if (result != null) {
        if (list.isEmpty()) {
          // delete from base
          this.predicateTable.remove(signature);
        }
      }
    }

    if (result != null) {
      if (!result.getHead().unifyWith(struct)) {
        throw new Error(
            "Detected unexpected state when found record can't be unify with search! Contact developer: " +
                clause);
      }
      if (context.hasTrigger(signature, JProlTriggerType.TRIGGER_RETRACT)) {
        context.notifyTriggersForSignature(signature, result.getHead(),
            JProlTriggerType.TRIGGER_RETRACT);
      }
      return true;
    } else {
      return false;
    }
  }

  @Override
  public boolean abolish(final JProlContext context, final String signature) {
    boolean result;

    final String normalSignature = ProlUtils.normalizeSignature(signature);
    if (normalSignature == null) {
      throw new IllegalArgumentException("Wrong signature format '" + signature + '\'');
    }

    result = this.predicateTable.remove(normalSignature) != null;

    if (result && context
        .hasTrigger(normalSignature, JProlTriggerType.TRIGGER_ABOLISH)) {
      context.notifyTriggersForSignature(normalSignature, null, JProlTriggerType.TRIGGER_ABOLISH);
    }
    return result;
  }

}
