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

package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static java.lang.System.identityHashCode;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

final class VariableStateSnapshot {

  private static final VariableStateContainer[] EMPTY = new VariableStateContainer[0];
  private static final VariableStateSnapshot EMPTY_SNAPSHOT = new VariableStateSnapshot(EMPTY);

  private final VariableStateContainer[] containers;

  private VariableStateSnapshot(final VariableStateContainer[] containers) {
    this.containers = containers;
  }

  static VariableStateSnapshot ofRoot(
      final Term source,
      final Map<String, Term> predefinedValues
  ) {
    if (source == null || !source.canContainVariables()) {
      return null;
    }
    final List<VariableStateContainer> collected = new ArrayList<>();
    extractAllVariables(collected, new VisitedVariables(0), source, predefinedValues);
    return collected.isEmpty() ? null : new VariableStateSnapshot(collected.toArray(EMPTY));
  }

  static VariableStateSnapshot ofSubgoalTerms(
      final Term rootGoal,
      final Term currentGoal,
      final Term nextAndGoal
  ) {
    final List<VariableStateContainer> collected = new ArrayList<>();
    final VisitedVariables visited = new VisitedVariables(0);
    if (rootGoal != null && rootGoal.canContainVariables()) {
      extractAllVariables(collected, visited, rootGoal, null);
    }
    if (currentGoal != null && currentGoal != rootGoal && currentGoal.canContainVariables()) {
      extractAllVariables(collected, visited, currentGoal, null);
    }
    if (nextAndGoal != null && nextAndGoal.canContainVariables()) {
      extractAllVariables(collected, visited, nextAndGoal, null);
    }
    return collected.isEmpty() ? EMPTY_SNAPSHOT :
        new VariableStateSnapshot(collected.toArray(EMPTY));
  }

  private static void extractAllVariables(
      final List<VariableStateContainer> containerList,
      final VisitedVariables processedVariables,
      final Term src,
      final Map<String, Term> predefinedValues
  ) {
    Term current = src;
    while (current != null) {
      final Term next;
      switch (current.getTermType()) {
        case LIST:
          next = extractFromList(containerList, processedVariables, (TermList) current,
              predefinedValues);
          break;
        case STRUCT:
          next = extractFromStruct(containerList, processedVariables, (TermStruct) current,
              predefinedValues);
          break;
        case VAR: {
          final TermVar var = (TermVar) current;
          if (!processedVariables.add(var)) {
            return;
          }
          final Term immediate = var.getImmediateValue();
          if (predefinedValues != null) {
            if (immediate == null || predefinedValues.containsKey(var.getText())) {
              containerList.add(new VariableStateContainer(var, predefinedValues));
            }
          } else if (immediate == null) {
            containerList.add(new VariableStateContainer(var));
          }
          next = immediate;
          break;
        }
        default:
          return;
      }
      current = next;
    }
  }

  VariableStateSnapshot snapshotCurrentState() {
    final VariableStateContainer[] source = this.containers;
    if (source.length == 0) {
      return this;
    }

    int changedCount = 0;
    boolean mayIntroduceVariables = false;
    for (final VariableStateContainer container : source) {
      if (container.isChanged()) {
        changedCount++;
        final Term immediate = container.variable.getImmediateValue();
        if (immediate == null || immediate.canContainVariables()) {
          mayIntroduceVariables = true;
        }
      }
    }
    if (changedCount == 0) {
      return this;
    }

    if (!mayIntroduceVariables) {
      final int remaining = source.length - changedCount;
      if (remaining == 0) {
        return EMPTY_SNAPSHOT;
      }
      final VariableStateContainer[] shrunk = new VariableStateContainer[remaining];
      int out = 0;
      for (final VariableStateContainer container : source) {
        if (!container.isChanged()) {
          shrunk[out++] = container;
        }
      }
      return new VariableStateSnapshot(shrunk);
    }

    final List<VariableStateContainer> updatedList = new ArrayList<>(source.length);
    final List<VariableStateContainer> changed = new ArrayList<>(changedCount);
    final VisitedVariables processedVariables = new VisitedVariables(source.length);

    for (final VariableStateContainer container : source) {
      if (container.isChanged()) {
        changed.add(container);
      } else {
        processedVariables.add(container.variable);
        updatedList.add(container);
      }
    }

    for (final VariableStateContainer container : changed) {
      if (processedVariables.add(container.variable)) {
        final Term immediate = container.variable.getImmediateValue();
        if (immediate == null) {
          updatedList.add(new VariableStateContainer(container.variable));
        } else {
          extractAllVariables(updatedList, processedVariables, immediate, null);
        }
      }
    }

    return updatedList.isEmpty()
        ? EMPTY_SNAPSHOT
        : new VariableStateSnapshot(updatedList.toArray(EMPTY));
  }

  private static Term extractFromList(
      final List<VariableStateContainer> containerList,
      final VisitedVariables processedVariables,
      final TermList startList,
      final Map<String, Term> predefinedValues
  ) {
    TermList list = startList;
    while (!list.isNullList()) {
      extractAllVariables(containerList, processedVariables, list.getHead(), predefinedValues);
      final Term tail = list.getTail();
      if (tail.getTermType() != LIST) {
        return tail;
      }
      list = (TermList) tail;
    }
    return null;
  }

  private static Term extractFromStruct(
      final List<VariableStateContainer> containerList,
      final VisitedVariables processedVariables,
      final TermStruct struct,
      final Map<String, Term> predefinedValues
  ) {
    final int arity = struct.getArity();
    if (arity == 0) {
      return null;
    }
    for (int i = 0; i < arity - 1; i++) {
      extractAllVariables(
          containerList, processedVariables, struct.getArgumentAt(i), predefinedValues);
    }
    return struct.getArgumentAt(arity - 1);
  }

  void resetToState() {
    for (final VariableStateContainer container : this.containers) {
      if (container.isChanged()) {
        container.reset();
      }
    }
  }

  @Override
  public String toString() {
    final StringBuilder buffer = new StringBuilder();
    buffer.append(super.toString());
    buffer.append('[');
    boolean notFirst = false;
    for (final VariableStateContainer variableStateContainer : this.containers) {
      if (notFirst) {
        buffer.append(',');
      } else {
        notFirst = true;
      }

      final String valueTxt;
      final TermVar value = variableStateContainer.variable;
      if (value == null) {
        valueTxt = ".NULL";
      } else if (value.getTermType() == VAR) {
        if (value.isGround()) {
          valueTxt = value.toSrcString() + '{' + identityHashCode(value) + '}' + '['
              + value.getValue().toString() + ']';
        } else {
          valueTxt = value.toSrcString() + '{' + identityHashCode(value) + '}';
        }
      } else {
        valueTxt = value.forWrite();
      }

      buffer.append(value == null ? valueTxt : value.toSrcString()).append('{')
          .append(value == null ? "<NULL>" : identityHashCode(value)).append('}').append('=')
          .append(valueTxt);
    }
    buffer.append(']');
    return buffer.toString();
  }

  private static final class VisitedVariables {

    private static final int LINEAR_CAPACITY = 16;

    private final TermVar[] linear;
    private int size;
    private IdentityHashMap<TermVar, Boolean> overflow;

    private VisitedVariables(final int expectedSize) {
      if (expectedSize > LINEAR_CAPACITY) {
        this.linear = null;
        this.overflow = new IdentityHashMap<>(expectedSize * 2);
      } else {
        this.linear = new TermVar[LINEAR_CAPACITY];
      }
    }

    private boolean add(final TermVar variable) {
      if (this.overflow != null) {
        return this.overflow.put(variable, Boolean.TRUE) == null;
      }

      for (int i = 0; i < this.size; i++) {
        if (this.linear[i] == variable) {
          return false;
        }
      }
      if (this.size == LINEAR_CAPACITY) {
        final IdentityHashMap<TermVar, Boolean> promoted =
            new IdentityHashMap<>(LINEAR_CAPACITY * 2);
        for (int i = 0; i < this.size; i++) {
          promoted.put(this.linear[i], Boolean.TRUE);
        }
        this.overflow = promoted;
        return promoted.put(variable, Boolean.TRUE) == null;
      }

      this.linear[this.size++] = variable;
      return true;
    }
  }
}
