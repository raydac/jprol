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

  private final VariableStateContainer[] containers;

  VariableStateSnapshot(final VariableStateSnapshot snapshot) {
    final VariableStateContainer[] source = snapshot.containers;
    if (source.length == 0) {
      this.containers = EMPTY;
      return;
    }

    int changedCount = 0;
    for (final VariableStateContainer container : source) {
      if (container.isChanged()) {
        changedCount++;
      }
    }
    if (changedCount == 0) {
      this.containers = source;
      return;
    }

    final List<VariableStateContainer> containers = new ArrayList<>(source.length);
    final List<VariableStateContainer> changed = new ArrayList<>(changedCount);
    final VisitedVariables processedVariables = new VisitedVariables();

    for (final VariableStateContainer container : source) {
      if (container.isChanged()) {
        changed.add(container);
      } else {
        processedVariables.add(container.variable);
        containers.add(container);
      }
    }

    for (final VariableStateContainer container : changed) {
      if (processedVariables.add(container.variable)) {
        containers.add(new VariableStateContainer(container.variable, null));
        extractAllVariables(
            containers,
            processedVariables,
            container.variable.getImmediateValue(),
            null);
      }
    }

    this.containers = containers.toArray(EMPTY);
  }

  VariableStateSnapshot(final Term source, final Map<String, Term> predefinedValues) {
    final List<VariableStateContainer> collected = new ArrayList<>();
    extractAllVariables(collected, new VisitedVariables(), source, predefinedValues);
    this.containers = collected.isEmpty() ? EMPTY : collected.toArray(EMPTY);
  }

  private static void extractAllVariables(
      final List<VariableStateContainer> containerList,
      final VisitedVariables processedVariables,
      final Term src,
      final Map<String, Term> predefinedValues
  ) {
    if (src == null) {
      return;
    }
    switch (src.getTermType()) {
      case LIST: {
        final TermList list = (TermList) src;
        if (!list.isNullList()) {
          extractAllVariables(containerList, processedVariables, list.getHead(), predefinedValues);
          extractAllVariables(containerList, processedVariables, list.getTail(), predefinedValues);
        }
      }
      break;
      case STRUCT: {
        final TermStruct struct = (TermStruct) src;
        final Term[] elements = struct.getArguments();
        for (final Term element : elements) {
          extractAllVariables(containerList, processedVariables, element, predefinedValues);
        }
      }
      break;
      case VAR: {
        final TermVar var = (TermVar) src;
        if (processedVariables.add(var)) {
          containerList.add(new VariableStateContainer(var, predefinedValues));
          final Term value = var.getImmediateValue();
          if (value != null) {
            extractAllVariables(containerList, processedVariables, value, predefinedValues);
          }
        }
      }
      break;
      default:
        break;
    }
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
      } else {
        if (value.getTermType() == VAR) {
          if (value.isGround()) {
            valueTxt = value.toSrcString() + '{' + identityHashCode(value) + '}' + '[' +
                value.getValue().toString() + ']';
          } else {
            valueTxt = value.toSrcString() + '{' + identityHashCode(value) + '}';
          }
        } else {
          valueTxt = value.forWrite();
        }
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

    private final TermVar[] linear = new TermVar[LINEAR_CAPACITY];
    private int size;
    private IdentityHashMap<TermVar, Boolean> overflow;

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
