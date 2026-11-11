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

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.utils.lazy.LinearLongSet;
import java.util.ArrayList;
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
    final LinearLongSet processedVariables = new LinearLongSet();

    for (final VariableStateContainer container : source) {
      if (container.isChanged()) {
        changed.add(container);
      } else {
        processedVariables.add(container.variable.getVarUid());
        containers.add(container);
      }
    }

    for (final VariableStateContainer container : changed) {
      final long uid = container.variable.getVarUid();
      if (!processedVariables.contains(uid)) {
        processedVariables.add(uid);
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
    extractAllVariables(collected, new LinearLongSet(), source, predefinedValues);
    this.containers = collected.isEmpty() ? EMPTY : collected.toArray(EMPTY);
  }

  private static void extractAllVariables(
      final List<VariableStateContainer> containerList,
      final LinearLongSet processedVariables,
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
        final long uid = var.getVarUid();
        if (!processedVariables.contains(uid)) {
          processedVariables.add(uid);
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
            valueTxt = value.toSrcString() + '{' + value.getVarUid() + '}' + '[' +
                value.getValue().toString() + ']';
          } else {
            valueTxt = value.toSrcString() + '{' + value.getVarUid() + '}';
          }
        } else {
          valueTxt = value.forWrite();
        }
      }

      buffer.append(value == null ? valueTxt : value.toSrcString()).append('{')
          .append(value == null ? "<NULL>" : value.getVarUid()).append('}').append('=')
          .append(valueTxt);
    }
    buffer.append(']');
    return buffer.toString();
  }

}
