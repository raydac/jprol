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
import com.igormaznitsa.jprol.utils.lazy.LazySet;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

final class VariableStateSnapshot {

  private final List<VariableStateContainer> containerList;

  VariableStateSnapshot(final VariableStateSnapshot snapshot) {
    if (snapshot.containerList.isEmpty()) {
      this.containerList = List.of();
      return;
    }

    final List<VariableStateContainer> containers = new ArrayList<>(snapshot.containerList.size());
    List<VariableStateContainer> changed = null;
    Set<Long> processedVariables = new LazySet<>();

    for (final VariableStateContainer container : snapshot.containerList) {
      if (container.isChanged()) {
        if (changed == null) {
          changed = new ArrayList<>();
        }
        changed.add(container);
      } else {
        processedVariables.add(container.variable.getVarUid());
        containers.add(container);
      }
    }

    if (changed != null) {
      for (final VariableStateContainer container : changed) {
        final Long uid = container.variable.getVarUid();
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
    }

    this.containerList = containers;
  }

  VariableStateSnapshot(final Term source, final Map<String, Term> predefinedValues) {
    this.containerList = new ArrayList<>();
    extractAllVariables(this.containerList, new LazySet<>(), source, predefinedValues);
  }

  private static void extractAllVariables(
      final List<VariableStateContainer> containerList,
      final Set<Long> processedVariables,
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
        final Long uid = var.getVarUid();
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
    }
  }

  void resetToState() {
    this.containerList.forEach(VariableStateContainer::reset);
  }

  @Override
  public String toString() {
    final StringBuilder buffer = new StringBuilder();
    buffer.append(super.toString());
    buffer.append('[');
    final Iterator<VariableStateContainer> iterator = this.containerList.iterator();
    boolean notFirst = false;
    while (iterator.hasNext()) {
      final VariableStateContainer variableStateContainer = iterator.next();

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
