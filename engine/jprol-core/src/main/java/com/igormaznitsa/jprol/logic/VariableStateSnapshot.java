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

  private final List<VariableContainer> containers;
  private Set<Long> processedVariables;

  VariableStateSnapshot(final VariableStateSnapshot snapshot) {
    if (snapshot.containers.isEmpty()) {
      this.containers = List.of();
      return;
    }

    this.containers = new ArrayList<>();

    final Iterator<VariableContainer> iterator = snapshot.containers.iterator();
    List<VariableContainer> changed = null;

    this.processedVariables = new LazySet<>();

    while (iterator.hasNext()) {
      final VariableContainer container = iterator.next();
      if (container.isChanged()) {
        if (changed == null) {
          changed = new ArrayList<>();
        }
        changed.add(container);
      } else {
        this.processedVariables.add(container.variable.getVarUid());
        this.containers.add(container);
      }
    }

    if (changed != null) {
      for (final VariableContainer container : changed) {
        final Long uid = container.variable.getVarUid();
        if (!this.processedVariables.contains(uid)) {
          this.processedVariables.add(uid);
          this.containers.add(new VariableContainer(container.variable, null));
          this.extractAllVariables(container.variable.getImmediateValue(), null);
        }
      }
    }
    this.processedVariables = null;
  }

  VariableStateSnapshot(final Term source, final Map<String, Term> predefinedValues) {
    this.containers = new ArrayList<>();
    this.extractAllVariables(source, predefinedValues);
    this.processedVariables = null;
  }

  private void extractAllVariables(final Term src, final Map<String, Term> predefinedValues) {
    if (src == null) {
      return;
    }
    switch (src.getTermType()) {
      case LIST: {
        final TermList list = (TermList) src;
        if (!list.isNullList()) {
          this.extractAllVariables(list.getHead(), predefinedValues);
          this.extractAllVariables(list.getTail(), predefinedValues);
        }
      }
      break;
      case STRUCT: {
        final TermStruct struct = (TermStruct) src;
        final Term[] elements = struct.getArguments();
        for (final Term element : elements) {
          this.extractAllVariables(element, predefinedValues);
        }
      }
      break;
      case VAR: {
        if (this.processedVariables == null) {
          this.processedVariables = new LazySet<>();
        }
        final TermVar var = (TermVar) src;
        final Long uid = var.getVarUid();
        if (!this.processedVariables.contains(uid)) {
          this.processedVariables.add(uid);
          this.containers.add(new VariableContainer(var, predefinedValues));
          final Term value = var.getImmediateValue();
          if (value != null) {
            this.extractAllVariables(value, predefinedValues);
          }
        }
      }
      break;
    }
  }

  void resetToState() {
    this.containers.forEach(VariableContainer::resetToSample);
  }

  @Override
  public String toString() {
    final StringBuilder buffer = new StringBuilder();
    buffer.append(super.toString());
    buffer.append('[');
    final Iterator<VariableContainer> iterator = this.containers.iterator();
    boolean notFirst = false;
    while (iterator.hasNext()) {
      final VariableContainer variableContainer = iterator.next();

      if (notFirst) {
        buffer.append(',');
      } else {
        notFirst = true;
      }

      final String valueTxt;
      final TermVar value = variableContainer.variable;
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

  private static final class VariableContainer {
    final TermVar variable;
    final Term sampleValue;

    VariableContainer(final TermVar var, final Map<String, Term> predefinedValues) {
      this.variable = var;

      if (predefinedValues == null) {
        this.sampleValue = var.getImmediateValue();
      } else {
        final Term predefined = predefinedValues.get(var.getText());
        this.sampleValue = predefined == null ? var.getImmediateValue() : predefined;
      }
    }

    void resetToSample() {
      this.variable.setImmediateValue(this.sampleValue);
    }

    boolean isChanged() {
      return this.variable.getImmediateValue() != this.sampleValue;
    }
  }
}
