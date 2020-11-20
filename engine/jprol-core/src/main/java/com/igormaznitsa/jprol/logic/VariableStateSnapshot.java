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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

final class VariableStateSnapshot {

  private final List<VariableContainer> containers;
  private Set<Integer> processedVariables;

  public VariableStateSnapshot(final VariableStateSnapshot snapshot) {
    this.containers = new ArrayList<>();

    final Iterator<VariableContainer> iterator = snapshot.containers.iterator();
    List<VariableContainer> changed = null;

    this.processedVariables = new HashSet<>();

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
      for (VariableContainer container : changed) {
        final int uid = container.variable.getVarUid();
        if (!this.processedVariables.contains(uid)) {
          this.processedVariables.add(uid);
          this.containers.add(new VariableContainer(container.variable, null));
          extractAllVariables(container.variable.getThisValue(), null);
        }
      }
    }
    this.processedVariables = null;
  }

  public VariableStateSnapshot(final Term source, final Map<String, Term> predefValues) {
    this.containers = new ArrayList<>();
    extractAllVariables(source, predefValues);
    this.processedVariables = null;
  }

  private void extractAllVariables(final Term src, final Map<String, Term> predefValues) {
    if (src == null) {
      return;
    }
    switch (src.getTermType()) {
      case LIST: {
        final TermList list = (TermList) src;
        if (!list.isNullList()) {
          extractAllVariables(list.getHead(), predefValues);
          extractAllVariables(list.getTail(), predefValues);
        }
      }
      break;
      case STRUCT: {
        final TermStruct struct = (TermStruct) src;
        final Term[] elements = struct.getElementArray();
        for (Term element : elements) {
          extractAllVariables(element, predefValues);
        }
      }
      break;
      case VAR: {
        if (this.processedVariables == null) {
          this.processedVariables = new HashSet<>();
        }
        final TermVar var = (TermVar) src;
        final Integer uid = var.getVarUid();
        if (!this.processedVariables.contains(uid)) {
          this.processedVariables.add(uid);
          this.containers.add(new VariableContainer(var, predefValues));
          final Term value = var.getThisValue();
          if (value != null) {
            extractAllVariables(value, predefValues);
          }
        }
      }
      break;
    }
  }

  public void resetToState() {
    this.containers.forEach(VariableContainer::resetToEtalon);
  }

  public int getSize() {
    return this.containers.size();
  }

  @Override
  public String toString() {
    final StringBuilder buffer = new StringBuilder();
    buffer.append(super.toString());
    buffer.append('[');
    final Iterator<VariableContainer> iter = this.containers.iterator();
    boolean notfirst = false;
    while (iter.hasNext()) {
      final VariableContainer varcont = iter.next();

      if (notfirst) {
        buffer.append(',');
      } else {
        notfirst = true;
      }

      final String valueTxt;
      final TermVar value = varcont.variable;
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
    final Term etalonValue;

    VariableContainer(final TermVar var, final Map<String, Term> predefinedValues) {
      this.variable = var;

      if (predefinedValues == null) {
        this.etalonValue = var.getThisValue();
      } else {
        final Term predef = predefinedValues.get(var.getText());
        this.etalonValue = predef == null ? var.getThisValue() : predef;
      }
    }

    void resetToEtalon() {
      this.variable.setThisValue(this.etalonValue);
    }

    boolean isChanged() {
      return this.variable.getThisValue() != this.etalonValue;
    }
  }
}
