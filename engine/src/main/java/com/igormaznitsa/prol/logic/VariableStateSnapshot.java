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
package com.igormaznitsa.prol.logic;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.utils.IntegerHashSet;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * This class describes a variable state container for a goal. It allows to save
 * and restore variable values
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
final class VariableStateSnapshot {

  /**
   * Inside class to save a variable-value pair
   *
   * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
   */
  private final static class VariableContainer {

    /**
     * The variable contains the value
     */
    final Var variable;

    /**
     * The variable contains the etalon value for the variable
     */
    final Term etalonValue;

    /**
     * The constructor
     *
     * @param var the source var which one will be saved into the container
     */
    public VariableContainer(final Var var) {
      variable = var;
      etalonValue = var.getThisValue();
    }

    /**
     * Reset the variable's value to the etalon value
     */
    public void resetToEtalon() {
      variable.setThisValue(etalonValue);
    }

    /**
     * Check that the variable's value is not equals to the etalon value
     *
     * @return true if the variable was changed, else false
     */
    public boolean isChanged() {
      return variable.getThisValue() != etalonValue;
    }
  }

  /**
   * The list of containers for variables
   */
  private final List<VariableContainer> containers;

  /**
   * Inside temporary hashset to save uids of processed variables, it is being
   * used only at the constructor
   */
  private IntegerHashSet processedVariables;

  /**
   * A Constructor, to make new object based on values from other shapshot
   *
   * @param snapshot the snapshot whose variables will be used in the new one,
   * must not be null
   */
  public VariableStateSnapshot(final VariableStateSnapshot snapshot) {
    containers = new LinkedList<VariableContainer>();

    final List<VariableContainer> thatContainers = snapshot.containers;

    final Iterator<VariableContainer> iterator = thatContainers.iterator();
    List<VariableContainer> changed = null;

    processedVariables = new IntegerHashSet();

    while (iterator.hasNext()) {
      final VariableContainer container = iterator.next();
      if (container.isChanged()) {
        if (changed == null) {
          changed = new ArrayList<VariableContainer>(16);
        }
        changed.add(container);
      }
      else {
        processedVariables.add(container.variable.getVarUID());
        containers.add(container);
      }
    }

    if (changed != null) {
      final Iterator<VariableContainer> iter = changed.iterator();
      while (iter.hasNext()) {
        final VariableContainer container = iter.next();
        final int uid = container.variable.getVarUID();
        if (!processedVariables.contains(uid)) {
          processedVariables.add(uid);
          containers.add(new VariableContainer(container.variable));
          extractAllVariables(container.variable.getThisValue());
        }
      }
    }
    processedVariables = null;
  }

  /**
   * A constructor allows to make snapshot based on the term
   *
   * @param source the term whose variables will be saved at the snapshot, must
   * not be null
   */
  public VariableStateSnapshot(final Term source) {
    containers = new LinkedList<VariableContainer>();
    extractAllVariables(source);
    processedVariables = null;
  }

  /**
   * Function extracts all found variables from the term into inside storage
   *
   * @param src the source term to be processed
   */
  private void extractAllVariables(final Term src) {
    if (src == null) {
      return;
    }
    switch (src.getTermType()) {
      case Term.TYPE_LIST: {
        final TermList list = (TermList) src;
        if (!list.isNullList()) {
          extractAllVariables(list.getHead());
          extractAllVariables(list.getTail());
        }
      }
      break;
      case Term.TYPE_STRUCT: {
        final TermStruct struct = (TermStruct) src;
        final Term[] elements = struct.getElementsAsArray();
        final int len = elements.length;
        for (int li = 0; li < len; li++) {
          extractAllVariables(elements[li]);
        }
      }
      break;
      case Term.TYPE_VAR: {
        if (processedVariables == null) {
          processedVariables = new IntegerHashSet();
        }
        final Var var = (Var) src;
        final Integer uid = var.getVarUID();
        if (!processedVariables.contains(uid)) {
          processedVariables.add(uid);
          containers.add(new VariableContainer(var));
          final Term value = var.getThisValue();
          if (value != null) {
            extractAllVariables(value);
          }
        }
      }
      break;
    }
  }

  /**
   * Reset all saved variables to their saved original states
   */
  public void resetToState() {
    final Iterator<VariableContainer> iterator = containers.iterator();
    while (iterator.hasNext()) {
      iterator.next().resetToEtalon();
    }
  }

  /**
   * Get the inside variable storage size
   *
   * @return the number of variables saved in the snapshot
   */
  public int getSize() {
    return containers.size();
  }

  @Override
  public String toString() {
    final StringBuilder buffer = new StringBuilder();
    buffer.append(super.toString());
    buffer.append('[');
    final Iterator<VariableContainer> iter = containers.iterator();
    boolean notfirst = false;
    while (iter.hasNext()) {
      final VariableContainer varcont = iter.next();

      if (notfirst) {
        buffer.append(',');
      }
      else {
        notfirst = true;
      }

      String valueTxt = null;
      final Term value = varcont.variable;
      if (value == null) {
        valueTxt = ".NULL";
      }
      else {
        if (value.getTermType() == Term.TYPE_VAR) {
          final Var var = (Var) value;
          if (var.isUndefined()) {
            valueTxt = var.getSourceLikeRepresentation() + '{' + var.getVarUID() + '}';
          }
          else {
            valueTxt = var.getSourceLikeRepresentation() + '{' + var.getVarUID() + '}' + '[' + var.getValue().toString() + ']';
          }
        }
        else {
          valueTxt = value.forWrite();
        }
      }

      final Var variable = varcont.variable;

      buffer.append(variable.getSourceLikeRepresentation()).append('{').append(variable.getVarUID()).append('}').append('=').append(valueTxt);
    }
    buffer.append(']');
    return buffer.toString();
  }
}
