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

package com.igormaznitsa.jprol.data;

import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newVar;
import static java.lang.System.identityHashCode;

import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.utils.lazy.LazyMap;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.stream.Stream;

public final class TermVar extends Term {

  private static final String ANONYMOUS_NAME = "_";

  /**
   * Loop counter to detect too long variable chains if someone by mistake made self-loop
   */
  private static final int LOOP_WATCHDOG = 8192;
  private final boolean anonymous;
  private Term immediateValue;

  private TermVar(
      final String name,
      final boolean anonymous,
      final Term immediateValue,
      final SourcePosition sourcePosition
  ) {
    super(name, sourcePosition);
    this.immediateValue = immediateValue;
    this.anonymous = anonymous;
  }

  TermVar(final String name, final SourcePosition sourcePosition) {
    this(name, false, null, sourcePosition);
  }

  TermVar(final Term immediateValue, final SourcePosition sourcePosition) {
    this(ANONYMOUS_NAME, true, immediateValue, sourcePosition);
  }

  static Map<TermVar, TermVar> newVariableMap() {
    return new LazyMap<>(IdentityHashMap::new);
  }

  @Override
  public Term replaceVar(final String varName, final Term targetTerm) {
    if (this.isAnonymous()) {
      return this;
    }
    if (this.getText().equals(varName)) {
      return targetTerm;
    } else {
      return this;
    }
  }

  @Override
  public TermType getTermType() {
    return VAR;
  }

  /**
   * Check that a variable presented in variable chain.
   *
   * @param otherVar variable to be checked
   * @return true if var presented among variables in hosted variable chain.
   * @since 3.0.0
   */
  public boolean isPresentedInVarChain(final TermVar otherVar) {
    if (this.isAnonymous() || otherVar.isAnonymous()) {
      return false;
    }
    TermVar current = this;
    for (int step = 0; step < LOOP_WATCHDOG; step++) {
      final Term next = current.getImmediateValue();
      if (!(next instanceof TermVar) || next == current) {
        return false;
      }
      if (next == otherVar || next.getText().equals(otherVar.getText())) {
        return true;
      }
      current = (TermVar) next;
    }
    return false;
  }

  @Override
  public Term makeClone() {
    Term thisValue = this.getImmediateValue();
    if (thisValue != null) {
      final Map<TermVar, TermVar> variableMap = newVariableMap();
      variableMap.put(this, this);
      thisValue = this.makeClone(variableMap);
    }

    return this.isAnonymous() ?
        new TermVar(thisValue, this.getSourcePosition()) :
        new TermVar(this.getText(), false, thisValue, this.getSourcePosition());
  }

  @Override
  public Number toNumber() {
    final Term data = this.getValue();
    if (data instanceof NumericTerm) {
      return data.toNumber();
    }
    throw new ProlInstantiationErrorException("NonInstantiated variable", this);
  }

  @Override
  public boolean canContainVariables() {
    return true;
  }

  @Override
  protected void arrangeVariableValues(final Map<String, TermVar> variables) {
    if (this.isAnonymous()) {
      return;
    }

    final String name = this.getText();
    final TermVar found = variables.get(name);
    if (found == null) {
      variables.put(name, this);
    } else if (!this.unifyWith(found)) {
      throw new IllegalStateException("Can't unify state between same named variables: " + name);
    }
  }

  @Override
  public Term cloneAndReplaceVariables(final Map<TermVar, TermVar> variables) {
    Term value = this.getValue();
    if (value == null) {
      if (this.isAnonymous()) {
        return this;
      }

      final Term result;
      final Term immediateValue = this.getImmediateValue();
      if (immediateValue == null) {
        final String varName = this.getText();
        TermVar newVar = variables.get(this);
        if (newVar == null) {
          newVar = newVar(varName, this.getSourcePosition());
          variables.put(this, newVar);

          final Term thisVal = this.getImmediateValue();

          if (thisVal != null) {
            newVar.setImmediateValue(thisVal.cloneAndReplaceVariables(variables));
          }
        }
        result = newVar;
      } else {
        result = immediateValue.makeClone(variables);
      }
      return result;
    } else {
      return value;
    }
  }

  @Override
  protected Term makeClone(final Map<TermVar, TermVar> variables) {
    final Term result;

    final Term thisValue = this.getImmediateValue();
    if (thisValue == null) {
      if (this.isAnonymous()) {
        return new TermVar((Term) null, this.getSourcePosition());
      }
      final String varName = this.getText();
      TermVar newVariable = variables.get(this);
      if (newVariable == null) {
        newVariable = new TermVar(varName, this.getSourcePosition());
        variables.put(this, newVariable);
      }
      result = newVariable;
    } else {
      result = thisValue.makeClone(variables);
    }

    return result;
  }

  /**
   * It looks for value. If value is another variable then it will be processed recursive.
   *
   * @return null if no value, ground value or another variable if value presented
   */
  public Term getValue() {
    Term result = this.immediateValue;
    int watchdog = LOOP_WATCHDOG;
    while (result != null && result.getTermType() == VAR) {
      final Term prev = result;
      result = ((TermVar) result).immediateValue;
      if (result == null) {
        result = prev;
        break;
      }
      watchdog--;
      if (watchdog <= 0) {
        throw new IllegalStateException(
            "Too deep variable chain detected, may be loop");
      }
    }
    return result;
  }

  /**
   * Set value. if there is already value as variable as chain will be processed recursively and the value will be placed in the last chained non-grounded variable.
   *
   * @param value value to be set, can't be null
   * @return true if unification of value successful, false otherwise
   */
  public boolean setValue(final Term value) {
    if (value == this) {
      return true;
    } else {
      if (value.getTermType() == VAR) {
        TermVar curVar = ((TermVar) value);
        int watchDogLoop = LOOP_WATCHDOG;
        while (watchDogLoop > 0) {
          watchDogLoop--;
          if (curVar == this) {
            return true;
          } else {
            final Term nextValue = curVar.immediateValue;
            if (nextValue != null && nextValue.getTermType() == VAR) {
              curVar = (TermVar) nextValue;
            } else {
              break;
            }
          }
        }

        if (watchDogLoop <= 0) {
          throw new IllegalStateException(
              "Too deep variable chain detected, may be loop");
        }
      }

      if (this.immediateValue == null) {
        this.immediateValue = value;
        return true;
      } else {
        final Term curValue = getValue();
        if (curValue == null) {
          return ((TermVar) this.immediateValue).setValue(value);
        } else {
          return curValue.unifyWith(value);
        }
      }
    }
  }

  @Override
  public Map<String, TermVar> findAllNamedVariables() {
    if (this.isAnonymous()) {
      return Map.of();
    }
    return Map.of(this.getText(), this);
  }

  @Override
  public Map<String, Term> findAllNamedVariableValues() {
    if (this.isAnonymous()) {
      return Map.of();
    }
    return Map.of(this.getText(), this.tryGroundOrDefault(this));
  }

  @Override
  public Stream<TermVar> variables() {
    return this.isAnonymous() ? Stream.empty() : Stream.of(this);
  }

  @Override
  public boolean isAnonymous() {
    return this.anonymous;
  }

  @Override
  public String toSrcString() {
    return this.getValue() == null ? (this.isAnonymous() ? "_" : this.getText()) :
        this.getValue().toSrcString();
  }

  public boolean isGround() {
    return this.immediateValue != null && this.immediateValue.isGround();
  }

  public boolean isUnground() {
    return this.immediateValue == null ||
        (this.immediateValue.getTermType() == VAR && this.immediateValue.isUnground());
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T extends Term> T tryGround() {
    return this.immediateValue == null ? (T) this :
        (T) this.immediateValue.tryGroundOrDefault(this);
  }

  @Override
  public <T extends Term> T tryGroundOrDefault(final T defaultValue) {
    return this.immediateValue == null ? defaultValue :
        this.immediateValue.tryGroundOrDefault(defaultValue);
  }

  @Override
  public String toString() {
    if (this.isGround()) {
      return this.getValue().toString();
    } else {
      return this.isAnonymous() ? "_" : this.getText();
    }
  }

  /**
   * Get value exactly saved in the variable.
   *
   * @return saved value or null
   */
  public Term getImmediateValue() {
    return this.immediateValue;
  }

  /**
   * Set value to the variable.
   *
   * @param value value to be stored as immediate value, old value will be lost, can be null
   */
  public void setImmediateValue(final Term value) {
    if (value != this) {
      this.immediateValue = value;
    }
  }

  /**
   * Examine chain of values and return the first variable without value or this variable if it is unground one.
   *
   * @return found unground variable in the variable chain or this variable, null if no ungrounded var in chain
   */
  public TermVar findUngroundVariable() {
    if (this.immediateValue == null) {
      return this;
    } else {
      return this.immediateValue.getTermType() == VAR ?
          ((TermVar) this.immediateValue).findUngroundVariable() : null;
    }
  }

  @Override
  public int hashCode() {
    return identityHashCode(this);
  }

  @Override
  public boolean equals(final Object other) {
    return this == other;
  }

  @Override
  public String getSignature() {
    return ".TermVar." + getText();
  }

  @Override
  public String forWrite() {
    final Term val = this.getValue();
    if (val == null) {
      if (this.isAnonymous()) {
        return "_G" + Integer.toUnsignedString(identityHashCode(this));
      } else {
        return this.getText();
      }
    } else {
      return val.forWrite();
    }
  }

  @Override
  public boolean unifyWith(final Term term) {
    if (this == term) {
      return true;
    } else {
      if (this.immediateValue == null && term.getTermType() != VAR) {
        this.immediateValue = term;
        return true;
      }
      final Term foundValue = this.getValue();
      final boolean result;
      if (foundValue == null) {
        result = this.setValue(term);
      } else {
        result = foundValue.unifyWith(term);
      }
      return result;
    }
  }

  @Override
  public boolean isUnifiableWith(final Term target) {
    if (this == target) {
      return true;
    } else {
      final Term varValue = this.getValue();
      return varValue == null || varValue.isUnifiableWith(target);
    }
  }

  @Override
  public boolean containsNamedVariable(final String name) {
    return !this.isAnonymous() && this.getText().equals(name);
  }
}
