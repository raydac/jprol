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

import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.utils.lazy.LazyMap;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Stream;

public final class TermVar extends Term {

  private static final AtomicLong UID_GENERATOR = new AtomicLong(0);

  /**
   * Loop counter to detect too long variable chains if someone by mistake made self-loop
   */
  private static final int LOOP_WATCHDOG = 8192;
  private final long uid;
  private final boolean anonymous;
  private Term immediateValue;

  private TermVar(
      final String name,
      final boolean anonymous,
      final Term immediateValue,
      final Object payload,
      final SourcePosition sourcePosition
  ) {
    super(name, payload, sourcePosition);
    this.uid = UID_GENERATOR.incrementAndGet();
    this.immediateValue = immediateValue;
    this.anonymous = anonymous;
  }

  TermVar(final String name, final SourcePosition sourcePosition) {
    this(name, false, null, null, sourcePosition);
  }

  TermVar(final String name, final Object payload, final SourcePosition sourcePosition) {
    this(name, false, null, payload, sourcePosition);
  }

  TermVar(final Term immediateValue, final Object payload, final SourcePosition sourcePosition) {
    this(Long.toString(UID_GENERATOR.incrementAndGet()), true, immediateValue,
        payload, sourcePosition);
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

  public long getVarUid() {
    return this.uid;
  }

  @Override
  public TermType getTermType() {
    return VAR;
  }

  /**
   * Check that a variable presented in among variables in values.
   *
   * @param otherVar variable to be checked
   * @return true if var presented among variables in hosted variable chain.
   * @since 2.2.0
   */
  public boolean hasAmongValues(final TermVar otherVar) {
    TermVar current = this;
    while (current != null) {
      if (this.isAnonymous() || otherVar.isAnonymous()) {
        return false;
      }
      if (this.immediateValue == null || this.immediateValue.getTermType() != VAR ||
          this.immediateValue == this) {
        return false;
      }
      if (this.immediateValue == otherVar ||
          this.immediateValue.getText().equals(otherVar.getText())) {
        return true;
      }
      current = (TermVar) this.immediateValue;
    }
    return false;
  }

  @Override
  public Term makeClone() {
    Term thisValue = this.getImmediateValue();
    if (thisValue != null) {
      final Map<Long, TermVar> variableMap = new LazyMap<>();
      variableMap.put(this.getVarUid(), this);
      thisValue = this.makeClone(variableMap);
    }

    return this.isAnonymous() ?
        new TermVar(thisValue, this.getPayload(), this.getSourcePosition()) :
        new TermVar(this.getText(), false, thisValue, this.payload, this.getSourcePosition());
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
    final String name = this.getText();
    final TermVar found = variables.get(name);
    if (found == null) {
      variables.put(name, this);
    } else if (!this.unifyWith(found)) {
      throw new IllegalStateException("Can't unify state between same named variables: " + name);
    }
  }

  @Override
  public Term cloneAndReplaceVariableByValue(final Map<Long, TermVar> variables) {
    Term value = this.getValue();
    if (value == null) {
      if (this.isAnonymous()) {
        return this;
      }

      final Term result;
      final Term immediateValue = this.getImmediateValue();
      if (immediateValue == null) {
        final String varName = this.getText();
        final long varId = this.getVarUid();
        TermVar newVar = variables.get(varId);
        if (newVar == null) {
          newVar = newVar(varName, this.getSourcePosition());
          variables.put(varId, newVar);

          final Term thisVal = this.getImmediateValue();

          if (thisVal != null) {
            newVar.setImmediateValue(thisVal.cloneAndReplaceVariableByValue(variables));
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
  protected Term makeClone(final Map<Long, TermVar> variables) {
    final Term result;

    final Term thisValue = this.getImmediateValue();
    if (thisValue == null) {
      if (this.isAnonymous()) {
        return new TermVar((Term) null, this.payload, this.getSourcePosition());
      }
      final String varName = this.getText();
      final long varId = this.getVarUid();
      TermVar newVariable = variables.get(varId);
      if (newVariable == null) {
        newVariable = new TermVar(varName, this.payload, this.getSourcePosition());
        variables.put(varId, newVariable);
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
   * Set value. if there is already value as variable then chain will be processed recursively and the value will be placed in the last chained non-grounded variable.
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
        (T) this.immediateValue.findGroundOrDefault(this);
  }

  @Override
  public <T extends Term> T findGroundOrDefault(final T defaultTerm) {
    return this.immediateValue == null ? defaultTerm :
        this.immediateValue.findGroundOrDefault(defaultTerm);
  }

  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder();
    final Term val = getValue();
    if (val == null) {
      builder.append(isAnonymous() ? '_' : getText());
    } else {
      builder.append(val);
    }
    return builder.toString();
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
   * @param value value to be save, can be null
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
    return Objects.hashCode(this.uid);
  }

  @Override
  public boolean equals(Object obj) {
    // every variable is unique for its uid, we can call them as a singleton so it can be equals only to itself
    return this == obj;
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
        return "_";
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
    return getText().equals(name);
  }
}
