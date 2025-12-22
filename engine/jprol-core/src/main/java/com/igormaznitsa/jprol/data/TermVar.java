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
import static com.igormaznitsa.jprol.data.Terms.newAnonymousVar;
import static com.igormaznitsa.jprol.data.Terms.newVar;

import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.utils.LazyMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

public final class TermVar extends Term {

  private static final AtomicInteger ANONYMITY_GENERATOR = new AtomicInteger(0);
  private static final AtomicInteger UID_GENERATOR = new AtomicInteger(0);
  private static final int LOOP_WATCHDOG = 8192;
  private final int uid;
  private final boolean anonymous;
  private volatile Term value;

  private TermVar(final String name, final boolean anonymous, final Object payload,
                  final SourcePosition sourcePosition) {
    super(name, payload, sourcePosition);
    this.uid = UID_GENERATOR.incrementAndGet();
    this.anonymous = anonymous;
  }

  TermVar(final String name, final SourcePosition sourcePosition) {
    this(name, false, null, sourcePosition);
  }

  TermVar(final String name, final Object payload, final SourcePosition sourcePosition) {
    this(name, false, payload, sourcePosition);
  }

  TermVar(final SourcePosition sourcePosition) {
    this("_$" + Long.toHexString(ANONYMITY_GENERATOR.incrementAndGet()), true,
        null, sourcePosition);
  }

  TermVar() {
    this(SourcePosition.UNKNOWN);
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

  public int getVarUid() {
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
      if (this.value == null || this.value.getTermType() != VAR || this.value == this) {
        return false;
      }
      if (this.value == otherVar || this.value.getText().equals(otherVar.getText())) {
        return true;
      }
      current = (TermVar) this.value;
    }
    return false;
  }

  @Override
  public Term makeClone() {
    final Term thisValue = this.getImmediateValue();
    TermVar result =
        this.isAnonymous() ? Terms.newAnonymousVar() :
            newVar(this.getText(), this.payload, this.getSourcePosition());
    if (thisValue != null) {
      final Map<Integer, TermVar> variableMap = new LazyMap<>();
      variableMap.put(this.getVarUid(), result);
      result.setImmediateValue(this.makeClone(variableMap));
    }
    return result;
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
    } else if (!this.unifyTo(found)) {
      throw new IllegalStateException("Can't unify state between same named variables: " + name);
    }
  }

  @Override
  public Term cloneAndReplaceVariablesByValues(final Map<Integer, TermVar> variables) {
    Term value = this.getValue();
    if (value == null) {
      final Term result;
      final Term immediateValue = this.getImmediateValue();
      if (immediateValue == null) {
        final String varName = this.getText();
        final int varId = this.getVarUid();
        TermVar newVar = variables.get(varId);
        if (newVar == null) {
          newVar =
              this.isAnonymous() ? newAnonymousVar() : newVar(varName, this.getSourcePosition());
          variables.put(varId, newVar);

          final Term thisVal = this.getImmediateValue();

          if (thisVal != null) {
            newVar.setImmediateValue(thisVal.cloneAndReplaceVariablesByValues(variables));
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
  protected Term makeClone(final Map<Integer, TermVar> variables) {
    final Term result;

    final Term thisValue = this.getImmediateValue();
    if (thisValue == null) {
      final String varName = this.getText();
      final int varId = this.getVarUid();
      TermVar newVariable = variables.get(varId);
      if (newVariable == null) {
        newVariable = this.isAnonymous() ? newAnonymousVar() :
            newVar(varName, this.payload, this.getSourcePosition());
        variables.put(varId, newVariable);
      }
      result = newVariable;
    } else {
      result = thisValue.makeClone(variables);
    }

    return result;
  }

  public Term getValue() {
    Term result = this.value;
    int watchdog = LOOP_WATCHDOG;
    while (result != null && result.getTermType() == VAR) {
      final Term prev = result;
      result = ((TermVar) result).value;
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
            final Term nextValue = curVar.value;
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

      if (this.value == null) {
        this.value = value;
        return true;
      } else {
        final Term curValue = getValue();
        if (curValue == null) {
          return ((TermVar) this.value).setValue(value);
        } else {
          return curValue.unifyTo(value);
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
    return this.value != null && this.value.isGround();
  }

  public boolean isUnground() {
    return this.value == null ||
        (this.value.getTermType() == VAR && ((TermVar) this.value).isUnground());
  }

  @Override
  @SuppressWarnings("unchecked")
  public <T extends Term> T findNonVarOrSame() {
    return this.value == null ? (T) this : (T) this.value.findNonVarOrDefault(this);
  }

  @Override
  public <T extends Term> T findNonVarOrDefault(final T term) {
    return this.value == null ? term : this.value.findNonVarOrDefault(term);
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
    return this.value;
  }

  /**
   * Set value to the variable.
   *
   * @param value value to be save, can be null
   */
  public void setImmediateValue(final Term value) {
    if (value != this) {
      this.value = value;
    }
  }

  /**
   * Examine chain of values and return the first variable without value or this variable if it is unground one.
   *
   * @return found unground variable in the variable chain or this variable
   */
  public TermVar findUngroundVariable() {
    if (this.value == null) {
      return this;
    } else {
      return this.value.getTermType() == VAR ? ((TermVar) this.value).findUngroundVariable() : this;
    }
  }

  @Override
  public int hashCode() {
    return uid;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (obj.getClass() == TermVar.class) {
      final TermVar that = (TermVar) obj;
      return (uid == that.uid && that.getText().hashCode() == getText().hashCode());
    }
    return false;
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
  public boolean unifyTo(final Term term) {
    if (this == term) {
      return true;
    } else {
      final Term val = this.getValue();
      final boolean result;
      if (val == null) {
        result = this.setValue(term);
      } else {
        result = val.unifyTo(term);
      }
      return result;
    }
  }

  @Override
  public boolean dryUnifyTo(final Term target) {
    if (this == target) {
      return true;
    } else {
      final Term varValue = this.getValue();
      return varValue == null || varValue.dryUnifyTo(target);
    }
  }

  @Override
  public boolean containsNamedVariable(final String name) {
    return getText().equals(name);
  }
}
