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

import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newVar;

public final class TermVar extends Term {

  private static final AtomicInteger ANONYM_GENERATOR = new AtomicInteger(0);
  private static final AtomicInteger UID_GENERATOR = new AtomicInteger(0);
  private final int uid;
  private final boolean anonymous;
  private volatile Term value;

  private TermVar(final String name, final boolean anonymous) {
    super(name);
    this.uid = UID_GENERATOR.incrementAndGet();
    this.anonymous = anonymous;
  }

  TermVar(final String name) {
    this(name, false);
  }

  TermVar() {
    this("_$" + Long.toHexString(ANONYM_GENERATOR.incrementAndGet()), true);
  }

  public final int getVarUid() {
    return this.uid;
  }

  @Override
  public TermType getTermType() {
    return VAR;
  }

  @Override
  public Term makeClone() {
    final Term value = this.getThisValue();
    TermVar result = this.isAnonymous() ? newVar() : newVar(this.getText());
    if (value != null) {
      final Map<Integer, TermVar> vars = new HashMap<>();
      vars.put(this.getVarUid(), result);
      result.setThisValue(doMakeClone(vars));
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
  protected void doArrangeVars(final Map<String, TermVar> variables) {
    final String name = this.getText();
    if (variables.containsKey(name)) {
      final TermVar var = variables.get(name);
      this.unifyTo(var);
    } else {
      variables.put(name, this);
    }
  }


  @Override
  protected Term makeCloneAndVarBound(final Map<Integer, TermVar> vars) {
    Term value = this.getValue();
    if (value == null) {
      final Term result;
      final Term val = this.getThisValue();
      if (val == null) {
        final String varName = this.getText();
        final int varId = this.getVarUid();
        TermVar newVar = vars.get(varId);
        if (newVar == null) {
          newVar = this.isAnonymous() ? newVar() : newVar(varName);
          vars.put(varId, newVar);

          final Term thisVal = this.getThisValue();

          if (thisVal != null) {
            newVar.setThisValue(thisVal.makeCloneAndVarBound(vars));
          }
        }
        result = newVar;
      } else {
        result = val.doMakeClone(vars);
      }
      return result;
    } else {
      return value;
    }
  }

  @Override
  protected Term doMakeClone(Map<Integer, TermVar> vars) {
    final Term result;

    final Term val = this.getThisValue();
    if (val == null) {
      final String varName = this.getText();
      final int varId = this.getVarUid();
      TermVar newVar = vars.get(varId);
      if (newVar == null) {
        newVar = this.isAnonymous() ? newVar() : newVar(varName);
        vars.put(varId, newVar);

        final Term thisVal = this.getThisValue();

        if (thisVal != null) {
          newVar.setThisValue(thisVal.doMakeClone(vars));
        }
      }
      result = newVar;
    } else {
      result = val.doMakeClone(vars);
    }

    return result;
  }

  public final Term getValue() {
    Term result = this.value;
    if (result != null && result.getTermType() == VAR) {
      final TermVar nextVar = (TermVar) result;
      result = nextVar.getValue();
      if (result == null) {
        result = nextVar;
      }
    }
    return result;
  }

  public final boolean setValue(final Term value) {
    boolean result = true;

    if (value != this) {

      if (value.getTermType() == VAR) {
        // check for loop
        TermVar curVar = ((TermVar) value);
        while (!Thread.currentThread().isInterrupted()) {
          if (curVar == this) {
            // loop detected, just return
            return true;
          } else {
            final Term nextval = curVar.getThisValue();
            if (nextval != null && nextval.getTermType() == VAR) {
              curVar = (TermVar) nextval;
            } else {
              break;
            }
          }
        }
      }

      if (this.value == null) {
        this.value = value;
      } else {
        final Term curValue = getValue();
        if (curValue == null) {
          ((TermVar) this.value).setValue(value);
        } else {
          result = curValue.unifyTo(value);
        }
      }
    }
    return result;
  }

  @Override
  public Stream<TermVar> variables() {
    return this.isAnonymous() ? Stream.empty() : Stream.of(this);
  }

  public boolean isAnonymous() {
    return anonymous;
  }

  @Override
  public String toSrcString() {
    return this.getValue() == null ? (this.isAnonymous() ? "_" : this.getText()) : this.getValue().toSrcString();
  }

  public boolean isGround() {
    return this.value != null && this.value.isGround();
  }

  public boolean isFree() {
    return this.value == null || (this.value.getTermType() == VAR && ((TermVar) this.value).isFree());
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
  public <T> T toObject() {
    final Term foundValue = this.findNonVarOrDefault(null);
    if (foundValue == null) {
      throw new IllegalStateException(String.format("Free variable \'%s\' can't be converted into Object", this.getText()));
    } else {
      return foundValue.toObject();
    }
  }

  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder();
    final Term val = getValue();
    if (val == null) {
      builder.append(isAnonymous() ? '_' : getText());
    } else {
      builder.append(val.toString());
    }
    return builder.toString();
  }

  public Term getThisValue() {
    return value;
  }

  public void setThisValue(final Term value) {
    this.value = value;
  }

  private TermVar getDeepestVar() {
    if (this.value == null) {
      return this;
    } else {
      return this.value.getTermType() == VAR ? ((TermVar) this.value).getDeepestVar() : this;
    }
  }

  public void changeVarChainValue(final Term value) {
    this.getDeepestVar().setThisValue(value);
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
      if (isAnonymous()) {
        return "_";
      } else {
        return this.getText();
      }
    } else {
      return val.forWrite();
    }
  }

  @Override
  public boolean unifyTo(final Term atom) {
    boolean result = true;
    if (this != atom) {
      final Term val = getValue();
      if (val == null) {
        result = setValue(atom);
      } else {
        result = val.unifyTo(atom);
      }
    }
    return result;
  }

  @Override
  public boolean dryUnifyTo(final Term atom) {
    if (this == atom) {
      return true;
    } else {
      final Term varValue = this.getValue();
      return varValue == null || varValue.dryUnifyTo(atom);
    }
  }

  @Override
  public int compareTermTo(Term atom) {
    if (this == atom) {
      return 0;
    }
    final Term thisAtom = this.findNonVarOrSame();
    atom = atom.findNonVarOrSame();

    int result = -1;
    if (thisAtom == this) {
      if (atom.getTermType() == VAR) {
        result = getText().compareTo(atom.getText());
      }
    } else {
      result = thisAtom.compareTermTo(atom);
    }
    return result;
  }

  @Override
  public boolean stronglyEqualsTo(final Term term) {
    boolean result = false;
    if (term.getClass() == TermVar.class) {
      final TermVar thatVar = (TermVar) term;

      if (this.uid == thatVar.uid) {
        final Term value = this.value;
        final Term thatValue = thatVar.value;
        if (value == null && thatValue == null) {
          result = true;
        } else {
          result = value != null && thatValue != null && value.stronglyEqualsTo(thatValue);
        }
      }
    }

    return result;
  }

  @Override
  public boolean hasVariableWithName(final String name) {
    return getText().equals(name);
  }
}
