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

package com.igormaznitsa.prol.data;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static com.igormaznitsa.prol.data.TermType.VAR;

public final class Var extends Term {

  private static final AtomicInteger VAR_COUNTER_ANONYM = new AtomicInteger(0);
  private static final AtomicInteger VAR_COUNTER_UID = new AtomicInteger(0);
  private final int variableUID;

  private Term value;
  private boolean anonymous;

  public Var(final String name) {
    super(name);
    variableUID = VAR_COUNTER_UID.incrementAndGet();
  }

  public Var() {
    this("_$" + Long.toHexString(VAR_COUNTER_ANONYM.incrementAndGet()));
    anonymous = true;
  }

  public final int getVarUID() {
    return variableUID;
  }

  @Override
  public TermType getTermType() {
    return VAR;
  }

  @Override
  public Term makeClone() {
    final Term value = this.getThisValue();
    Var result = this.isAnonymous() ? new Var() : new Var(this.getText());
    if (value != null) {
      final Map<Integer, Var> vars = new HashMap<>();
      vars.put(this.getVarUID(), result);
      result.setThisValue(doMakeClone(vars));
    }
    return result;
  }

  @Override
  protected Term _makeCloneWithVarReplacement(final Map<Integer, Var> vars) {
    Term value = this.getValue();
    if (value == null) {
      final Term result;
      final Term val = this.getThisValue();
      if (val == null) {
        final String varName = this.getText();
        final int varId = this.getVarUID();
        Var newVar = vars.get(varId);
        if (newVar == null) {
          newVar = this.isAnonymous() ? new Var() : new Var(varName);
          vars.put(varId, newVar);

          final Term thisVal = this.getThisValue();

          if (thisVal != null) {
            newVar.setThisValue(thisVal._makeCloneWithVarReplacement(vars));
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
  protected Term doMakeClone(Map<Integer, Var> vars) {
    final Term result;

    final Term val = this.getThisValue();
    if (val == null) {
      final String varName = this.getText();
      final int varId = this.getVarUID();
      Var newVar = vars.get(varId);
      if (newVar == null) {
        newVar = this.isAnonymous() ? new Var() : new Var(varName);
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
    Term result = value;
    if (result != null && result.getTermType() == VAR) {
      final Var nextVar = (Var) result;
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
        Var curVar = ((Var) value);
        while (!Thread.currentThread().isInterrupted()) {
          if (curVar == this) {
            // loop detected, just return
            return true;
          } else {
            final Term nextval = curVar.getThisValue();
            if (nextval != null && nextval.getTermType() == VAR) {
              curVar = (Var) nextval;
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
          ((Var) this.value).setValue(value);
        } else {
          result = curValue.Equ(value);
        }
      }
    }
    return result;
  }

  @Override
  public Stream<Var> variables() {
    return Stream.of(this);
  }

  public boolean isAnonymous() {
    return anonymous;
  }

  @Override
  public String getSourceLikeRepresentation() {
    String result = "_";
    if (!isAnonymous()) {
      result = getText();
    }
    return result;
  }

  public final boolean isUndefined() {
    boolean result = false;
    if (value == null) {
      result = true;
    } else {
      if (value.getTermType() == VAR) {
        result = ((Var) value).isUndefined();
      }
    }
    return result;
  }

  @Override
  public boolean isAllVariablesInstantiated() {
    if (isAnonymous()) {
      return true;
    }
    return !isUndefined();
  }

  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder();
    final Term val = getValue();
    if (val == null) {
      builder.append(isAnonymous() ? '_' : getText());//.append("{uid=").append(variableUID).append('}');
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

  public Var getDeepestVar() {
    Var curVar = this;
    while (!Thread.currentThread().isInterrupted()) {
      final Term term = curVar.getThisValue();
      if (term == null || term.getTermType() != VAR) {
        break;
      } else {
        curVar = (Var) term;
      }
    }
    return curVar;
  }

  public void changeValue(final Term value) {
    Var deepestVar = getDeepestVar();
    deepestVar.setThisValue(value);
  }

  @Override
  public int hashCode() {
    return variableUID;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (this == obj) {
      return true;
    }
    if (obj.getClass() == Var.class) {
      final Var that = (Var) obj;
      return (variableUID == that.variableUID && that.getText().hashCode() == getText().hashCode());
    }
    return false;
  }

  @Override
  public String getSignature() {
    return ".Var." + getText();
  }

  @Override
  public String forWrite() {
    final Term val = getValue();
    if (val == null) {
      if (isAnonymous()) {
        return "_";
      } else {
        return getText();
      }
    } else {
      return val.forWrite();
    }
  }

  @Override
  public boolean Equ(final Term atom) {
    boolean result = true;
    if (this != atom) {
      final Term val = getValue();
      if (val == null) {
        result = setValue(atom);
      } else {
        result = val.Equ(atom);
      }
    }
    return result;
  }

  @Override
  public boolean equWithoutSet(final Term atom) {
    boolean result = true;
    if (this != atom) {
      final Term val = getValue();
      if (val != null) {
        result = val.equWithoutSet(atom);
      }
    }
    return result;
  }

  @Override
  public int termComparsion(Term atom) {
    if (this == atom) {
      return 0;
    }

    Term thisAtom = getValue();
    if (thisAtom == null) {
      thisAtom = this;
    }

    if (atom.getTermType() == VAR && !((Var) atom).isUndefined()) {
      atom = ((Var) atom).getValue();
    }

    int result = -1;
    if (thisAtom == this) {
      if (atom.getTermType() == VAR) {
        result = getText().compareTo(atom.getText());

      }
    } else {
      result = thisAtom.termComparsion(atom);
    }
    return result;
  }

  @Override
  public boolean hasAnyDifference(final Term atom) {
    if (atom.getTermType() != VAR) {
      return true;
    }

    final Var thatVar = (Var) atom;

    if (!getText().equals(thatVar.getText())) {
      return true;
    }

    final Term thisVal = getValue();
    final Term thatVal = thatVar.getValue();
    if (thisVal == null && thatVal == null) {
      return false;
    }
    if (thisVal != null && thatVal != null) {
      return thisVal.hasAnyDifference(thatVal);
    }
    return true;
  }

  @Override
  public boolean hasVariableWithName(final String name) {
    return getText().equals(name);
  }
}
