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

import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import static com.igormaznitsa.prol.data.TermType.VAR;
import static com.igormaznitsa.prol.data.Terms.newVar;

public final class Var extends Term {

  private static final AtomicInteger ANONYM_GENERATOR = new AtomicInteger(0);
  private static final AtomicInteger UID_GENERATOR = new AtomicInteger(0);
  private final int uid;
  private final boolean anonymous;
  private volatile Term value;

  private Var(final String name, final boolean anonymous) {
    super(name);
    this.uid = UID_GENERATOR.incrementAndGet();
    this.anonymous = anonymous;
  }

  public Var(final String name) {
    this(name, false);
  }

  public Var() {
    this("_$" + Long.toHexString(ANONYM_GENERATOR.incrementAndGet()), true);
  }

  public final int getVarUID() {
    return uid;
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
  public Number toNumber() {
    final Term data = this.getValue();
    if (data instanceof NumericTerm) {
      return data.toNumber();
    }
    throw new ProlInstantiationErrorException("NonInstantiated variable", this);
  }

  @Override
  protected Term makeCloneAndVarBound(final Map<Integer, Var> vars) {
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
          result = curValue.unifyTo(value);
        }
      }
    }
    return result;
  }

  @Override
  public Stream<Var> variables() {
    return this.isAnonymous() ? Stream.empty() : Stream.of(this);
  }

  public boolean isAnonymous() {
    return anonymous;
  }

  @Override
  public String toSrcString() {
    String result = "_";
    if (!isAnonymous()) {
      result = getText();
    }
    return result;
  }

  public boolean isGround() {
    return this.value != null && this.value.isGround();
  }

  public boolean isFree() {
    return this.value == null || (this.value.getTermType() == VAR && ((Var) this.value).isFree());
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
    if (obj.getClass() == Var.class) {
      final Var that = (Var) obj;
      return (uid == that.uid && that.getText().hashCode() == getText().hashCode());
    }
    return false;
  }

  @Override
  public String getSignature() {
    return ".Var." + getText();
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
    boolean result = true;
    if (this != atom) {
      final Term val = getValue();
      if (val != null) {
        result = val.dryUnifyTo(atom);
      }
    }
    return result;
  }

  @Override
  public int compareTermTo(Term atom) {
    if (this == atom) {
      return 0;
    }

    Term thisAtom = getValue();
    if (thisAtom == null) {
      thisAtom = this;
    }

    atom = atom.findNonVarOrDefault(atom);

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
    if (term.getClass() == Var.class) {
      final Var thatVar = (Var) term;

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
