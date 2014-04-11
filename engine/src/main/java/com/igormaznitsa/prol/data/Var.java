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

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The class contains the prolog var implementation, very important class for
 * engine work. A variable can contain another variable as a value and in the
 * case we will have a variable chain. There is a protection mechanism to avoid
 * the link to itself but a user can to set values directly and in the case it
 * should be careful.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.data.Term
 */
public final class Var extends Term {

  /**
   * The counter is used to get unique id for new created variable, it is very
   * important
   */
  private static final AtomicInteger NonamedVariableCounter = new AtomicInteger(0);
  /**
   * The counter is used to generate UID for every new generated Variable
   */
  private static final AtomicInteger VariableUIDCounter = new AtomicInteger(0);
  /**
   * The value of the variable, if the variable is not instantiated, it contains
   * null or other variable
   */
  private Term value;
  /**
   * The flag shows that the variable is an anonymous variable and its name has
   * been automatic generated
   */
  private boolean anonymous;
  /**
   * The variable contains generated UIDof the variable in the creation time,
   * there must not be any other variable with such UID
   */
  private final int variableUID;

  /**
   * A constructor allows to create a variable with predefined name, it will be
   * not an anonymous variable
   *
   * @param name the name of the created variable, must not be null
   */
  public Var(final String name) {
    super(name);
    variableUID = VariableUIDCounter.incrementAndGet();
  }

  /**
   * A constructor to create an anonymous variable
   */
  public Var() {
    this("_$" + Long.toHexString(NonamedVariableCounter.incrementAndGet()));
    anonymous = true;
  }

  /**
   * Get the variable UID
   *
   * @return the variable UID as integer
   */
  public final int getVarUID() {
    return variableUID;
  }

  @Override
  public int getTermType() {
    return TYPE_VAR;
  }

  /**
   * Get the value of the variable, if the variable has an other variable as its
   * value, the function will find walue recursively
   *
   * @return the value of the variable if it is instantiated and null if it is
   * not instantiated
   */
  public final Term getValue() {
    Term result = value;
    if (result != null && result.getTermType() == TYPE_VAR) {
      result = ((Var) result).getValue();
    }
    return result;
  }

  /**
   * Set the value for the variable, if the value is a variable then the term
   * will be tried to be set to the child variable.
   *
   * @param value the value for the variable as a Term object
   * @return true if the variable can be instantiated by the value or false if
   * it can't
   */
  public final boolean setValue(final Term value) {
    boolean result = true;

    if (value != this) {

      if (value.getTermType() == Term.TYPE_VAR) {
        // check for loop
        Var curVar = ((Var) value);
        while (true) {
          if (curVar == this) {
            // loop detected, just return
            return true;
          }
          else {
            final Term nextval = curVar.getThisValue();
            if (nextval != null && nextval.getTermType() == Term.TYPE_VAR) {
              curVar = (Var) nextval;
            }
            else {
              break;
            }
          }
        }
      }

      if (this.value == null) {
        this.value = value;
      }
      else {
        final Term curValue = getValue();
        if (curValue == null) {
          ((Var) this.value).setValue(value);
        }
        else {
          result = curValue.Equ(value);
        }
      }
    }
    return result;
  }

  @Override
  public void fillVarables(final Map<String, Var> table) {
    table.put(getText(), this);
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

  /**
   * Check that the variable is instantiated
   *
   * @return true if the variable instantiated else false
   */
  public final boolean isUndefined() {
    boolean result = false;
    if (value == null) {
      result = true;
    }
    else {
      if (value.getTermType() == TYPE_VAR) {
        result = ((Var) value).isUndefined();
      }
    }
    return result;
  }

  @Override
  public boolean checkVariables() {
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
    }
    else {
      builder.append(val.toString());
    }
    return builder.toString();
  }

  /**
   * Get the value of the variable without any recursion into child variables if
   * they are presented.
   *
   * @return the term which is the direct value of the variable, without any
   * inside recursion.
   */
  public Term getThisValue() {
    return value;
  }

  public Var getDeepestVar() {
    Var curVar = this;
    while (true) {
      final Term term = curVar.getThisValue();
      if (term == null || term.getTermType() != Term.TYPE_VAR) {
        return curVar;
      }
      else {
        curVar = (Var) term;
      }
    }
  }

  /**
   * Set the value of the variable without any inside recursion, just directly.
   *
   * @param value it is new direct value of the variable, it will be set to the
   * variable even it has another variable as a value already
   */
  public void setThisValue(final Term value) {
    this.value = value;
  }

  /**
   * To change current value for the variable or the variable chain if it is
   * presented
   *
   * @param value the new value for the variable or the variable chain, can be
   * null (then the variable will be not instantiated)
   */
  public void changeValue(final Term value) {
    Var deepestVar = getDeepestVar();
    deepestVar.setThisValue(value);
//        if (this.value == null) {
//            this.value = value;
//        } else {
//            if (this.value.getTermType() == Term.TYPE_VAR) {
//                ((Var) this.value).changeValue(value);
//            } else {
//                this.value = value;
//            }
//        }
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
      }
      else {
        return getText();
      }
    }
    else {
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
      }
      else {
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

    if (atom.getTermType() == Term.TYPE_VAR && !((Var) atom).isUndefined()) {
      atom = ((Var) atom).getValue();
    }

    int result = -1;
    if (thisAtom == this) {
      if (atom.getTermType() == TYPE_VAR) {
        result = getText().compareTo(atom.getText());

      }
    }
    else {
      result = thisAtom.termComparsion(atom);
    }
    return result;
  }

  @Override
  public boolean hasAnyDifference(final Term atom) {
    if (atom.getTermType() != Term.TYPE_VAR) {
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
