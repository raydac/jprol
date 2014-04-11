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

/**
 * The class describes a ground term which can save an integer value
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.data.NumericTerm
 */
public final class TermInteger extends Term implements NumericTerm {

  /**
   * The variable contains the integer value associated with the numeric term
   */
  private final int intValue;

  /**
   * A constructor, it allows to make the float number term from a string
   *
   * @param name the string represents a float number, mut ot be null
   */
  public TermInteger(final String name) {
    super(name);
    intValue = Integer.parseInt(name);
  }

  /**
   * A constructor allows to make an integer term from an integer numeric value
   *
   * @param value the integer numeric value which will be associated with the
   * term
   */
  public TermInteger(final int value) {
    super("");
    intValue = value;
  }

  @Override
  public String toString() {
    final String val = getText();
    if (val.isEmpty()) {
      return Integer.toString(intValue);
    }
    else {
      return val;
    }
  }

  @Override
  public String forWrite() {
    return getText().isEmpty() ? Integer.toString(intValue) : getText();
  }

  @Override
  public int hashCode() {
    return intValue;
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }

    if (obj.getClass() == TermInteger.class) {
      return ((TermInteger) obj).intValue == intValue;
    }

    return super.equals(obj);
  }

  @Override
  public boolean equWithoutSet(Term atom) {
    if (this == atom) {
      return true;
    }

    if (atom.getTermType() == Term.TYPE_VAR) {
      atom = ((Var) atom).getValue();
    }

    if (atom == null) {
      return true;
    }

    if (atom.getTermType() == TYPE_ATOM) {
      if (atom instanceof NumericTerm) {
        return compare((NumericTerm) atom) == 0;
      }
    }
    return false;
  }

  @Override
  public boolean Equ(Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case Term.TYPE_ATOM: {
        if (atom instanceof NumericTerm) {
          return compare((NumericTerm) atom) == 0;
        }
        else {
          return getText().equals(atom.getText());
        }
      }
      case Term.TYPE_VAR: {
        final Var var = (Var) atom;
        final Term value = var.getValue();
        if (value == null) {
          return ((Var) atom).setValue(this);
        }
        else {
          return Equ(value);
        }
      }
    }
    return false;
  }

  /**
   * Get current integer value represented by the term
   *
   * @return the value as integer
   */
  public int getValue() {
    return intValue;
  }

  @Override
  public Number getNumericValue() {
    return intValue;
  }

  @Override
  public String getSourceLikeRepresentation() {
    String strValue = Integer.toString(intValue);
    return strValue;
  }

  @Override
  public String getText() {
    final String value = super.getText();
    if (value == null) {
      return Integer.toString(intValue);
    }
    else {
      return value;
    }
  }

  @Override
  public int compare(NumericTerm atom) {
    if (atom.getNumberType() == NUMBER_FLOAT) {
      final float value = atom.getNumericValue().floatValue();
      return Float.compare((float) intValue, value);
    }

    final int value = atom.getNumericValue().intValue();
    if (intValue == value) {
      return 0;
    }
    if (intValue < value) {
      return -1;
    }
    return 1;
  }

  @Override
  public NumericTerm add(NumericTerm atom) {
    if (atom.getNumberType() == NUMBER_FLOAT) {
      final float value = atom.getNumericValue().floatValue();
      return new TermFloat((float) intValue + value);
    }
    else {
      final int value = atom.getNumericValue().intValue();
      return new TermInteger(intValue + value);
    }
  }

  @Override
  public NumericTerm sub(NumericTerm atom) {
    if (atom.getNumberType() == NUMBER_FLOAT) {
      final float value = atom.getNumericValue().floatValue();
      return new TermFloat((float) intValue - value);
    }
    else {
      final int value = atom.getNumericValue().intValue();
      return new TermInteger(intValue - value);
    }
  }

  @Override
  public NumericTerm div(NumericTerm atom) {
    if (atom.getNumberType() == NUMBER_FLOAT) {
      final float value = atom.getNumericValue().floatValue();
      return new TermFloat((float) intValue / value);
    }
    else {
      final int value = atom.getNumericValue().intValue();
      return new TermInteger(intValue / value);
    }
  }

  @Override
  public NumericTerm mul(NumericTerm atom) {
    if (atom.getNumberType() == NUMBER_FLOAT) {
      final float value = atom.getNumericValue().floatValue();
      return new TermFloat((float) intValue * value);
    }
    else {
      final int value = atom.getNumericValue().intValue();
      return new TermInteger(intValue * value);
    }
  }

  @Override
  public NumericTerm neg() {
    return new TermInteger(-intValue);
  }

  @Override
  public int getNumberType() {
    return NUMBER_INTEGER;
  }

  @Override
  public int termComparsion(Term atom) {
    if (this == atom) {
      return 0;
    }

    if (atom.getTermType() == Term.TYPE_VAR && !((Var) atom).isUndefined()) {
      atom = ((Var) atom).getValue();
    }

    switch (atom.getTermType()) {
      case Term.TYPE_VAR:
        return 1;
      case Term.TYPE_ATOM: {
        if (atom instanceof NumericTerm) {
          int value = 0;
          if (atom instanceof TermFloat) {
            value = Math.round(((TermFloat) atom).getNumericValue().floatValue());
          }
          else {
            value = ((NumericTerm) atom).getNumericValue().intValue();
          }

          if (intValue < value) {
            return -1;
          }
          if (intValue > value) {
            return 1;
          }
          return 0;
        }
        else {
          return -1;
        }
      }
      default:
        return -1;
    }
  }

  @Override
  public boolean hasAnyDifference(final Term atom) {
    if (!(atom instanceof TermInteger)) {
      return true;
    }

    final TermInteger thatInt = (TermInteger) atom;

    return intValue != thatInt.intValue;
  }

  @Override
  public NumericTerm abs() {
    if (intValue >= 0) {
      return this;
    }
    return new TermInteger(Math.abs(intValue));
  }

  @Override
  public NumericTerm sign() {
    int sign = 0;
    if (intValue < 0) {
      sign = -1;
    }
    else if (intValue > 0) {
      sign = 1;
    }
    return new TermInteger(sign);
  }
}
