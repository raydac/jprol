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
 * The class describes a ground term which can save a float value
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.data.NumericTerm
 */
public final class TermFloat extends Term implements NumericTerm {

  /**
   * The variable contains the term value
   */
  private final float floatValue;

  /**
   * A constructor, it allows to make the float number term from a string
   *
   * @param name the string represents a float number, mut ot be null
   */
  public TermFloat(final String name) {
    super(name);

    int len = name.length() - 1;
    while (len >= 0) {
      if (Character.isWhitespace(name.charAt(len--))) {
        throw new NumberFormatException();
      }
    }

    floatValue = Float.parseFloat(name);
  }

  /**
   * A constructor, it allows to make the float number term from a float value
   *
   * @param value the float value which will be used by the term
   */
  public TermFloat(final float value) {
    super("");
    floatValue = value;
  }

  @Override
  public String toString() {
    return getText();
  }

  @Override
  public int hashCode() {
    return (int) floatValue;
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }

    if (obj.getClass() == TermFloat.class) {
      return ((TermFloat) obj).floatValue == floatValue;
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

    if (atom.getTermType() == TYPE_ATOM){
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
   * Get current value represented by the term
   *
   * @return the float value of the term
   */
  public float getValue() {
    return floatValue;
  }

  @Override
  public Number getNumericValue() {
    return floatValue;
  }

  @Override
  public String forWrite() {
    return getText().isEmpty() ? Float.toString(floatValue) : getText();
  }

  @Override
  public String getSourceLikeRepresentation() {
    String text = getText();
    if (text.indexOf('.') < 0) {
      text += ".0";
    }
    return text;
  }

  @Override
  public String getText() {
    final String value = super.getText();
    if (value.isEmpty()) {
      return Float.toString(floatValue);
    }
    else {
      return value;
    }
  }

  @Override
  public int compare(NumericTerm atom) {
    final float value = atom.getNumericValue().floatValue();
    return Float.compare(floatValue, value);
  }

  @Override
  public NumericTerm add(NumericTerm atom) {
    final float value = atom.getNumericValue().floatValue();
    return new TermFloat(floatValue + value);
  }

  @Override
  public NumericTerm sub(NumericTerm atom) {
    final float value = atom.getNumericValue().floatValue();
    return new TermFloat(floatValue - value);
  }

  @Override
  public NumericTerm div(NumericTerm atom) {
    final float value = atom.getNumericValue().floatValue();
    return new TermFloat(floatValue / value);
  }

  @Override
  public NumericTerm mul(NumericTerm atom) {
    final float value = atom.getNumericValue().floatValue();
    return new TermFloat(floatValue * value);
  }

  @Override
  public NumericTerm neg() {
    return new TermFloat(-floatValue);
  }

  @Override
  public int getNumberType() {
    return NUMBER_FLOAT;
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
          NumericTerm num = (NumericTerm) atom;
          float value = num.getNumericValue().floatValue();
          if (floatValue < value) {
            return -1;
          }
          if (floatValue > value) {
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
    if (!(atom instanceof TermFloat)) {
      return true;
    }

    final TermFloat thatFloat = (TermFloat) atom;

    return Float.compare(floatValue, thatFloat.floatValue) != 0;
  }

  @Override
  public NumericTerm abs() {
    if (floatValue >= 0) {
      return this;
    }
    return new TermFloat(Math.abs(floatValue));
  }

  @Override
  public NumericTerm sign() {
    return new TermFloat(Math.signum(floatValue));
  }
}
