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

import static com.igormaznitsa.prol.data.TermType.ATOM;
import static com.igormaznitsa.prol.data.TermType.VAR;
import static com.igormaznitsa.prol.data.Terms.newFloat;

public final class TermFloat extends NumericTerm {

  private final float floatValue;

  TermFloat(final String name) {
    super(name);

    int len = name.length() - 1;
    while (len >= 0) {
      if (Character.isWhitespace(name.charAt(len--))) {
        throw new NumberFormatException();
      }
    }

    floatValue = Float.parseFloat(name);
  }

  TermFloat(final float value) {
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
  public boolean stronglyEqualsTo(final Term term) {
    return this == term || (term.getClass() == TermFloat.class && Float.compare(this.floatValue, ((TermFloat) term).floatValue) == 0);
  }

  @Override
  public boolean dryUnifyTo(Term atom) {
    if (this == atom) {
      return true;
    }

    if (atom.getTermType() == VAR) {
      atom = ((Var) atom).getValue();
    }

    if (atom == null) {
      return true;
    }

    if (atom.getTermType() == ATOM && atom instanceof NumericTerm) {
      return compare((NumericTerm) atom) == 0;
    }
    return false;
  }

  @Override
  public boolean unifyTo(Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case ATOM: {
        if (atom instanceof NumericTerm) {
          return compare((NumericTerm) atom) == 0;
        } else {
          return getText().equals(atom.getText());
        }
      }
      case VAR: {
        final Var var = (Var) atom;
        final Term value = var.getValue();
        if (value == null) {
          return ((Var) atom).setValue(this);
        } else {
          return unifyTo(value);
        }
      }
    }
    return false;
  }

  @Override
  public Number toNumber() {
    return this.floatValue;
  }

  @Override
  public String forWrite() {
    return getText().isEmpty() ? Float.toString(floatValue) : getText();
  }

  @Override
  public String toSrcString() {
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
    } else {
      return value;
    }
  }

  @Override
  public int compare(final NumericTerm atom) {
    final float value = atom.toNumber().floatValue();
    return Float.compare(floatValue, value);
  }

  @Override
  public NumericTerm add(final NumericTerm atom) {
    final float value = atom.toNumber().floatValue();
    return newFloat(floatValue + value);
  }

  @Override
  public NumericTerm sub(final NumericTerm atom) {
    final float value = atom.toNumber().floatValue();
    return newFloat(floatValue - value);
  }

  @Override
  public NumericTerm div(final NumericTerm atom) {
    final float value = atom.toNumber().floatValue();
    return newFloat(floatValue / value);
  }

  @Override
  public NumericTerm mul(final NumericTerm atom) {
    final float value = atom.toNumber().floatValue();
    return newFloat(floatValue * value);
  }

  @Override
  public NumericTerm neg() {
    return newFloat(-floatValue);
  }

  @Override
  public boolean isFloat() {
    return true;
  }

  @Override
  public int compareTermTo(Term atom) {
    if (this == atom) {
      return 0;
    }

    atom = atom.findNonVarOrDefault(atom);

    switch (atom.getTermType()) {
      case VAR:
        return 1;
      case ATOM: {
        if (atom instanceof NumericTerm) {
          NumericTerm num = (NumericTerm) atom;
          float value = num.toNumber().floatValue();
          return Float.compare(floatValue, value);
        } else {
          return -1;
        }
      }
      default:
        return -1;
    }
  }

  @Override
  public NumericTerm abs() {
    if (floatValue >= 0) {
      return this;
    }
    return newFloat(Math.abs(floatValue));
  }

  @Override
  public NumericTerm sign() {
    return newFloat(Math.signum(floatValue));
  }
}
