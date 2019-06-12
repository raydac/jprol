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
import static com.igormaznitsa.prol.data.Terms.newInt;

public final class TermInteger extends NumericTerm {

  private final int intValue;

  public TermInteger(final String name) {
    super(name);
    intValue = Integer.parseInt(name);
  }

  public TermInteger(final int value) {
    super("");
    intValue = value;
  }

  @Override
  public String toString() {
    final String val = getText();
    if (val.isEmpty()) {
      return Integer.toString(intValue);
    } else {
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
  public boolean stronglyEqualsTo(final Term term) {
    return this == term || (term.getClass() == TermInteger.class && this.intValue == ((TermInteger) term).intValue);
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
    return this.intValue;
  }

  @Override
  public String toSrcString() {
    return Integer.toString(intValue);
  }

  @Override
  public String getText() {
    final String value = super.getText();
    if (value == null) {
      return Integer.toString(intValue);
    } else {
      return value;
    }
  }

  @Override
  public int compare(final NumericTerm atom) {
    if (atom.isFloat()) {
      final float value = atom.toNumber().floatValue();
      return Float.compare((float) intValue, value);
    }
    return Integer.compare(intValue, atom.toNumber().intValue());
  }

  @Override
  public NumericTerm add(final NumericTerm atom) {
    if (atom.isFloat()) {
      final float value = atom.toNumber().floatValue();
      return newFloat((float) intValue + value);
    } else {
      return newInt(intValue + atom.toNumber().intValue());
    }
  }

  @Override
  public NumericTerm sub(final NumericTerm atom) {
    if (atom.isFloat()) {
      final float value = atom.toNumber().floatValue();
      return newFloat((float) intValue - value);
    } else {
      return newInt(intValue - atom.toNumber().intValue());
    }
  }

  @Override
  public NumericTerm div(final NumericTerm atom) {
    if (atom.isFloat()) {
      final float value = atom.toNumber().floatValue();
      return newFloat((float) intValue / value);
    } else {
      return newInt(intValue / atom.toNumber().intValue());
    }
  }

  @Override
  public NumericTerm mul(final NumericTerm atom) {
    if (atom.isFloat()) {
      final float value = atom.toNumber().floatValue();
      return newFloat((float) intValue * value);
    } else {
      return newInt(intValue * atom.toNumber().intValue());
    }
  }

  @Override
  public NumericTerm neg() {
    return newInt(-intValue);
  }

  @Override
  public boolean isFloat() {
    return false;
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
          final int value = ((NumericTerm) atom).isFloat() ? Math.round(atom.toNumber().floatValue()) :
              atom.toNumber().intValue();
          return Integer.compare(intValue, value);
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
    if (intValue >= 0) {
      return this;
    }
    return newInt(Math.abs(intValue));
  }

  @Override
  public NumericTerm sign() {
    int sign = 0;
    if (intValue < 0) {
      sign = -1;
    } else if (intValue > 0) {
      sign = 1;
    }
    return newInt(sign);
  }
}
