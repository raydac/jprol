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

public final class TermDouble extends NumericTerm {

  private final Double value;

  TermDouble(final String name) {
    super(name);

    int len = name.length() - 1;
    while (len >= 0) {
      if (Character.isWhitespace(name.charAt(len--))) {
        throw new NumberFormatException();
      }
    }

    value = Double.parseDouble(name);
  }

  TermDouble(final double value) {
    super("");
    this.value = value;
  }

  @Override
  public String toString() {
    return getText();
  }

  @Override
  public int hashCode() {
    return this.value.hashCode();
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }

    if (obj.getClass() == TermDouble.class) {
      return ((TermDouble) obj).value == value;
    }

    return super.equals(obj);
  }

  @Override
  public Number toNumber() {
    return this.value;
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
    return Double.toString(this.value);
  }

  @Override
  public int compare(final NumericTerm atom) {
    final double value = atom.toNumber().doubleValue();
    return Double.compare(this.value, value);
  }

  @Override
  public NumericTerm add(final NumericTerm atom) {
    final double value = atom.toNumber().doubleValue();
    return Terms.newDouble(this.value + value);
  }

  @Override
  public NumericTerm sub(final NumericTerm atom) {
    final double value = atom.toNumber().doubleValue();
    return Terms.newDouble(this.value - value);
  }

  @Override
  public NumericTerm div(final NumericTerm atom) {
    final double value = atom.toNumber().doubleValue();
    return Terms.newDouble(this.value / value);
  }

  @Override
  public NumericTerm mul(final NumericTerm atom) {
    final double value = atom.toNumber().doubleValue();
    return Terms.newDouble(this.value * value);
  }

  @Override
  public NumericTerm neg() {
    return Terms.newDouble(-this.value);
  }

  @Override
  public boolean isDouble() {
    return true;
  }

  @Override
  public NumericTerm abs() {
    if (Double.compare(this.value, 0.0d) >= 0) {
      return this;
    }
    return Terms.newDouble(Math.abs(value));
  }

  @Override
  public NumericTerm sign() {
    return Terms.newLong(Double.compare(this.value, 0.0d));
  }
}
