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

import static com.igormaznitsa.jprol.data.Terms.newDouble;
import static com.igormaznitsa.jprol.data.Terms.newLong;

import java.util.Objects;

public final class TermDouble extends NumericTerm {

  private final double value;

  TermDouble(final String name, final SourcePosition sourcePosition) {
    this(name, null, sourcePosition);
  }

  TermDouble(final String name, final Object payload, final SourcePosition sourcePosition) {
    this(parseDoubleWithCheckWhitespace(name), payload, sourcePosition);
  }

  TermDouble(final double value, final SourcePosition sourcePosition) {
    this(value, null, sourcePosition);
  }

  TermDouble(final double value, final Object payload, final SourcePosition sourcePosition) {
    super("", payload, sourcePosition);
    this.value = value;
  }

  private static double parseDoubleWithCheckWhitespace(final String text) {
    int len = text.length() - 1;
    while (len >= 0) {
      if (Character.isWhitespace(text.charAt(len--))) {
        throw new NumberFormatException();
      }
    }
    return Double.parseDouble(text);
  }

  @Override
  public String toString() {
    return getText();
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.value);
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }

    if (obj == this) {
      return true;
    }

    if (obj.getClass() == TermDouble.class) {
      return Double.compare(this.value, ((TermDouble) obj).value) == 0;
    }

    return false;
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
    return newDouble(this.value + value);
  }

  @Override
  public NumericTerm sub(final NumericTerm atom) {
    final double value = atom.toNumber().doubleValue();
    return newDouble(this.value - value);
  }

  @Override
  public NumericTerm div(final NumericTerm atom) {
    final double value = atom.toNumber().doubleValue();
    return newDouble(this.value / value);
  }

  @Override
  public NumericTerm mul(final NumericTerm atom) {
    final double value = atom.toNumber().doubleValue();
    return newDouble(this.value * value);
  }

  @Override
  public NumericTerm neg() {
    return newDouble(-this.value, this.payload);
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
    return newDouble(Math.abs(value), this.payload);
  }

  @Override
  public NumericTerm sign() {
    return newLong(Double.compare(this.value, 0.0d), this.payload);
  }
}
