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

public final class TermLong extends NumericTerm {

  private final long value;

  TermLong(final String name, final SourcePosition sourcePosition) {
    this(name, null, sourcePosition);
  }

  TermLong(final String name, final Object payload, final SourcePosition sourcePosition) {
    this(Long.parseLong(name), payload, sourcePosition);
  }

  TermLong(final long value, final SourcePosition sourcePosition) {
    this(value, null, sourcePosition);
  }

  TermLong(final long value, final Object payload, final SourcePosition sourcePosition) {
    super("", payload, sourcePosition);
    this.value = value;
  }

  @Override
  public String toString() {
    final String val = getText();
    if (val.isEmpty()) {
      return Long.toString(value);
    } else {
      return val;
    }
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

    if (obj.getClass() == TermLong.class) {
      final TermLong that = (TermLong) obj;
      return this.value == that.value;
    }

    return false;
  }

  @Override
  public Number toNumber() {
    return this.value;
  }

  @Override
  public String toSrcString() {
    return Long.toString(this.value);
  }

  @Override
  public String getText() {
    return Long.toString(this.value);
  }

  @Override
  public int compare(final NumericTerm atom) {
    if (atom.isDouble()) {
      final double value = atom.toNumber().doubleValue();
      return Double.compare((double) this.value, value);
    }
    return Long.compare(this.value, atom.toNumber().longValue());
  }

  @Override
  public NumericTerm add(final NumericTerm atom) {
    if (atom.isDouble()) {
      final double value = atom.toNumber().doubleValue();
      return newDouble((double) this.value + value);
    } else {
      return newLong(this.value + atom.toNumber().longValue());
    }
  }

  @Override
  public NumericTerm sub(final NumericTerm atom) {
    if (atom.isDouble()) {
      final double value = atom.toNumber().doubleValue();
      return newDouble((double) this.value - value);
    } else {
      return newLong(this.value - atom.toNumber().longValue());
    }
  }

  @Override
  public NumericTerm div(final NumericTerm atom) {
    if (atom.isDouble()) {
      final double value = atom.toNumber().doubleValue();
      return newDouble((double) this.value / value);
    } else {
      return newLong(this.value / atom.toNumber().longValue());
    }
  }

  @Override
  public NumericTerm mul(final NumericTerm atom) {
    if (atom.isDouble()) {
      final double value = atom.toNumber().doubleValue();
      return newDouble((double) this.value * value);
    } else {
      return newLong(this.value * atom.toNumber().longValue());
    }
  }

  @Override
  public NumericTerm neg() {
    return newLong(-this.value, this.payload);
  }

  @Override
  public boolean isDouble() {
    return false;
  }

  @Override
  public NumericTerm abs() {
    if (this.value >= 0L) {
      return this;
    }
    return newLong(Math.abs(this.value), this.payload);
  }

  @Override
  public NumericTerm sign() {
    int sign = 0;
    if (this.value < 0L) {
      sign = -1;
    } else if (this.value > 0L) {
      sign = 1;
    }
    return newLong(sign, this.payload);
  }
}
