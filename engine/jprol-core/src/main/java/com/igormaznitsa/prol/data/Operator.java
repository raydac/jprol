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

import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.PrintWriter;

import static com.igormaznitsa.prol.data.TermType.OPERATOR;

public final class Operator extends Term {

  public static final int PRIORITY_MAX = 0;
  public static final int PRIORITY_MIN = 1200;

  private final OpAssoc opType;
  private final int opPriority;
  private final int precalculatedHashCode;
  private final String signature;

  public Operator(final int priority, final OpAssoc type, final String name) {
    super(name);

    if (priority < PRIORITY_MAX || priority > PRIORITY_MIN) {
      throw new IllegalArgumentException("Wrong priority value");
    }

    switch (type) {
      case FX:
      case FY:
      case XF:
      case YF: {
        signature = name + "/1";
      }
      break;
      case YFX:
      case XFY:
      case XFX: {
        signature = name + "/2";
      }
      break;
      default:
        throw new IllegalArgumentException("Wrong operator type");
    }

    opType = type;
    opPriority = priority;

    int hash = name.hashCode();
    hash = 89 * hash + this.opType.hashCode();
    hash = 89 * hash + this.opPriority;
    precalculatedHashCode = hash;
  }

  public static Operator[] makeOperators(final int priority, final OpAssoc type, final String[] names) {
    final Operator[] result = new Operator[names.length];
    for (int li = 0; li < names.length; li++) {
      result[li] = new Operator(priority, type, names[li]);
    }
    return result;
  }

  @Override
  public TermType getTermType() {
    return OPERATOR;
  }

  public OpAssoc getOperatorType() {
    return opType;
  }

  @Override
  public int getPriority() {
    return opPriority;
  }

  @Override
  public int hashCode() {
    return precalculatedHashCode;
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }

    final Operator other = (Operator) obj;

    if (this.precalculatedHashCode != other.precalculatedHashCode) {
      return false;
    }

    if (this.opType != other.opType) {
      return false;
    }

    if (this.opPriority != other.opPriority) {
      return false;
    }

    return this.getText().equals(other.getText());
  }

  @Override
  public String toSrcString() {
    return getText();
  }

  @Override
  public String toString() {
    return getText();
  }

  public String getTypeAsString() {
    return this.opType.getText();
  }

  public void write(final PrintWriter writer) {
    if (writer == null) {
      throw new NullPointerException("Writer is null");
    }
    writer.print(":- op(");
    writer.print(opPriority);
    writer.print(',');
    writer.print(getTypeAsString());
    writer.print(",\'");
    writer.print(getText());
    writer.println("\').");
  }

  @Override
  public String getSignature() {
    return signature;
  }

  @Override
  public String forWrite() {
    return getText();
  }

  @Override
  public boolean unifyTo(final Term atom) {
    if (this == atom) {
      return true;
    }

    final boolean result;

    switch (atom.getTermType()) {
      case OPERATOR:
      case ATOM: {
        result = getText().equals(atom.getText());
      }
      break;
      case VAR: {
        final Var var = (Var) atom;
        final Term value = var.getValue();
        if (value == null) {
          result = ((Var) atom).setValue(this);
        } else {
          result = unifyTo(value);
        }
      }
      break;
      default:
        result = false;
        break;
    }
    return result;
  }

  @Override
  public boolean dryUnifyTo(final Term atom) {
    if (this == atom) {
      return true;
    }

    final boolean result;

    switch (atom.getTermType()) {
      case OPERATOR:
      case ATOM: {
        result = getText().equals(atom.getText());
      }
      break;
      case VAR: {
        final Var var = (Var) atom;
        final Term value = var.getValue();
        result = value == null || this.dryUnifyTo(value);
      }
      break;
      default:
        result = false;
        break;
    }
    return result;
  }
}
