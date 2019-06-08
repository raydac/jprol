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

import java.io.PrintWriter;

import static com.igormaznitsa.prol.data.TermType.OPERATOR;

public final class Operator extends Term {

  public static final int PRIORITY_MAX = 0;
  public static final int PRIORITY_MIN = 1200;

  public static final int OPTYPE_XF = 0;
  public static final int OPTYPE_YF = 1;
  public static final int OPTYPE_FX = 2;
  public static final int OPTYPE_FY = 3;
  public static final int OPTYPE_XFX = 4;
  public static final int OPTYPE_XFY = 5;
  public static final int OPTYPE_YFX = 6;

  private final int opType;
  private final int opPriority;
  private final int precalculatedHashCode;
  private final String signature;

  public Operator(final int priority, final int type, final String name) {
    super(name);

    if (priority < PRIORITY_MAX || priority > PRIORITY_MIN) {
      throw new IllegalArgumentException("Wrong priority value");
    }

    switch (type) {
      case OPTYPE_FX:
      case OPTYPE_FY:
      case OPTYPE_XF:
      case OPTYPE_YF: {
        signature = name + "/1";
      }
      break;
      case OPTYPE_YFX:
      case OPTYPE_XFY:
      case OPTYPE_XFX: {
        signature = name + "/2";
      }
      break;
      default:
        throw new IllegalArgumentException("Wrong operator type");
    }

    opType = type;
    opPriority = priority;

    int hash = name.hashCode();
    hash = 89 * hash + this.opType;
    hash = 89 * hash + this.opPriority;
    precalculatedHashCode = hash;
  }

  public static Operator[] makeOperators(final int priority, final int type, final String[] names) {
    final Operator[] result = new Operator[names.length];
    for (int li = 0; li < names.length; li++) {
      result[li] = new Operator(priority, type, names[li]);
    }
    return result;
  }

  public static String getTypeFromIndex(final int index) {
    switch (index) {
      case OPTYPE_FX:
        return "fx";
      case OPTYPE_FY:
        return "fy";
      case OPTYPE_XF:
        return "xf";
      case OPTYPE_YF:
        return "yf";
      case OPTYPE_XFX:
        return "xfx";
      case OPTYPE_XFY:
        return "xfy";
      case OPTYPE_YFX:
        return "yfx";
      default:
        return "<UNKNOWN>";
    }
  }

  public static int getTypeFromString(final String op_type) {
    switch (op_type.length()) {
      case 2: {
        if ("xf".equals(op_type)) {
          return OPTYPE_XF;
        }
        if ("fx".equals(op_type)) {
          return OPTYPE_FX;
        }
        if ("fy".equals(op_type)) {
          return OPTYPE_FY;
        }
        if ("yf".equals(op_type)) {
          return OPTYPE_YF;
        }
        return -1;
      }
      case 3: {
        if ("xfx".equals(op_type)) {
          return OPTYPE_XFX;
        }
        if ("yfx".equals(op_type)) {
          return OPTYPE_YFX;
        }
        if ("xfy".equals(op_type)) {
          return OPTYPE_XFY;
        }
        return -1;
      }
      default:
        return -1;
    }
  }

  @Override
  public TermType getTermType() {
    return OPERATOR;
  }

  public int getOperatorType() {
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
  public String toSourceString() {
    return getText();
  }

  @Override
  public String toString() {
    return getText();
  }

  public String getTypeAsString() {
    return Operator.getTypeFromIndex(opType);
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

    switch (atom.getTermType()) {
      case ATOM: {
        return getText().equals(atom.getText());
      }
      case OPERATOR: {
        return this == atom;
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
}
