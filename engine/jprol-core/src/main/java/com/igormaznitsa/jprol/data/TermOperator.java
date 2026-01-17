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

import static com.igormaznitsa.jprol.data.TermType.OPERATOR;

import com.igormaznitsa.prologparser.tokenizer.Op;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.PrintWriter;

public final class TermOperator extends Term {

  public static final int PRIORITY_MAX = 0;
  public static final int PRIORITY_MIN = 1200;

  private final Op op;
  private final String signature;

  public TermOperator(final int priority, final OpAssoc type, final String name,
                      final SourcePosition sourcePosition) {
    super(name, sourcePosition);

    if (priority < PRIORITY_MAX || priority > PRIORITY_MIN) {
      throw new IllegalArgumentException("Wrong priority value");
    }

    switch (type) {
      case FX:
      case FY:
      case XF:
      case YF: {
        this.signature = name + "/1";
      }
      break;
      case YFX:
      case XFY:
      case XFX: {
        this.signature = name + "/2";
      }
      break;
      default:
        throw new IllegalArgumentException("Wrong operator type");
    }

    this.op = Op.make(priority, type, name);
  }

  public Op asOperator() {
    return this.op;
  }

  @Override
  public TermType getTermType() {
    return OPERATOR;
  }

  public OpAssoc getType() {
    return this.op.getAssoc();
  }

  @Override
  public int getPrecedence() {
    return this.op.getPrecedence();
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }

    if (this == obj) {
      return true;
    }

    if (obj instanceof TermOperator) {
      final TermOperator that = (TermOperator) obj;
      return this.op.equals(that.op);
    } else if (obj instanceof Term) {
      return this.getText().equals(((Term) obj).getText());
    }

    return false;
  }

  @Override
  public String toSrcString() {
    return this.getText();
  }

  @Override
  public String toString() {
    return this.getText();
  }

  public String getTypeAsString() {
    return this.op.getAssoc().getText();
  }

  public void write(final PrintWriter writer) {
    writer.print(":- op(");
    writer.print(this.op.getPrecedence());
    writer.print(',');
    writer.print(getTypeAsString());
    writer.print(",'");
    writer.print(getText());
    writer.println("').");
  }

  @Override
  public String getSignature() {
    return this.signature;
  }

  @Override
  public boolean unifyWith(final Term atom) {
    if (this == atom) {
      return true;
    }

    final boolean result;

    switch (atom.getTermType()) {
      case OPERATOR:
      case ATOM: {
        result = this.getText().equals(atom.getText());
      }
      break;
      case VAR: {
        final TermVar var = (TermVar) atom;
        final Term value = var.getValue();
        if (value == null) {
          result = ((TermVar) atom).setValue(this);
        } else {
          result = unifyWith(value);
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
  public boolean isUnifiableWith(final Term target) {
    if (this == target) {
      return true;
    }

    final boolean result;

    switch (target.getTermType()) {
      case OPERATOR:
      case ATOM: {
        result = getText().equals(target.getText());
      }
      break;
      case VAR: {
        final TermVar var = (TermVar) target;
        final Term value = var.getValue();
        result = value == null || this.isUnifiableWith(value);
      }
      break;
      default:
        result = false;
        break;
    }
    return result;
  }
}
