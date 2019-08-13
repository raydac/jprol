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

import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import static com.igormaznitsa.jprol.data.TermType.OPERATORS;

public final class TermOperatorContainer extends Term {

  private final OpContainer opContainer;
  private volatile TermOperator opFZ;
  private volatile TermOperator opZF;
  private volatile TermOperator opZFZ;

  private TermOperatorContainer(final TermOperatorContainer etalon) {
    super(etalon.getText());
    this.opContainer = OpContainer.make(etalon.getText(),
        etalon.opFZ == null ? null : etalon.opFZ.asOp(),
        etalon.opZF == null ? null : etalon.opZF.asOp(),
        etalon.opZFZ == null ? null : etalon.opZFZ.asOp());
    opFZ = etalon.opFZ;
    opZF = etalon.opZF;
    opZFZ = etalon.opZFZ;
  }

  public TermOperatorContainer(final TermOperator operator) {
    super(operator.getText());
    this.opContainer = OpContainer.make(operator.asOp());
    setOperator(operator);
  }

  public OpContainer asOpContainer() {
    return this.opContainer;
  }

  public boolean setOperator(final TermOperator operator) {
    switch (operator.getOperatorType()) {
      case FX:
      case FY: {
        if (opFZ != null) {
          return false;
        }
        opFZ = operator;
        this.opContainer.add(operator.asOp());
      }
      break;
      case XF:
      case YF: {
        if (opZF != null) {
          return false;
        }
        opZF = operator;
        this.opContainer.add(operator.asOp());
      }
      break;
      case XFX:
      case XFY:
      case YFX: {
        if (opZFZ != null) {
          return false;
        }
        opZFZ = operator;
        this.opContainer.add(operator.asOp());
      }
      break;
      default: {
        throw new Error("Unsupported operator type");
      }
    }
    return true;
  }

  @Override
  public TermType getTermType() {
    return OPERATORS;
  }

  public void write(final PrintWriter writer) {
    if (opFZ != null) {
      opFZ.write(writer);
    }
    if (opZF != null) {
      opZF.write(writer);
    }
    if (opZFZ != null) {
      opZFZ.write(writer);
    }
  }

  public TermOperator getForTypePrecisely(final OpAssoc type) {
    TermOperator result = null;
    switch (type) {
      case FY:
      case FX: {
        if (opFZ != null) {
          result = opFZ;
        }
      }
      break;
      case XF:
      case YF: {
        if (opZF != null) {
          result = opZF;
        }
      }
      break;
      case XFX:
      case YFX:
      case XFY: {
        if (opZFZ != null) {
          result = opZFZ;
        }
      }
      break;
      default: {
        throw new Error("Unsupported operator type");
      }
    }

    if (result != null && result.getOperatorType() == type) {
      return result;
    }
    return null;
  }

  public boolean removeOperatorForType(final OpAssoc type) {
    boolean result = false;
    switch (type) {
      case FX:
      case FY: {
        if (opFZ != null && opFZ.getOperatorType() == type) {
          opFZ = null;
          this.opContainer.removeForType(type);
          result = true;
        }
      }
      break;
      case XF:
      case YF: {
        if (opZF != null && opZF.getOperatorType() == type) {
          opZF = null;
          this.opContainer.removeForType(type);
          result = true;
        }
      }
      break;
      case XFX:
      case YFX:
      case XFY: {
        if (opZFZ != null && opZFZ.getOperatorType() == type) {
          opZFZ = null;
          this.opContainer.removeForType(type);
          result = true;
        }
      }
      break;
      default:
        return false;
    }
    return result;
  }

  public List<TermOperator> toList() {
    final List<TermOperator> result = new ArrayList<>(3);
    final TermOperator fz = this.opFZ;
    final TermOperator zfz = this.opZFZ;
    final TermOperator zf = this.opZF;
    if (fz != null) {
      result.add(fz);
    }
    if (zfz != null) {
      result.add(zfz);
    }
    if (zf != null) {
      result.add(zf);
    }
    return result;
  }

  public TermOperatorContainer makeCopy() {
    return new TermOperatorContainer(this);
  }
}
