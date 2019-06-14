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

import static com.igormaznitsa.prol.data.TermType.OPERATORS;

public final class OperatorContainer extends Term {

  private Operator opFZ;
  private Operator opZF;
  private Operator opZFZ;
  private int numberAtContainer;

  private OperatorContainer(final OperatorContainer etalon) {
    super(etalon.getText());
    opFZ = etalon.opFZ;
    opZF = etalon.opZF;
    opZFZ = etalon.opZFZ;
    numberAtContainer = etalon.numberAtContainer;
  }

  public OperatorContainer(final Operator operator) {
    super(operator.getText());
    setOperator(operator);
  }

  public boolean setOperator(final Operator operator) {
    switch (operator.getOperatorType()) {
      case FX:
      case FY: {
        if (opFZ != null) {
          return false;
        }
        opFZ = operator;
        numberAtContainer++;
      }
      break;
      case XF:
      case YF: {
        if (opZF != null) {
          return false;
        }
        opZF = operator;
        numberAtContainer++;
      }
      break;
      case XFX:
      case XFY:
      case YFX: {
        if (opZFZ != null) {
          return false;
        }
        opZFZ = operator;
        numberAtContainer++;
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

  public int size() {
    return numberAtContainer;
  }

  public Operator getOperatorIfSingle() {
    if (numberAtContainer == 1) {
      if (opZFZ != null) {
        return opZFZ;
      }
      if (opFZ != null) {
        return opFZ;
      }
      return opZF;
    }

    return null;
  }

  public Operator getCompatibleOperator(final boolean leftPresented, final boolean rightPresented) {
    if (leftPresented && rightPresented) {
      if (opZFZ != null) {
        return opZFZ;
      }
      if (opFZ != null) {
        return opFZ;
      }
      return opZF;
    }
    if (leftPresented && !rightPresented) {
      if (opZF != null) {
        return opZF;
      }
      return opFZ;
    }
    if (!leftPresented && rightPresented) {
      if (opFZ != null) {
        return opFZ;
      }
      return opZF;
    }
    return null;
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

  public Operator getForTypePrecisely(final OpAssoc type) {
    Operator result = null;
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
          result = true;
        }
      }
      break;
      case XF:
      case YF: {
        if (opZF != null && opZF.getOperatorType() == type) {
          opZF = null;
          result = true;
        }
      }
      break;
      case XFX:
      case YFX:
      case XFY: {
        if (opZFZ != null && opZFZ.getOperatorType() == type) {
          opZFZ = null;
          result = true;
        }
      }
      break;
      default:
        return false;
    }
    return result;
  }

  public OperatorContainer makeCopy() {
    return new OperatorContainer(this);
  }
}
