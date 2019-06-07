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

package com.igormaznitsa.prol.containers;

import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermType;

import java.io.PrintWriter;

import static com.igormaznitsa.prol.data.TermType.OPERATORS;

public final class OperatorContainer extends Term {

  private final boolean system;
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
    system = etalon.system;
  }

  public OperatorContainer(final Operator operator) {
    this(operator, false);
  }

  public OperatorContainer(final Operator operator, final boolean systemOperator) {
    super(operator.getText());
    setOperator(operator);
    this.system = systemOperator;
  }

  public boolean isSystem() {
    return system;
  }

  public boolean setOperator(final Operator operator) {
    switch (operator.getOperatorType()) {
      case Operator.OPTYPE_FX:
      case Operator.OPTYPE_FY: {
        if (opFZ != null) {
          return false;
        }
        opFZ = operator;
        numberAtContainer++;
      }
      break;
      case Operator.OPTYPE_XF:
      case Operator.OPTYPE_YF: {
        if (opZF != null) {
          return false;
        }
        opZF = operator;
        numberAtContainer++;
      }
      break;
      case Operator.OPTYPE_XFX:
      case Operator.OPTYPE_XFY:
      case Operator.OPTYPE_YFX: {
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

  public Operator getForTypePrecisely(final int type) {
    Operator result = null;
    switch (type) {
      case Operator.OPTYPE_FY:
      case Operator.OPTYPE_FX: {
        if (opFZ != null) {
          result = opFZ;
        }
      }
      break;
      case Operator.OPTYPE_XF:
      case Operator.OPTYPE_YF: {
        if (opZF != null) {
          result = opZF;
        }
      }
      break;
      case Operator.OPTYPE_XFX:
      case Operator.OPTYPE_YFX:
      case Operator.OPTYPE_XFY: {
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

  public Operator getOperatorForTypeFamily(final int type) {
    switch (type) {
      case Operator.OPTYPE_FX:
      case Operator.OPTYPE_FY:
        return opFZ;
      case Operator.OPTYPE_XF:
      case Operator.OPTYPE_YF:
        return opZF;
      case Operator.OPTYPE_XFX:
      case Operator.OPTYPE_YFX:
      case Operator.OPTYPE_XFY:
        return opZFZ;
      default:
        return null;
    }
  }

  public boolean removeOperatorForType(final int type) {
    boolean result = false;
    switch (type) {
      case Operator.OPTYPE_FX:
      case Operator.OPTYPE_FY: {
        if (opFZ != null && opFZ.getOperatorType() == type) {
          opFZ = null;
          result = true;
        }
      }
      break;
      case Operator.OPTYPE_XF:
      case Operator.OPTYPE_YF: {
        if (opZF != null && opZF.getOperatorType() == type) {
          opZF = null;
          result = true;
        }
      }
      break;
      case Operator.OPTYPE_XFX:
      case Operator.OPTYPE_YFX:
      case Operator.OPTYPE_XFY: {
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
