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

import com.igormaznitsa.prol.data.*;
import java.io.PrintWriter;

/**
 * The class implements an operator contaier, i.e. the object contains all
 * operators of a knowledge base which have the same names but different types
 * and priorities
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class OperatorContainer extends Term {

  /**
   * The variable contains FZ operator (i.e. fx,fy)
   */
  private Operator opFZ;
  /**
   * The variable contains ZF operator (i.e. xf,yf)
   */
  private Operator opZF;
  /**
   * The variable contains ZF operator (i.e. xfx,yfx,xfy)
   */
  private Operator opZFZ;
  /**
   * The counter of operators saved by the contaier
   */
  private int numberAtContainer;
  /**
   * The flag shows that it is a system operator and can't be removed or changed
   * in runtime (as you can see it works foe all operators with the same name)
   */
  private boolean system;

  /**
   * Inside auxiliary constructor to make new container as copy of an existent
   * container
   *
   * @param etalon the etalon
   */
  private OperatorContainer(final OperatorContainer etalon) {
    super(etalon.getText());
    opFZ = etalon.opFZ;
    opZF = etalon.opZF;
    opZFZ = etalon.opZFZ;
    numberAtContainer = etalon.numberAtContainer;
    system = etalon.system;
  }

  /**
   * A constructor
   *
   * @param operator an operator as the ground for the container
   */
  public OperatorContainer(final Operator operator) {
    this(operator, false);
  }

  /**
   * A constructor
   *
   * @param operator an operator as the ground for the container
   * @param systemOperator if true, the operator container will be signed as a
   * system operator
   */
  public OperatorContainer(final Operator operator, final boolean systemOperator) {
    super(operator.getText());
    setOperator(operator);
    this.system = systemOperator;
  }

  /**
   * Chack that the container has system operators
   *
   * @return true if the container contains system operators else false
   */
  public boolean isSystem() {
    return system;
  }

  /**
   * Add an operator in the container
   *
   * @param operator an operator to be added into the container
   * @return true if the operator has been added, else false
   */
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
  public int getTermType() {
    return TYPE_OPERATORS;
  }

  /**
   * Return the number of operators in the container
   *
   * @return the operator number as integer
   */
  public int size() {
    return numberAtContainer;
  }

  /**
   * Get an operator from the container if the operator is the only operator
   * here
   *
   * @return an operator if it is only operator else null
   */
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

  /**
   * Get an operator from the container which can be used for situation desribed
   * by arguments
   *
   * @param leftPresented true if thee is the left argument of the operator,
   * false if there is not any
   * @param rightPresented false if thee is the right argument of the operator,
   * false if there is not any
   * @return found operator or null if not found
   */
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

  /**
   * Write operators into a writter
   *
   * @param writer the writter to out operator definitions
   */
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

  /**
   * Get a saved operator for its type
   *
   * @param type the operator type
   * @return the found operator or null
   */
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

  /**
   * Get a saved operator for its family type
   *
   * @param type the family type
   * @return the found operator or null
   */
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

  /**
   * Remove an operator from the container for its type
   *
   * @param type the operator type
   * @return true if the operator was found and removed, else false
   */
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

  /**
   * Make copy of the operator container
   *
   * @return the copy of the container
   */
  public OperatorContainer makeCopy() {
    return new OperatorContainer(this);
  }
}
