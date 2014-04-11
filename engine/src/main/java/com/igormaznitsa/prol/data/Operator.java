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

import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import java.io.PrintWriter;

/**
 * This class represents a prolog operator
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class Operator extends Term {

  /**
   * The max operator priority constant
   */
  public static final int PRIORITY_MAX = 0;
  /**
   * The min operator priority constant
   */
  public static final int PRIORITY_MIN = 1200;
  /**
   * The constant represents the 'xf' type
   */
  public static final int OPTYPE_XF = 0;
  /**
   * The constant represents the 'yf' type
   */
  public static final int OPTYPE_YF = 1;
  /**
   * The constant represents the 'fx' type
   */
  public static final int OPTYPE_FX = 2;
  /**
   * The constant represents the 'fy' type
   */
  public static final int OPTYPE_FY = 3;
  /**
   * The constant represents the 'xfx' type
   */
  public static final int OPTYPE_XFX = 4;
  /**
   * The constant represents the 'xfy' type
   */
  public static final int OPTYPE_XFY = 5;
  /**
   * The constant represents the 'yfx' type
   */
  public static final int OPTYPE_YFX = 6;
  /**
   * The variable contains the operator type value
   */
  private final int opType;
  /**
   * The variable contains the operator priority value
   */
  private final int opPriority;
  /**
   * The variable contains the precalculated hash code for the operator
   */
  private final int precalculatedHashCode;
  /**
   * The variable contains the operator signature
   */
  private final String signature;

  /**
   * This auxulary function allows to generate a lot of similar operators from a
   * string array
   *
   * @param priority the priority of all created operators
   * @param type the type of all created operators
   * @param names a string array contains names of created operators, must not
   * be null
   * @return an array of Operator objects which were generated from the
   * arguments
   */
  public static Operator[] makeOperators(final int priority, final int type, final String[] names) {
    final Operator[] result = new Operator[names.length];
    for (int li = 0; li < names.length; li++) {
      result[li] = new Operator(priority, type, names[li]);
    }
    return result;
  }

  /**
   * The constructor
   *
   * @param priority the operator priority
   * @param type the operator type
   * @param name the operator name, must not be null
   * @throws java.lang.IllegalArgumentException will be thrown if there is some
   * incompatible value at arguments
   */
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

  @Override
  public int getTermType() {
    return TYPE_OPERATOR;
  }

  /**
   * Get the type of the operator
   *
   * @return the operator type as integer
   */
  public int getOperatorType() {
    return opType;
  }

  @Override
  public int getPriority() {
    return opPriority;
  }

  /**
   * Check that the operator can be functor for the structure
   *
   * @param struct the checked structure, must not be null
   * @return true if the operator is compatible with the structure else false
   */
  public boolean validateStructureForOperator(final TermStruct struct) {
    boolean result = false;
    if (struct != null) {

      switch (struct.getArity()) {
        case 1: {
          switch (opType) {
            case OPTYPE_XFY:
            case OPTYPE_XFX:
            case OPTYPE_YFX: {
              result = false;
            }
            break;

            case OPTYPE_XF:
            case OPTYPE_FX: {
              final Term atom = struct.getElement(0);
              if (atom == null) {
                result = false;
              }
              else {
                result = atom.getPriority() < getPriority();
              }
            }
            break;
            case OPTYPE_YF:
            case OPTYPE_FY: {
              final Term atom = struct.getElement(0);
              if (atom == null) {
                result = false;
              }
              else {
                result = atom.getPriority() <= getPriority();
              }
            }
            break;
            default:
              throw new ProlCriticalError("Unknown type");
          }
        }
        break;

        case 2: {
          switch (opType) {
            case OPTYPE_XFY:
            case OPTYPE_XFX:
            case OPTYPE_YFX: {
              final Term elementLeft = struct.getElement(0);
              final Term elementRight = struct.getElement(1);

              if (elementLeft == null || elementRight == null) {
                result = false;
              }
              else {

                switch (opType) {
                  case OPTYPE_XFX: {
                    result = elementLeft.getPriority() < getPriority() && elementRight.getPriority() < getPriority();
                  }
                  break;
                  case OPTYPE_YFX: {
                    result = elementLeft.getPriority() <= getPriority() && elementRight.getPriority() < getPriority();
                  }
                  break;
                  case OPTYPE_XFY: {
                    result = elementLeft.getPriority() < getPriority() && elementRight.getPriority() <= getPriority();
                  }
                  break;
                  default: {
                    result = false;
                  }
                  break;
                }
              }
            }
            break;

            case OPTYPE_XF:
            case OPTYPE_FX: {
              final Term atom = struct.getElement(opType == OPTYPE_XF ? 0 : 1);
              if (atom == null) {
                result = false;
              }
              else {
                result = atom.getPriority() < getPriority();
              }
            }
            break;
            case OPTYPE_YF:
            case OPTYPE_FY: {
              final Term atom = struct.getElement(opType == OPTYPE_YF ? 0 : 1);
              if (atom == null) {
                result = false;
              }
              else {
                result = atom.getPriority() <= getPriority();
              }
            }
            break;
            default: {
              throw new ProlCriticalError("Unknown type");
            }
          }
        }
        break;
        default: {
          result = false;
        }
      }
    }
    return result;
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
  public String getSourceLikeRepresentation() {
    return getText();
  }

  @Override
  public String toString() {
    return getText();
  }

  /**
   * Decode the operator type to the string representation
   *
   * @return the operator type as String
   */
  public String getTypeAsString() {
    return Operator.getTypeFromIndex(opType);
  }

  /**
   * Write the operator into a writter like prolog source format
   *
   * @param writer the writer, must not be null
   * @throws NullPointerException if the writter is null
   */
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
  @SuppressWarnings("unchecked")
  public boolean Equ(final Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case Term.TYPE_ATOM: {
        return getText().equals(atom.getText());
      }
      case Term.TYPE_OPERATOR: {
        return this == atom;
      }
      case Term.TYPE_VAR: {
        final Var var = (Var) atom;
        final Term value = var.getValue();
        if (value == null) {
          return ((Var) atom).setValue(this);
        }
        else {
          return Equ(value);
        }
      }
    }
    return false;
  }

  /**
   * Decode a type index into its string representation
   *
   * @param index the index to be decoded
   * @return the string representation of the index
   */
  public static final String getTypeFromIndex(final int index) {
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

  /**
   * Decode the string operator type representation into its numeric analogue
   *
   * @param op_type the string operator type representation
   * @return numeric value equals the string value or -1 if it's not an operator
   * type
   */
  public static final int getTypeFromString(final String op_type) {
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
}
