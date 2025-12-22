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

import static com.igormaznitsa.jprol.data.TermType.OPERATORS;

import com.igormaznitsa.prologparser.terms.OpContainer;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

public final class TermOperatorContainer extends SpecialTerm {

  private final TermOperator opFZ;
  private final TermOperator opZF;
  private final TermOperator opZFZ;

  private final OpContainer opContainer;

  private TermOperatorContainer(
      final String text,
      final TermOperator opFZ,
      final TermOperator opZF,
      final TermOperator opZFZ,
      final SourcePosition sourcePosition) {
    super(text, sourcePosition);
    this.opFZ = opFZ;
    this.opZF = opZF;
    this.opZFZ = opZFZ;
    this.opContainer = OpContainer.make(text,
        opFZ == null ? null : opFZ.asOperator(),
        opZF == null ? null : opZF.asOperator(),
        opZFZ == null ? null : opZFZ.asOperator());
  }

  public static TermOperatorContainer makeFor(final TermOperator operator,
                                              final SourcePosition sourcePosition) {
    switch (operator.getType()) {
      case FX:
      case FY:
        return new TermOperatorContainer(operator.getText(), operator, null, null, sourcePosition);
      case XF:
      case YF:
        return new TermOperatorContainer(operator.getText(), null, operator, null, sourcePosition);
      case XFX:
      case XFY:
      case YFX:
        return new TermOperatorContainer(operator.getText(), null, null, operator, sourcePosition);
      default:
        throw new Error("Unexpected type, contact developer");
    }
  }

  public TermOperatorContainer makeFor(final TermOperator operator) {
    switch (operator.getType()) {
      case FX:
      case FY: {
        if (this.opFZ == null) {
          return new TermOperatorContainer(this.getText(), operator, this.opZF, this.opZFZ,
              this.getSourcePosition());
        } else {
          if (this.opFZ.equals(operator)) {
            return this;
          } else {
            return null;
          }
        }
      }
      case XF:
      case YF: {
        if (this.opZF == null) {
          return new TermOperatorContainer(this.getText(), this.opFZ, operator, this.opZFZ,
              this.getSourcePosition());
        } else {
          if (this.opZF.equals(operator)) {
            return this;
          } else {
            return null;
          }
        }
      }
      case XFX:
      case XFY:
      case YFX: {
        if (this.opZFZ == null) {
          return new TermOperatorContainer(this.getText(), this.opFZ, this.opZF, operator,
              this.getSourcePosition());
        } else {
          if (this.opZFZ.equals(operator)) {
            return this;
          } else {
            return null;
          }
        }
      }
      default: {
        throw new Error("Unexpected operator type, contact developer");
      }
    }

  }

  @Override
  public TermType getTermType() {
    return OPERATORS;
  }

  public void write(final PrintWriter writer) {
    if (this.opFZ != null) {
      this.opFZ.write(writer);
    }
    if (this.opZF != null) {
      this.opZF.write(writer);
    }
    if (this.opZFZ != null) {
      this.opZFZ.write(writer);
    }
  }

  public TermOperator getForExactType(final OpAssoc type) {
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

    if (result != null && result.getType() == type) {
      return result;
    }
    return null;
  }

  public TermOperatorContainer removeType(final OpAssoc type) {
    switch (type) {
      case FX:
      case FY:
        if (this.opFZ == null) {
          return this;
        } else {
          return new TermOperatorContainer(this.getText(), null, this.opZF, this.opZFZ,
              this.getSourcePosition());
        }
      case XF:
      case YF:
        if (this.opZF == null) {
          return this;
        } else {
          return new TermOperatorContainer(this.getText(), this.opFZ, null, this.opZFZ,
              this.getSourcePosition());
        }
      case YFX:
      case XFY:
      case XFX:
        if (this.opZFZ == null) {
          return this;
        } else {
          return new TermOperatorContainer(this.getText(), this.opFZ, this.opZF, null,
              this.getSourcePosition());
        }
      default:
        throw new Error("Illegal type, contact developer");
    }
  }

  public OpContainer asOpContainer() {
    return this.opContainer;
  }

  public List<TermOperator> makeList() {
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

}
