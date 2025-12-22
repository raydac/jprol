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
import java.util.concurrent.locks.ReentrantLock;

public final class TermOperatorContainer extends SpecialTerm {

  private final OpContainer opContainer;
  private final ReentrantLock locker = new ReentrantLock();
  private TermOperator opFZ;
  private TermOperator opZF;
  private TermOperator opZFZ;

  private TermOperatorContainer(final TermOperatorContainer sample,
                                final SourcePosition sourcePosition) {
    super(sample.getText(), sourcePosition);
    this.opContainer = OpContainer.make(sample.getText(),
        sample.opFZ == null ? null : sample.opFZ.asOperator(),
        sample.opZF == null ? null : sample.opZF.asOperator(),
        sample.opZFZ == null ? null : sample.opZFZ.asOperator());
    opFZ = sample.opFZ;
    opZF = sample.opZF;
    opZFZ = sample.opZFZ;
  }

  public TermOperatorContainer(final TermOperator operator, final SourcePosition sourcePosition) {
    super(operator.getText(), sourcePosition);
    this.opContainer = OpContainer.make(operator.asOperator());
    setOperator(operator);
  }

  public OpContainer asOpContainer() {
    return this.opContainer;
  }

  public boolean setOperator(final TermOperator operator) {
    this.locker.lock();
    try {
      switch (operator.getType()) {
        case FX:
        case FY: {
          if (opFZ != null) {
            return false;
          }
          opFZ = operator;
          this.opContainer.add(operator.asOperator());
        }
        break;
        case XF:
        case YF: {
          if (opZF != null) {
            return false;
          }
          opZF = operator;
          this.opContainer.add(operator.asOperator());
        }
        break;
        case XFX:
        case XFY:
        case YFX: {
          if (opZFZ != null) {
            return false;
          }
          opZFZ = operator;
          this.opContainer.add(operator.asOperator());
        }
        break;
        default: {
          throw new Error("Unsupported operator type");
        }
      }
      return true;
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public TermType getTermType() {
    return OPERATORS;
  }

  public void write(final PrintWriter writer) {
    this.locker.lock();
    try {
      if (opFZ != null) {
        opFZ.write(writer);
      }
      if (opZF != null) {
        opZF.write(writer);
      }
      if (opZFZ != null) {
        opZFZ.write(writer);
      }
    } finally {
      this.locker.unlock();
    }
  }

  public TermOperator getForExactType(final OpAssoc type) {
    this.locker.lock();
    try {
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
    } finally {
      this.locker.unlock();
    }
  }

  public boolean removeForType(final OpAssoc type) {
    this.locker.lock();
    try {
      boolean result = false;
      switch (type) {
        case FX:
        case FY: {
          if (opFZ != null && opFZ.getType() == type) {
            opFZ = null;
            this.opContainer.removeForType(type);
            result = true;
          }
        }
        break;
        case XF:
        case YF: {
          if (opZF != null && opZF.getType() == type) {
            opZF = null;
            this.opContainer.removeForType(type);
            result = true;
          }
        }
        break;
        case XFX:
        case YFX:
        case XFY: {
          if (opZFZ != null && opZFZ.getType() == type) {
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
    } finally {
      this.locker.unlock();
    }
  }

  public List<TermOperator> makeList() {
    this.locker.lock();
    try {
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
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public Term makeClone() {
    return new TermOperatorContainer(this, this.getSourcePosition());
  }

}
