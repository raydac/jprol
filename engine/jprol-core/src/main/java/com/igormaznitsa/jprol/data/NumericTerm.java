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

import static com.igormaznitsa.jprol.data.TermType.VAR;

public abstract class NumericTerm extends Term {

  NumericTerm(final String text, final Object payload, final SourcePosition sourcePosition) {
    super(text, payload, sourcePosition);
  }

  public abstract int compare(NumericTerm term);

  public abstract NumericTerm add(NumericTerm term);

  public abstract NumericTerm sub(NumericTerm term);

  public abstract NumericTerm div(NumericTerm term);

  public abstract NumericTerm mul(NumericTerm term);

  public abstract NumericTerm neg();

  public abstract NumericTerm abs();

  public abstract NumericTerm sign();

  public abstract boolean isDouble();

  @Override
  public final String forWrite() {
    return this.getText();
  }

  @Override
  public final boolean isUnifiableWith(Term target) {
    if (this == target) {
      return true;
    }

    if (target.getTermType() == VAR) {
      if (target.isUnground()) {
        return true;
      } else {
        target = target.tryGround();
      }
    }

    if (target instanceof NumericTerm) {
      return this.compare((NumericTerm) target) == 0;
    }
    return false;
  }

  @Override
  public final boolean unifyWith(Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case ATOM: {
        if (atom.getClass() == this.getClass()) {
          return this.compare((NumericTerm) atom) == 0;
        } else {
          return false;
        }
      }
      case VAR: {
        final TermVar ungrounded = ((TermVar) atom).findUngroundVariable();

        if (ungrounded == null) {
          return this.unifyWith(atom.tryGround());
        } else {
          ungrounded.setImmediateValue(this);
          return true;
        }
      }
    }
    return false;
  }

}
