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
import com.igormaznitsa.prol.libraries.PredicateProcessor;
import com.igormaznitsa.prol.utils.Utils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import static com.igormaznitsa.prol.data.TermType.*;

public class TermStruct extends Term {

  private static final Term[] EMPTY_ARRAY = new Term[0];
  protected final Term[] terms;
  protected final Term functor;
  protected PredicateProcessor predicateProcessor;
  private String structureSignature;
  private boolean rulefunctor;
  private boolean rulefunctorset;

  public TermStruct(final Term functor) {
    this(functor, EMPTY_ARRAY);
  }

  public TermStruct(final String functor, final Term[] elements) {
    this(new Term(functor), elements);
    if (functor.length() != 2) {
      rulefunctor = false;
      rulefunctorset = true;
    }
  }

  public TermStruct(final Term functor, final Term[] elements) {
    super(functor.getText());
    final String functorText = functor.getText();
    if (functorText.length() != 2) {
      rulefunctor = false;
      rulefunctorset = true;
    }

    this.functor = functor;
    this.terms = elements == null ? EMPTY_ARRAY : elements;
  }

  public TermStruct(final Term functor, final Term[] elements, final PredicateProcessor processor) {
    this(functor, elements);
    predicateProcessor = processor;
  }

  public final boolean isFunctorLikeRuleDefinition() {
    if (!rulefunctorset) {
      rulefunctor = ":-".equals(functor.getText());
      rulefunctorset = true;
    }
    return rulefunctor;
  }

  public final Term getFunctor() {
    return functor;
  }

  public final Term[] getElementsAsArray() {
    return terms;
  }

  public final void setElement(final int index, final Term element) {
    terms[index] = element;
  }

  @Override
  public Stream<Var> variables() {
    return this.stream().filter(x -> x.getTermType() == VAR).map(x -> (Var)x);
  }

  @Override
  public Stream<Term> stream() {
    return Stream.concat(Stream.of(this.functor),Arrays.stream(this.terms).flatMap(Term::stream));
  }

  public final Term getElement(final int index) {
    return terms[index];
  }

  public final int getArity() {
    return terms.length;
  }

  @Override
  public int getPriority() {
    if (functor.getTermType() == OPERATOR) {
      return functor.getPriority();
    } else {
      return 0;
    }
  }

  @Override
  public String toString() {
    return getStringRepresentation(false);
  }

  @Override
  public String toSourceString() {
    return getStringRepresentation(true);
  }

  private String getStringRepresentation(final boolean sourceLike) {
    if (functor.getTermType() != OPERATOR) {
      // just struct
      final StringBuilder buffer = new StringBuilder(Utils.encodeTextSourceLike(getText()));

      if (getArity() != 0) {
        buffer.append('(');
        for (int li = 0; li < getArity(); li++) {
          if (li > 0) {
            buffer.append(',');
          }
          buffer.append(sourceLike ? getElement(li).toSourceString() : getElement(li).toString());
        }
        buffer.append(')');
      }

      return buffer.toString();
    } else {
      // it's an operator
      final String opName = sourceLike ? functor.toSourceString() : functor.toString();
      final StringBuilder builder = new StringBuilder();

      final Operator OperatorFunctor = (Operator) functor;

      final int priority = OperatorFunctor.getPriority();

      switch (OperatorFunctor.getOperatorType()) {
        case Operator.OPTYPE_FX: {
          builder.append(opName);
          builder.append(' ');

          final String text = sourceLike ? getElement(0).toSourceString() : getElement(0).toString();

          if (getElement(0).getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }
        }
        break;
        case Operator.OPTYPE_FY: {
          builder.append(opName);
          builder.append(' ');

          final String text = sourceLike ? getElement(0).toSourceString() : getElement(0).toString();

          if (getElement(0).getPriority() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }
        }
        break;
        case Operator.OPTYPE_XF: {
          final String text = sourceLike ? getElement(0).toSourceString() : getElement(0).toString();

          if (getElement(0).getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
        }
        break;
        case Operator.OPTYPE_YF: {
          final String text = sourceLike ? getElement(0).toSourceString() : getElement(0).toString();

          if (getElement(0).getPriority() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
        }
        break;
        case Operator.OPTYPE_XFX: {
          final String text = sourceLike ? getElement(0).toSourceString() : getElement(0).toString();
          final String text2 = sourceLike ? getElement(1).toSourceString() : getElement(1).toString();

          if (getElement(0).getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (getElement(1).getPriority() >= priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        case Operator.OPTYPE_YFX: {
          final String text = sourceLike ? getElement(0).toSourceString() : getElement(0).toString();
          final String text2 = sourceLike ? getElement(1).toSourceString() : getElement(1).toString();

          if (getElement(0).getPriority() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (getElement(1).getPriority() >= priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        case Operator.OPTYPE_XFY: {
          final String text = sourceLike ? getElement(0).toSourceString() : getElement(0).toString();
          final String text2 = sourceLike ? getElement(1).toSourceString() : getElement(1).toString();

          if (getElement(0).getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (getElement(1).getPriority() > priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        default:
          throw new ProlCriticalError("Unsupported type");
      }

      return builder.toString();
    }
  }

  @Override
  public String forWrite() {
    if (functor.getTermType() != OPERATOR) {
      // just struct
      final StringBuilder buffer = new StringBuilder(functor.forWrite());

      if (getArity() > 0) {
        buffer.append('(');
        for (int li = 0; li < getArity(); li++) {
          if (li > 0) {
            buffer.append(',');
          }
          buffer.append(getElement(li).forWrite());
        }
        buffer.append(')');
      }

      return buffer.toString();
    } else {
      // it's an operator
      final String opName = functor.forWrite();
      final StringBuilder builder = new StringBuilder();

      final Operator OperatorFunctor = (Operator) functor;

      switch (OperatorFunctor.getOperatorType()) {
        case Operator.OPTYPE_FX:
        case Operator.OPTYPE_FY: {
          builder.append(opName);
          builder.append(' ');
          builder.append(getElement(0).forWrite());
        }
        break;
        case Operator.OPTYPE_XF:
        case Operator.OPTYPE_YF: {
          builder.append(getElement(0).forWrite());
          builder.append(' ');
          builder.append(opName);
        }
        break;
        case Operator.OPTYPE_XFX:
        case Operator.OPTYPE_YFX:
        case Operator.OPTYPE_XFY: {
          builder.append(getElement(0).forWrite());
          builder.append(' ');
          builder.append(opName);
          builder.append(' ');
          builder.append(getElement(1).forWrite());
        }
        break;
        default:
          throw new ProlCriticalError("Unsupported type");
      }

      return builder.toString();
    }
  }

  public final PredicateProcessor getPredicateProcessor() {
    return predicateProcessor;
  }

  public void setPredicateProcessor(final PredicateProcessor processor) {
    predicateProcessor = processor;
  }

  @Override
  public boolean isAllVariablesInstantiated() {
    return this.terms.length == 0 || this.stream().allMatch(Term::isAllVariablesInstantiated);
  }

  @Override
  public TermType getTermType() {
    return STRUCT;
  }

  @Override
  public String getSignature() {
    if (structureSignature == null) {
      structureSignature = functor.getText() + '/' + getArity();
    }
    return structureSignature;
  }

  @Override
  public boolean unifyTo(final Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case STRUCT: {
        final TermStruct thisStruct = this;
        final TermStruct thatStruct = (TermStruct) atom;

        final int arity = thisStruct.getArity();

        if (arity == thatStruct.getArity() && thisStruct.getFunctor().unifyTo(thatStruct.getFunctor())) {
          for (int li = 0; li < arity; li++) {
            final Term thiselement = thisStruct.getElement(li);
            final Term thatelement = thatStruct.getElement(li);
            if (thiselement != thatelement && !thiselement.unifyTo(thatelement)) {
              return false;
            }
          }
          return true;
        }
      }
      break;
      case VAR: {
        final Var var = (Var) atom;
        final Term value = var.getValue();

        if (value == null) {
          return var.setValue(this);
        } else {
          if (value == this) {
            return true;
          }
          return value.unifyTo(this);
        }
      }
    }
    return false;
  }

  @Override
  public boolean dryUnifyTo(Term atom) {
    if (this == atom) {
      return true;
    }

    if (atom.getTermType() == VAR) {
      atom = ((Var) atom).getValue();
    }

    if (atom == null) {
      return true;
    } else {
      if (atom == this) {
        return true;
      }
    }

    if (atom.getTermType() == STRUCT) {
      final TermStruct thisStruct = this;
      final TermStruct thatStruct = (TermStruct) atom;

      final int arity = thisStruct.getArity();

      if (arity == thatStruct.getArity() && thisStruct.getFunctor().dryUnifyTo(thatStruct.getFunctor())) {
        for (int li = 0; li < arity; li++) {
          if (!thisStruct.getElement(li).dryUnifyTo(thatStruct.getElement(li))) {
            return false;
          }
        }
        return true;
      }
    }
    return false;
  }

  @Override
  public int termComparsion(Term atom) {
    if (this == atom) {
      return 0;
    }

    if (atom.getTermType() == VAR && !((Var) atom).isUndefined()) {
      atom = ((Var) atom).getValue();
    }

    switch (atom.getTermType()) {
      case LIST:
        return -1;
      case STRUCT: {
        final TermStruct thatStruct = (TermStruct) atom;

        final int thisArity = getArity();
        final int thatArity = thatStruct.getArity();

        if (thisArity == thatArity) {
          int result = getFunctor().termComparsion(thatStruct.getFunctor());
          if (result == 0) {
            for (int li = 0; li < thisArity; li++) {
              final Term thisAtom = getElement(li);
              final Term thatAtom = thatStruct.getElement(li);

              result = thisAtom.termComparsion(thatAtom);
              if (result != 0) {
                return result;
              }
            }
            return 0;
          } else {
            return result;
          }
        } else {
          if (thisArity < thatArity) {
            return -1;
          } else {
            return 1;
          }
        }
      }
      default:
        return 1;
    }
  }

  public Term makeClone() {
    return this.getArity() == 0 ? this : this.doMakeClone(new HashMap<>());
  }

  @Override
  protected Term makeCloneWithVarReplacement(final Map<Integer, Var> vars) {
    final Term result;
    if (this.getArity() == 0) {
      result = this;
    } else {
      final Term[] elements = this.getElementsAsArray();
      final int arity = elements.length;
      final Term[] destElements = new Term[arity];

      for (int li = 0; li < arity; li++) {
        final Term element = elements[li];
        destElements[li] = element.makeCloneWithVarReplacement(vars);
      }
      result = new TermStruct(this.getFunctor(), destElements, this.getPredicateProcessor());
    }
    return result;
  }

  @Override
  protected Term doMakeClone(Map<Integer, Var> vars) {
    final Term result;
    if (this.getArity() == 0) {
      result = this;
    } else {
      final Term[] elements = this.getElementsAsArray();
      final int arity = elements.length;
      final Term[] destElements = new Term[arity];

      for (int li = 0; li < arity; li++) {
        final Term element = elements[li];
        destElements[li] = element.doMakeClone(vars);
      }
      result = new TermStruct(this.getFunctor(), destElements, this.getPredicateProcessor());
    }
    return result;
  }

  @Override
  public boolean hasAnyDifference(final Term atom) {
    if (atom.getTermType() != STRUCT) {
      return true;
    }

    final TermStruct thatStruct = (TermStruct) atom;
    if (functor.hasAnyDifference(thatStruct.functor)) {
      return true;
    }
    final int thisarity = getArity();
    final int thatarity = thatStruct.getArity();

    if (thatarity == thisarity) {

      for (int li = 0; li < thisarity; li++) {
        if (terms[li].hasAnyDifference(thatStruct.terms[li])) {
          return true;
        }
      }
    } else {
      return true;
    }

    return false;
  }

  @Override
  public boolean hasVariableWithName(final String name) {
    final int arity = getArity();
    for (int li = 0; li < arity; li++) {
      if (getElement(li).hasVariableWithName(name)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean equals(final Object obj) {
    return this == obj;
  }

  @Override
  public int hashCode() {
    return System.identityHashCode(this);
  }
}
