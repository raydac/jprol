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

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.OPERATOR;
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static java.util.Objects.requireNonNull;


import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.jprol.utils.Utils;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class TermStruct extends CompoundTerm {

  static final Term[] EMPTY_ARRAY = new Term[0];
  final Term[] terms;
  final Term functor;
  private final String structureSignature;
  private volatile PredicateInvoker predicateProcessor;

  TermStruct(final Term functor) {
    this(functor, EMPTY_ARRAY);
  }

  TermStruct(final String functor, final Term[] elements) {
    this(new Term(functor), elements);
  }

  TermStruct(final Term functor, final Term[] elements) {
    super(functor.getText());
    this.functor = functor;
    this.terms = elements == null ? EMPTY_ARRAY : elements;
    this.structureSignature = functor.getText() + '/' + getArity();
    this.predicateProcessor = PredicateInvoker.NULL_PROCESSOR;
  }

  TermStruct(final Term functor, final Term[] elements, final PredicateInvoker processor) {
    this(functor, elements);
    this.predicateProcessor = requireNonNull(processor);
  }

  public final Term getFunctor() {
    return functor;
  }

  public final Term[] getElementArray() {
    return this.terms;
  }

  public void setElement(final int index, final Term element) {
    this.terms[index] = element;
  }

  @Override
  public Stream<TermVar> variables() {
    return this.stream().flatMap(Term::variables);
  }

  @Override
  public Stream<Term> stream() {
    return Stream.concat(Stream.of(this.functor), Arrays.stream(this.terms).flatMap(Term::stream));
  }

  @SuppressWarnings("unchecked")
  public <T extends Term> T getElement(final int index) {
    return (T) this.terms[index];
  }

  public final int getArity() {
    return terms.length;
  }

  public boolean isClause() {
    final int arity = this.getArity();
    return (arity > 0 && arity < 3) && ":-".equals(this.functor.getText());
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
  public String toSrcString() {
    return getStringRepresentation(true);
  }

  private String getStringRepresentation(final boolean sourceLike) {
    if (functor.getTermType() != OPERATOR) {
      // just struct
      final StringBuilder buffer = new StringBuilder(Utils.escapeSrc(getText()));

      if (getArity() != 0) {
        buffer.append('(');
        for (int li = 0; li < getArity(); li++) {
          if (li > 0) {
            buffer.append(',');
          }
          buffer.append(sourceLike ? getElement(li).toSrcString() : getElement(li).toString());
        }
        buffer.append(')');
      }

      return buffer.toString();
    } else {
      // it's an operator
      final String opName = sourceLike ? functor.toSrcString() : functor.toString();
      final StringBuilder builder = new StringBuilder();

      final TermOperator OperatorFunctor = (TermOperator) functor;

      final int priority = OperatorFunctor.getPriority();

      final Term arg1 = this.getElement(0);
      final Term arg2 = getArity() > 1 ? this.getElement(1) : null;

      switch (OperatorFunctor.getOperatorType()) {
        case FX: {
          builder.append(opName);
          builder.append(' ');

          final String text = sourceLike ? arg1.toSrcString() : arg1.toString();

          if (arg1.getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }
        }
        break;
        case FY: {
          builder.append(opName);
          builder.append(' ');

          final String text = sourceLike ? arg1.toSrcString() : arg1.toString();

          if (arg1.getPriority() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }
        }
        break;
        case XF: {
          final String text = sourceLike ? arg1.toSrcString() : arg1.toString();

          if (arg1.getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
        }
        break;
        case YF: {
          final String text = sourceLike ? arg1.toSrcString() : arg1.toString();

          if (arg1.getPriority() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
        }
        break;
        case XFX: {
          final String text = sourceLike ? arg1.toSrcString() : arg1.toString();
          final String text2 = sourceLike ? arg2.toSrcString() : arg2.toString();

          if (arg1.getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (arg2.getPriority() >= priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        case YFX: {
          final String text = sourceLike ? arg1.toSrcString() : arg1.toString();
          final String text2 = sourceLike ? arg2.toSrcString() : arg2.toString();

          if (arg1.getPriority() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (arg2.getPriority() >= priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        case XFY: {
          final String text = sourceLike ? arg1.toSrcString() : arg1.toString();
          final String text2 = sourceLike ? arg2.toSrcString() : arg2.toString();

          if (arg1.getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (arg2.getPriority() > priority) {
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

      final TermOperator OperatorFunctor = (TermOperator) functor;

      switch (OperatorFunctor.getOperatorType()) {
        case FX:
        case FY: {
          builder.append(opName);
          builder.append(' ');
          builder.append(getElement(0).forWrite());
        }
        break;
        case XF:
        case YF: {
          builder.append(getElement(0).forWrite());
          builder.append(' ');
          builder.append(opName);
        }
        break;
        case XFX:
        case YFX:
        case XFY: {
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

  public final PredicateInvoker getPredicateProcessor() {
    return this.predicateProcessor;
  }

  public void setPredicateProcessor(final PredicateInvoker processor) {
    this.predicateProcessor = requireNonNull(processor);
  }

  @Override
  public boolean isGround() {
    return this.getArity() == 0 || this.stream().allMatch(Term::isGround);
  }

  @Override
  public TermType getTermType() {
    return STRUCT;
  }

  @Override
  public String getSignature() {
    return this.structureSignature;
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

        if (arity == thatStruct.getArity() &&
            thisStruct.getFunctor().unifyTo(thatStruct.getFunctor())) {
          for (int li = 0; li < arity; li++) {
            final Term thisElement = thisStruct.getElement(li);
            final Term thatElement = thatStruct.getElement(li);
            if (thisElement != thatElement && !thisElement.unifyTo(thatElement)) {
              return false;
            }
          }
          return true;
        }
      }
      break;
      case VAR: {
        final TermVar var = (TermVar) atom;
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
      case ATOM: {
        return this.getArity() == 0 && this.getFunctor().getText().equals(atom.getText());
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
      atom = ((TermVar) atom).getValue();
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

      if (arity == thatStruct.getArity() &&
          thisStruct.getFunctor().dryUnifyTo(thatStruct.getFunctor())) {
        for (int li = 0; li < arity; li++) {
          if (!thisStruct.getElement(li).dryUnifyTo(thatStruct.getElement(li))) {
            return false;
          }
        }
        return true;
      }
    } else if (this.getArity() == 0 && atom.getTermType() == ATOM) {
      return this.getFunctor().getText().equals(atom.getText());
    }
    return false;
  }

  public Term makeClone() {
    return this.getArity() == 0 ? this : this.doMakeClone(new HashMap<>());
  }

  @Override
  protected Term makeCloneAndVarBound(final Map<Integer, TermVar> vars) {
    final Term result;
    if (this.getArity() == 0) {
      result = this;
    } else {
      final Term[] elements = this.getElementArray();
      final int arity = elements.length;
      final Term[] destElements = new Term[arity];

      for (int li = 0; li < arity; li++) {
        final Term element = elements[li];
        destElements[li] = element.makeCloneAndVarBound(vars);
      }
      result = Terms.newStruct(this.getFunctor(), destElements, this.getPredicateProcessor());
    }
    return result;
  }

  @Override
  protected void doArrangeVars(final Map<String, TermVar> variables) {
    final TermStruct struct = this;
    final int arity = struct.getArity();

    for (int li = 0; li < arity; li++) {
      final Term element = struct.getElement(li);
      if (element.getTermType() == VAR) {
        final TermVar thatVar = (TermVar) element;
        final String variableName = thatVar.getText();
        if (!thatVar.isAnonymous() && thatVar.isFree()) {
          final TermVar var = variables.get(variableName);
          if (var == null) {
            variables.put(variableName, (TermVar) element);
          } else {
            struct.setElement(li, var);
          }
        }
      } else {
        element.doArrangeVars(variables);
      }
    }
  }


  @Override
  protected Term doMakeClone(Map<Integer, TermVar> vars) {
    final Term result;
    if (this.getArity() == 0) {
      result = this;
    } else {
      final Term[] elements = this.getElementArray();
      final int arity = elements.length;
      final Term[] destElements = new Term[arity];

      for (int li = 0; li < arity; li++) {
        final Term element = elements[li];
        destElements[li] = element.doMakeClone(vars);
      }
      result = Terms.newStruct(this.getFunctor(), destElements, this.getPredicateProcessor());
    }
    return result;
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
