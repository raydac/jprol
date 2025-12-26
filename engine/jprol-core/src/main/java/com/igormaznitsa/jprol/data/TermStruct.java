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
import com.igormaznitsa.jprol.utils.ProlUtils;
import com.igormaznitsa.jprol.utils.lazy.LazyMap;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;

public class TermStruct extends CompoundTerm {

  static final Term[] EMPTY_ARRAY = new Term[0];
  final Term[] arguments;
  final Term functor;
  private final String structureSignature;
  private PredicateInvoker predicateProcessor;

  TermStruct(final Term functor, final SourcePosition sourcePosition) {
    this(functor, EMPTY_ARRAY, null, sourcePosition, PredicateInvoker.NULL_PROCESSOR);
  }

  TermStruct(final Term functor, final Object payload, final SourcePosition sourcePosition) {
    this(functor, EMPTY_ARRAY, payload, sourcePosition, PredicateInvoker.NULL_PROCESSOR);
  }

  TermStruct(final String functor, final Term[] arguments, final SourcePosition sourcePosition) {
    this(new Term(functor, sourcePosition), arguments, null, sourcePosition,
        PredicateInvoker.NULL_PROCESSOR);
  }

  TermStruct(final Term functor, final Term[] arguments, final SourcePosition sourcePosition) {
    this(functor, arguments, null, sourcePosition, PredicateInvoker.NULL_PROCESSOR);
  }

  TermStruct(final Term functor, final Term[] arguments, final Object payload,
             final SourcePosition sourcePosition) {
    this(functor, arguments, payload, sourcePosition, PredicateInvoker.NULL_PROCESSOR);
  }

  TermStruct(final Term functor, final Term[] arguments, final Object payload,
             final SourcePosition sourcePosition, final PredicateInvoker predicateInvoker) {
    super(functor.getText(), payload, sourcePosition);
    this.functor = functor;
    this.arguments = arguments == null ? EMPTY_ARRAY : arguments;
    this.structureSignature = functor.getText() + '/' + getArity();
    this.predicateProcessor = predicateInvoker;
  }

  @Override
  public boolean canContainVariables() {
    return true;
  }

  public final Term getFunctor() {
    return this.functor;
  }

  public final Term[] getArguments() {
    return this.arguments;
  }

  public void setArgumentAt(final int index, final Term argument) {
    this.arguments[index] = argument;
  }

  @Override
  public Spliterator<Term> spliteratorChildren() {
    return Spliterators.spliterator(this.arguments, Spliterator.SIZED);
  }

  @Override
  public Iterator<Term> iterator() {
    return new Iterator<>() {
      int index = 0;

      @Override
      public boolean hasNext() {
        return this.index < TermStruct.this.arguments.length;
      }

      @Override
      public Term next() {
        if (this.index < TermStruct.this.arguments.length) {
          return TermStruct.this.arguments[this.index++];
        } else {
          throw new NoSuchElementException();
        }
      }
    };
  }

  @Override
  public Stream<TermVar> variables() {
    return this.stream().flatMap(Term::variables);
  }

  @Override
  public Stream<Term> stream() {
    return Stream.concat(Stream.of(this.functor),
        Arrays.stream(this.arguments).flatMap(Term::stream));
  }

  @SuppressWarnings("unchecked")
  public <T extends Term> T getArgumentAt(final int index) {
    return (T) this.arguments[index];
  }

  public final int getArity() {
    return this.arguments.length;
  }

  public boolean isClause() {
    final int arity = this.getArity();
    return (arity > 0 && arity < 3) && ":-".equals(this.functor.getText());
  }

  @Override
  public int getPrecedence() {
    if (this.functor.getTermType() == OPERATOR) {
      return this.functor.getPrecedence();
    } else {
      return 0;
    }
  }

  @Override
  public String toString() {
    return this.makeStringView(false);
  }

  @Override
  public String toSrcString() {
    return this.makeStringView(true);
  }

  private String makeStringView(final boolean asSources) {
    if (functor.getTermType() != OPERATOR) {
      // just struct
      final StringBuilder buffer = new StringBuilder(ProlUtils.escapeSrc(getText()));

      if (this.getArity() != 0) {
        buffer.append('(');
        for (int i = 0; i < this.getArity(); i++) {
          if (i > 0) {
            buffer.append(',');
          }
          buffer.append(
              asSources ? this.getArgumentAt(i).toSrcString() : this.getArgumentAt(i).toString());
        }
        buffer.append(')');
      }

      return buffer.toString();
    } else {
      // it's an operator
      final String opName = asSources ? functor.toSrcString() : functor.toString();
      final StringBuilder builder = new StringBuilder();

      final TermOperator OperatorFunctor = (TermOperator) functor;

      final int priority = OperatorFunctor.getPrecedence();

      final Term arg1 = this.getArgumentAt(0);
      final Term arg2 = getArity() > 1 ? this.getArgumentAt(1) : null;

      switch (OperatorFunctor.getType()) {
        case FX: {
          builder.append(opName);
          builder.append(' ');

          final String text = asSources ? arg1.toSrcString() : arg1.toString();

          if (arg1.getPrecedence() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }
        }
        break;
        case FY: {
          builder.append(opName);
          builder.append(' ');

          final String text = asSources ? arg1.toSrcString() : arg1.toString();

          if (arg1.getPrecedence() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }
        }
        break;
        case XF: {
          final String text = asSources ? arg1.toSrcString() : arg1.toString();

          if (arg1.getPrecedence() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
        }
        break;
        case YF: {
          final String text = asSources ? arg1.toSrcString() : arg1.toString();

          if (arg1.getPrecedence() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
        }
        break;
        case XFX: {
          final String text = asSources ? arg1.toSrcString() : arg1.toString();
          final String text2 = asSources ? arg2.toSrcString() : arg2.toString();

          if (arg1.getPrecedence() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (arg2.getPrecedence() >= priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        case YFX: {
          final String text = asSources ? arg1.toSrcString() : arg1.toString();
          final String text2 = asSources ? arg2.toSrcString() : arg2.toString();

          if (arg1.getPrecedence() > priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (arg2.getPrecedence() >= priority) {
            builder.append('(').append(text2).append(')');
          } else {
            builder.append(text2);
          }
        }
        break;
        case XFY: {
          final String text = asSources ? arg1.toSrcString() : arg1.toString();
          final String text2 = asSources ? arg2.toSrcString() : arg2.toString();

          if (arg1.getPrecedence() >= priority) {
            builder.append('(').append(text).append(')');
          } else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (arg2.getPrecedence() > priority) {
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
          buffer.append(getArgumentAt(li).forWrite());
        }
        buffer.append(')');
      }

      return buffer.toString();
    } else {
      // it's an operator
      final String opName = functor.forWrite();
      final StringBuilder builder = new StringBuilder();

      final TermOperator OperatorFunctor = (TermOperator) functor;

      switch (OperatorFunctor.getType()) {
        case FX:
        case FY: {
          builder.append(opName);
          builder.append(' ');
          builder.append(getArgumentAt(0).forWrite());
        }
        break;
        case XF:
        case YF: {
          builder.append(getArgumentAt(0).forWrite());
          builder.append(' ');
          builder.append(opName);
        }
        break;
        case XFX:
        case YFX:
        case XFY: {
          builder.append(getArgumentAt(0).forWrite());
          builder.append(' ');
          builder.append(opName);
          builder.append(' ');
          builder.append(getArgumentAt(1).forWrite());
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
    for (final Term t : this.arguments) {
      if (!t.isGround()) {
        return false;
      }
    }
    return true;
  }

  @Override
  public boolean isUnground() {
    for (final Term t : this.arguments) {
      if (t.isUnground()) {
        return true;
      }
    }
    return false;
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
  public boolean unifyWith(final Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case STRUCT: {
        final TermStruct thisStruct = this;
        final TermStruct thatStruct = (TermStruct) atom;

        final int arity = thisStruct.getArity();

        if (arity == thatStruct.getArity() &&
            thisStruct.getFunctor().unifyWith(thatStruct.getFunctor())) {
          for (int li = 0; li < arity; li++) {
            final Term thisElement = thisStruct.getArgumentAt(li);
            final Term thatElement = thatStruct.getArgumentAt(li);
            if (thisElement != thatElement && !thisElement.unifyWith(thatElement)) {
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
          return value.unifyWith(this);
        }
      }
      case ATOM: {
        return this.getArity() == 0 && this.getFunctor().getText().equals(atom.getText());
      }
    }
    return false;
  }

  @Override
  public boolean isUnifiableWith(Term target) {
    if (this == target) {
      return true;
    }

    if (target.getTermType() == VAR) {
      target = ((TermVar) target).getValue();
    }

    if (target == null) {
      return true;
    } else {
      if (target == this) {
        return true;
      }
    }

    if (target.getTermType() == STRUCT) {
      final TermStruct thisStruct = this;
      final TermStruct thatStruct = (TermStruct) target;

      final int arity = thisStruct.getArity();

      if (arity == thatStruct.getArity() &&
          thisStruct.getFunctor().isUnifiableWith(thatStruct.getFunctor())) {
        for (int li = 0; li < arity; li++) {
          if (!thisStruct.getArgumentAt(li).isUnifiableWith(thatStruct.getArgumentAt(li))) {
            return false;
          }
        }
        return true;
      }
    } else if (this.getArity() == 0 && target.getTermType() == ATOM) {
      return this.getFunctor().getText().equals(target.getText());
    }
    return false;
  }

  public Term makeClone() {
    return this.getArity() == 0 ? this : this.makeClone(new LazyMap<>());
  }

  @Override
  public Term cloneAndReplaceVariableByValue(final Map<Integer, TermVar> variables) {
    final Term result;
    if (this.getArity() == 0) {
      result = this;
    } else {
      final Term[] elements = this.getArguments();
      final int arity = elements.length;
      final Term[] targetElements = new Term[arity];

      boolean changed = false;
      for (int i = 0; i < arity; i++) {
        final Term element = elements[i];
        final Term cloned = element.cloneAndReplaceVariableByValue(variables);
        targetElements[i] = element.cloneAndReplaceVariableByValue(variables);
        changed |= element != cloned;
      }
      if (changed) {
        result = Terms.newStruct(this.getFunctor(), targetElements, this.getPredicateProcessor(),
            this.getSourcePosition());
      } else {
        result = this;
      }
    }
    return result;
  }

  @Override
  protected void arrangeVariableValues(final Map<String, TermVar> variables) {
    final TermStruct struct = this;
    final int arity = struct.getArity();

    for (int li = 0; li < arity; li++) {
      final Term element = struct.getArgumentAt(li);
      if (element.getTermType() == VAR) {
        final TermVar thatVar = (TermVar) element;
        final String variableName = thatVar.getText();
        if (!thatVar.isAnonymous() && thatVar.isUnground()) {
          final TermVar var = variables.get(variableName);
          if (var == null) {
            variables.put(variableName, (TermVar) element);
          } else {
            struct.setArgumentAt(li, var);
          }
        }
      } else {
        element.arrangeVariableValues(variables);
      }
    }
  }


  @Override
  protected Term makeClone(final Map<Integer, TermVar> variables) {
    final Term result;
    if (this.getArity() == 0) {
      result = this;
    } else {
      final Term[] elements = this.getArguments();
      final int arity = elements.length;
      final Term[] targetElements = new Term[arity];
      for (int i = 0; i < arity; i++) {
        final Term element = elements[i];
        targetElements[i] = element.makeClone(variables);
      }
      result = Terms.newStruct(this.getFunctor(), targetElements, this.getPredicateProcessor(),
          this.payload, this.getSourcePosition());
    }
    return result;
  }

  @Override
  public Term replaceVar(final String varName, final Term targetTerm) {
    boolean changed = false;
    final Term[] newTerms = new Term[this.arguments.length];
    for (int i = 0; i < this.arguments.length; i++) {
      final Term next = this.arguments[i];
      final Term replaced = next.replaceVar(varName, targetTerm);
      newTerms[i] = replaced;
      changed |= next != replaced;
    }
    if (changed) {
      return Terms.newStruct(this.functor, newTerms);
    } else {
      return this;
    }
  }

  @Override
  public boolean containsNamedVariable(final String name) {
    for (final Term t : this.arguments) {
      if (t.containsNamedVariable(name)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }
    if (obj == this) {
      return true;
    }
    if (obj instanceof TermStruct) {
      final TermStruct that = (TermStruct) obj;
      return this.arguments.length == that.arguments.length
          && this.functor.equals(that.functor)
          && Arrays.equals(this.arguments, that.arguments);
    }
    return false;
  }

  @Override
  public int hashCode() {
    return System.identityHashCode(this);
  }
}
