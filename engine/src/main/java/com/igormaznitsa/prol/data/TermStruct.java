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
import java.util.Map;

/**
 * The class describing the prolog structure
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class TermStruct extends Term {

  /**
   * It will be used to init zero-arity structures
   */
  private static final Term[] EMPTY_ARRAY = new Term[0];
  /**
   * The array contains all items of the structure, can't be null, if it has
   * zero arity then empty array will be saved into the variable
   */
  protected final Term[] terms;
  /**
   * The variable contains the functor of the structure, must not be null
   */
  protected final Term functor;
  /**
   * The variable contains the link to the processor which can be used to
   * process data of the structure, can be null
   */
  protected PredicateProcessor predicateProcessor;
  /**
   * The variable contains the signature of the structure in the format
   * <functor>/<arity>
   */
  private String structureSignature;
  /**
   * This flag shows that the rule definition functor is presented as the struct
   * functor
   */
  private boolean rulefunctor;
  /**
   * The flag shows that the rulefunctor flag has been inited, for lazy initing
   */
  private boolean rulefunctorset;

  /**
   * A constructor allows to make structure with zero arity
   *
   * @param functor the functor for the structure, must not be null
   */
  public TermStruct(final Term functor) {
    this(functor, EMPTY_ARRAY);
  }

  /**
   * A constructor allows to create structure with a functor and predeficned
   * elements
   *
   * @param functor the string describes the functor, must not be null
   * @param elements the array which contains elements of the structure, it can
   * be null and in the case the structure will have zero arity
   */
  public TermStruct(final String functor, final Term[] elements) {
    this(new Term(functor), elements);
    if (functor.length() != 2) {
      rulefunctor = false;
      rulefunctorset = true;
    }
  }

  /**
   * A constructor allows to create structure with a functor and predeficned
   * elements
   *
   * @param functor the functor for the dreated structure, must not be null
   * @param elements the array which contains elements of the structure, it can
   * be null and in the case the structure will have zero arity
   */
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

  /**
   * Check that the functor is the term represended by ':-' text p,s. the
   * function makes a lazy initing of inside flag
   *
   * @return true if the runle functor, else false
   */
  public final boolean isFunctorLikeRuleDefinition() {
    if (!rulefunctorset) {
      rulefunctor = ":-".equals(functor.getText());
      rulefunctorset = true;
    }
    return rulefunctor;
  }

  /**
   * A constructor allows to create structure with a functor and predeficned
   * elements and set the predicate processor
   *
   * @param functor the functor for the dreated structure, must not be null
   * @param elements the array which contains elements of the structure, it can
   * be null and in the case the structure will have zero arity
   * @param processor the predicate processor which will be used to process
   * elements of the structure, can be null
   */
  public TermStruct(final Term functor, final Term[] elements, final PredicateProcessor processor) {
    this(functor, elements);
    predicateProcessor = processor;
  }

  /**
   * Get the functor of the structure
   *
   * @return the functor as a Term object
   */
  public final Term getFunctor() {
    return functor;
  }

  /**
   * Get the structure elements as a Term array
   *
   * @return the Term array contains structure elements, it is empty array if
   * the structure has zero arity
   */
  public final Term[] getElementsAsArray() {
    return terms;
  }

  /**
   * Set the structure element for its index (the first element index is zero)
   *
   * @param index the index of an element (the first element index is zero)
   * @param element the element which will be placed in the position
   */
  public final void setElement(final int index, final Term element) {
    terms[index] = element;
  }

  @Override
  public void fillVarables(final Map<String, Var> table) {
    final Term[] arr = terms;

    final int len = arr.length;

    for (int li = 0; li < len; li++) {
      arr[li].fillVarables(table);
    }
  }

  /**
   * Check that the structure doesn't have null as one from its elements
   *
   * @return true if there is not any null element, else false
   */
  public final boolean hasNullElement() {
    final Term[] arr = terms;

    final int len = arr.length;

    for (int li = 0; li < len; li++) {
      if (arr[li] == null) {
        return false;
      }
    }
    return true;
  }

  /**
   * Get the element for its index (the first element has the zero index)
   *
   * @param index the element index (the first element has the zero index)
   * @return the element has been found at the position
   */
  public final Term getElement(final int index) {
    return terms[index];
  }

  /**
   * Get the arity of the structure
   *
   * @return the arity of the structure as integer
   */
  public final int getArity() {
    return terms.length;
  }

  @Override
  public int getPriority() {
    if (functor.getTermType() == TYPE_OPERATOR) {
      return functor.getPriority();
    }
    else {
      return 0;
    }
  }

  @Override
  public String toString() {
    return getStringRepresentation(false);
  }

  @Override
  public String getSourceLikeRepresentation() {
    return getStringRepresentation(true);
  }

  /**
   * Inside auxulary function to make source like representation of the
   * structure
   *
   * @param sourceLike if true then the result will be optimized for output as
   * prolog sources else for write
   * @return the representation of the structure as String object, must not be
   * null
   */
  private final String getStringRepresentation(final boolean sourceLike) {
    if (functor.getTermType() != Term.TYPE_OPERATOR) {
      // just struct
      final StringBuilder buffer = new StringBuilder(Utils.encodeTextSourceLike(getText()));

      if (getArity() != 0) {
        buffer.append('(');
        for (int li = 0; li < getArity(); li++) {
          if (li > 0) {
            buffer.append(',');
          }
          buffer.append(sourceLike ? getElement(li).getSourceLikeRepresentation() : getElement(li).toString());
        }
        buffer.append(')');
      }

      return buffer.toString();
    }
    else {
      // it's an operator
      final String opName = sourceLike ? functor.getSourceLikeRepresentation() : functor.toString();
      final StringBuilder builder = new StringBuilder();

      final Operator OperatorFunctor = (Operator) functor;

      final int priority = OperatorFunctor.getPriority();

      switch (OperatorFunctor.getOperatorType()) {
        case Operator.OPTYPE_FX: {
          builder.append(opName);
          builder.append(' ');

          final String text = sourceLike ? getElement(0).getSourceLikeRepresentation() : getElement(0).toString();

          if (getElement(0).getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          }
          else {
            builder.append(text);
          }
        }
        break;
        case Operator.OPTYPE_FY: {
          builder.append(opName);
          builder.append(' ');

          final String text = sourceLike ? getElement(0).getSourceLikeRepresentation() : getElement(0).toString();

          if (getElement(0).getPriority() > priority) {
            builder.append('(').append(text).append(')');
          }
          else {
            builder.append(text);
          }
        }
        break;
        case Operator.OPTYPE_XF: {
          final String text = sourceLike ? getElement(0).getSourceLikeRepresentation() : getElement(0).toString();

          if (getElement(0).getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          }
          else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
        }
        break;
        case Operator.OPTYPE_YF: {
          final String text = sourceLike ? getElement(0).getSourceLikeRepresentation() : getElement(0).toString();

          if (getElement(0).getPriority() > priority) {
            builder.append('(').append(text).append(')');
          }
          else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
        }
        break;
        case Operator.OPTYPE_XFX: {
          final String text = sourceLike ? getElement(0).getSourceLikeRepresentation() : getElement(0).toString();
          final String text2 = sourceLike ? getElement(1).getSourceLikeRepresentation() : getElement(1).toString();

          if (getElement(0).getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          }
          else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (getElement(1).getPriority() >= priority) {
            builder.append('(').append(text2).append(')');
          }
          else {
            builder.append(text2);
          }
        }
        break;
        case Operator.OPTYPE_YFX: {
          final String text = sourceLike ? getElement(0).getSourceLikeRepresentation() : getElement(0).toString();
          final String text2 = sourceLike ? getElement(1).getSourceLikeRepresentation() : getElement(1).toString();

          if (getElement(0).getPriority() > priority) {
            builder.append('(').append(text).append(')');
          }
          else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (getElement(1).getPriority() >= priority) {
            builder.append('(').append(text2).append(')');
          }
          else {
            builder.append(text2);
          }
        }
        break;
        case Operator.OPTYPE_XFY: {
          final String text = sourceLike ? getElement(0).getSourceLikeRepresentation() : getElement(0).toString();
          final String text2 = sourceLike ? getElement(1).getSourceLikeRepresentation() : getElement(1).toString();

          if (getElement(0).getPriority() >= priority) {
            builder.append('(').append(text).append(')');
          }
          else {
            builder.append(text);
          }

          builder.append(' ');
          builder.append(opName);
          builder.append(' ');

          if (getElement(1).getPriority() > priority) {
            builder.append('(').append(text2).append(')');
          }
          else {
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
    if (functor.getTermType() != Term.TYPE_OPERATOR) {
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
    }
    else {
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

  /**
   * Get the predicate processor for the structure
   *
   * @return the predicate processor for the structure or null if it is not
   * defined
   */
  public final PredicateProcessor getPredicateProcessor() {
    return predicateProcessor;
  }

  /**
   * Set the predicate processor for the structure
   *
   * @param processor the processor which will be called if we need to process
   * the structure data (as an example it the functor is an Operator), it can be
   * null
   */
  public void setPredicateProcessor(final PredicateProcessor processor) {
    predicateProcessor = processor;
  }

  @Override
  public boolean checkVariables() {
    final int arity = terms.length;
    for (int li = 0; li < arity; li++) {
      if (!terms[li].checkVariables()) {
        return false;
      }
    }
    return true;
  }

  @Override
  public int getTermType() {
    return TYPE_STRUCT;
  }

  @Override
  public String getSignature() {
    if (structureSignature == null) {
      structureSignature = new StringBuilder(functor.getText()).append('/').append(getArity()).toString();
    }
    return structureSignature;
  }

  @Override
  public boolean Equ(final Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case Term.TYPE_STRUCT: {
        final TermStruct thisStruct = this;
        final TermStruct thatStruct = (TermStruct) atom;

        final int arity = thisStruct.getArity();

        if (arity == thatStruct.getArity() && thisStruct.getFunctor().Equ(thatStruct.getFunctor())) {
          for (int li = 0; li < arity; li++) {
            final Term thiselement = thisStruct.getElement(li);
            final Term thatelement = thatStruct.getElement(li);
            if (thiselement != thatelement && !thiselement.Equ(thatelement)) {
              return false;
            }
          }
          return true;
        }
      }
      break;
      case Term.TYPE_VAR: {
        final Var var = (Var) atom;
        final Term value = var.getValue();

        if (value == null) {
          return var.setValue(this);
        }
        else {
          if (value == this) {
            return true;
          }
          return value.Equ(this);
        }
      }
    }
    return false;
  }

  @Override
  public boolean equWithoutSet(Term atom) {
    if (this == atom) {
      return true;
    }

    if (atom.getTermType() == Term.TYPE_VAR) {
      atom = ((Var) atom).getValue();
    }

    if (atom == null) {
      return true;
    }
    else {
      if (atom == this) {
        return true;
      }
    }

    if (atom.getTermType() == Term.TYPE_STRUCT) {
      final TermStruct thisStruct = this;
      final TermStruct thatStruct = (TermStruct) atom;

      final int arity = thisStruct.getArity();

      if (arity == thatStruct.getArity() && thisStruct.getFunctor().equWithoutSet(thatStruct.getFunctor())) {
        for (int li = 0; li < arity; li++) {
          if (!thisStruct.getElement(li).equWithoutSet(thatStruct.getElement(li))) {
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

    if (atom.getTermType() == Term.TYPE_VAR && !((Var) atom).isUndefined()) {
      atom = ((Var) atom).getValue();
    }

    switch (atom.getTermType()) {
      case Term.TYPE_LIST:
        return -1;
      case Term.TYPE_STRUCT: {
        final TermStruct thatStruct = (TermStruct) atom;

        final int thisArity = getArity();
        final int thatArity = getArity();

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
          }
          else {
            return result;
          }
        }
        else {
          if (thisArity < thatArity) {
            return -1;
          }
          else {
            return 1;
          }
        }
      }
      default:
        return 1;
    }
  }

  @Override
  public boolean hasAnyDifference(final Term atom) {
    if (atom.getTermType() != Term.TYPE_STRUCT) {
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
    }
    else {
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
