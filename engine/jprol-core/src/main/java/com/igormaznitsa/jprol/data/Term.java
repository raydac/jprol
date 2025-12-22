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
/*
 * Copyright (C) 2014 Igor Maznitsa (http://www.igormaznitsa.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.igormaznitsa.jprol.data;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.utils.ProlUtils.escapeSrc;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toMap;

import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.utils.LazyMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

public class Term {

  protected final Object payload;
  private final String text;
  private final SourcePosition sourcePosition;

  Term(final String text, final SourcePosition sourcePosition) {
    this(text, null, sourcePosition);
  }

  Term(final String text, final Object payload, final SourcePosition sourcePosition) {
    this.text = requireNonNull(text);
    this.payload = payload;
    this.sourcePosition = requireNonNull(sourcePosition);
  }

  /**
   * Check that the term is a null list term.
   *
   * @return true if it is a null list term, false otherwise
   * @since 2.3.0
   */
  public boolean isNullList() {
    return false;
  }

  /**
   * Check that the term is an anonymous variable.
   *
   * @return true if it is an anonymous variable, false otherwise
   * @since 2.3.0
   */
  public boolean isAnonymous() {
    return false;
  }

  /**
   * Get payload object associated with the term.
   *
   * @param <T> type of expected payload object
   * @return the payload object saved in the term, can be null
   * @since 2.3.0
   */
  @SuppressWarnings("unchecked")
  public <T> T getPayload() {
    return (T) this.payload;
  }

  /**
   * Make version of the term where a named variable replaced by a target term.
   *
   * @param varName    name of variable to be replaced, must not be null
   * @param targetTerm target term to be injected instead of the variable, must not be null
   * @return new version of the term with replaced variables or the same
   * @since 2.3.0
   */
  public Term replaceVar(final String varName, final Term targetTerm) {
    return this;
  }

  /**
   * Get source position for the term.
   *
   * @return the source position
   * @see SourcePosition#UNKNOWN
   */
  public SourcePosition getSourcePosition() {
    return this.sourcePosition;
  }

  public int getPrecedence() {
    return 0;
  }

  public String getText() {
    return this.text;
  }

  public TermType getTermType() {
    return ATOM;
  }

  /**
   * Works like unify but don't change internal state of terms and can be used for check unify possibility.
   *
   * @param target target term to be checked, must not be null
   * @return true if it can be unified with the target term, false otherwise
   * @see #unifyTo(Term)
   */
  public boolean dryUnifyTo(Term target) {
    if (this == target) {
      return true;
    }

    switch (target.getTermType()) {
      case VAR: {
        target = ((TermVar) target).getValue();
        return target == null || this.dryUnifyTo(target);
      }
      case ATOM: {
        return target.getClass() == Term.class && getText().equals(target.getText());
      }
      case STRUCT: {
        final TermStruct thatStruct = (TermStruct) target;
        return thatStruct.getArity() == 0 && getText().equals(thatStruct.getFunctor().getText());
      }
      default:
        return false;
    }
  }

  public Map<String, TermVar> findAllNamedVariables() {
    return this.variables()
        .collect(
            toMap(TermVar::getText,
                e -> e,
                (v1, v2) -> v2
            )
        );
  }

  public Map<String, Term> findAllNamedVariableValues() {
    return this.variables()
        .filter(v -> !v.isUnground())
        .collect(toMap(TermVar::getText, e -> e.<Term>findNonVarOrDefault(e), (v1, v2) -> v2));
  }

  /**
   * Check that the term represents a ground term (not a variable)
   *
   * @return true if grounded term, false otherwise
   */
  public boolean isGround() {
    return true;
  }

  @SuppressWarnings("unchecked")
  public <T extends Term> T findNonVarOrSame() {
    return (T) this;
  }

  @SuppressWarnings("unchecked")
  public <T extends Term> T findNonVarOrDefault(final T term) {
    return (T) this;
  }

  /**
   * Stream for all (!) internal terms, in case of a list term it will stream even functor.
   * Do not use the method to iterate only children in compound terms. Use special methods in CompoundTerm
   *
   * @return stream of all terms
   * @see CompoundTerm#streamChildren()
   * @see CompoundTerm#spliteratorChildren()
   * @see CompoundTerm#iterator()
   */
  public Stream<Term> stream() {
    return Stream.of(this);
  }

  public Stream<TermVar> variables() {
    return Stream.empty();
  }

  public String toSrcString() {
    return String.format("'%s'", escapeSrc(getText()));
  }

  @Override
  public String toString() {
    return toSrcString();
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.text);
  }

  public Number toNumber() {
    throw new ProlTypeErrorException("numeric", "NonNumeric term", this);
  }

  public String getSignature() {
    return getText() + "/0";
  }

  public String forWrite() {
    return getText();
  }

  /**
   * Unify the term with another term. It can change state if a non-ground variable.
   * We can say that this operation align state of terms.
   *
   * @param other term to be unified.
   * @return true if state aligned successfully, false otherwise.
   */
  public boolean unifyTo(final Term other) {
    if (this == other) {
      return true;
    }

    boolean result = false;

    switch (other.getTermType()) {
      case VAR: {
        final TermVar var = (TermVar) other;
        final Term value = var.getValue();
        if (value == null) {
          result = ((TermVar) other).setValue(this);
        } else {
          result = unifyTo(value);
        }
      }
      break;
      case ATOM: {
        result = other.getClass() == Term.class && getText().equals(other.getText());
      }
      break;
      case STRUCT: {
        final TermStruct thatStruct = (TermStruct) other;
        result = thatStruct.getArity() == 0 && getText().equals(thatStruct.getFunctor().getText());
      }
      break;
    }

    return result;
  }

  @Override
  public boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }

    if (this == obj) {
      return true;
    }

    if (obj.getClass() == Term.class) {
      final Term that = (Term) obj;
      return this.text.equals(that.text);
    }

    return false;
  }

  /**
   * Check that term can contain variables.
   *
   * @return true if the term can contain variables, false otherwise
   */
  public boolean canContainVariables() {
    return false;
  }

  /**
   * Arrange named variable values for their names.
   *
   * @param variables a mutable map as store for found variables.
   */
  protected void arrangeVariableValues(final Map<String, TermVar> variables) {
  }

  public final void arrangeVariablesWith(final Term termTwo) {
    if (this.canContainVariables() && termTwo.canContainVariables()) {
      final Map<String, TermVar> variables = new LazyMap<>();
      this.arrangeVariableValues(variables);
      termTwo.arrangeVariableValues(variables);
    }
  }

  public int getTextLength() {
    return this.text == null ? 0 : this.text.length();
  }

  /**
   * Check that the term has a named variable among its content.
   *
   * @param name name of variable, must not be null
   * @return true if it contains, false otherwise
   */
  public boolean containsNamedVariable(final String name) {
    return false;
  }

  /**
   * Get cloned version of the term.
   * @return a cloned version.
   */
  public Term makeClone() {
    return this;
  }

  /**
   * Get cloned version of the term, all named variables will be deduplicated.
   *
   * @param variables a mutable map to play a store for deduplication, must not be null
   * @return cloned term, must not be null
   */
  protected Term makeClone(final Map<Integer, TermVar> variables) {
    return this;
  }

  /**
   * Get cloned version of term where grounded variables replaced by their values and non-grounded variables are aligned.
   *
   * @param variables a mutual map to be used as store for variables
   * @return cloned term, must not be null
   */
  public Term cloneAndReplaceVariablesByValues(final Map<Integer, TermVar> variables) {
    return this;
  }

}
