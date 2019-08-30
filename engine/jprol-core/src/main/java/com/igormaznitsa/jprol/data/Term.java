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

import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.Terms.newList;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.utils.Utils.createOrAppendToList;
import static com.igormaznitsa.jprol.utils.Utils.escapeSrc;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toMap;

public class Term {

  private final String text;

  Term(final String text) {
    this.text = requireNonNull(text);
  }

  public int getPriority() {
    return 0;
  }

  public String getText() {
    return this.text;
  }

  public TermType getTermType() {
    return ATOM;
  }

  public boolean dryUnifyTo(Term term) {
    if (this == term) {
      return true;
    }

    switch (term.getTermType()) {
      case VAR: {
        term = ((TermVar) term).getValue();
        return term == null || this.dryUnifyTo(term);
      }
      case ATOM: {
        return term.getClass() == Term.class && getText().equals(term.getText());
      }
      case STRUCT: {
        final TermStruct thatStruct = (TermStruct) term;
        return thatStruct.getArity() == 0 && getText().equals(thatStruct.getFunctor().getText());
      }
      default:
        return false;
    }
  }

  public Map<String, TermVar> allNamedVarsAsMap() {
    return this.variables()
        .collect(toMap(TermVar::getText, e -> e, (v1, v2) -> v2));
  }

  public Map<String, Term> allNamedVarValuesAsMap() {
    return this.variables()
        .filter(v -> !v.isFree())
        .collect(toMap(TermVar::getText, e -> e.<Term>findNonVarOrDefault(e), (v1, v2) -> v2));
  }

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

  public Stream<Term> stream() {
    return Stream.of(this);
  }

  public Stream<TermVar> variables() {
    return Stream.empty();
  }

  public String toSrcString() {
    return String.format("\'%s\'", escapeSrc(getText()));
  }

  @Override
  public String toString() {
    return toSrcString();
  }

  @Override
  public int hashCode() {
    if (text == null) {
      return super.hashCode();
    } else {
      return text.hashCode();
    }
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

    final Class<?> objclass = obj.getClass();

    if (objclass == TermLong.class || obj.getClass() == Term.class) {

      final Term other = (Term) obj;

      if (hashCode() != other.hashCode()) {
        return false;
      }
      return this.text.equals(other.text);
    } else {
      return false;
    }
  }

  public TermList toCharList() {
    final String text = getText();
    final int len = text.length();
    if (len == 0) {
      return Terms.NULL_LIST;
    }

    final StringBuilder buff = new StringBuilder(1);

    TermList resultList = null;
    TermList curList = null;

    for (int li = 0; li < len; li++) {
      buff.append(text.charAt(li));
      final Term newAtom = new Term(buff.toString());
      buff.setLength(0);
      if (li == 0) {
        resultList = newList(newAtom);
        curList = resultList;
      } else {
        curList = createOrAppendToList(curList, newAtom);
      }
    }

    return resultList;
  }

  public int compareTermTo(Term atom) {
    if (this == atom) {
      return 0;
    }

    atom = atom.findNonVarOrSame();

    switch (atom.getTermType()) {
      case ATOM: {
        if (atom instanceof NumericTerm) {
          return 1;
        }
        return this.text.compareTo(atom.text);
      }
      case VAR: {
        return 1;
      }
      case OPERATOR: {
        return this.text.compareTo(atom.text);
      }
      default:
        return -1;
    }
  }

  public TermList toCharCodeList() {
    final String text = this.getText();

    if (text == null || text.isEmpty()) {
      return Terms.NULL_LIST;
    } else {
      final TermList result = createOrAppendToList(null, newLong(text.charAt(0)));
      TermList current = result;
      for (int i = 1; i < text.length(); i++) {
        current = createOrAppendToList(current, newLong(text.charAt(i)));
      }
      return result;
    }
  }

  protected void doArrangeVars(final Map<String, TermVar> variables) {
  }

  public final void arrangeVariablesInsideTerms(final Term termTwo) {
    final Map<String, TermVar> varMap = new HashMap<>();
    this.doArrangeVars(varMap);
    termTwo.doArrangeVars(varMap);
  }

  public int getTextLength() {
    return this.text == null ? 0 : this.text.length();
  }

  public boolean hasVariableWithName(final String name) {
    return false;
  }

  public Term makeClone() {
    return this;
  }

  protected Term doMakeClone(final Map<Integer, TermVar> vars) {
    return this;
  }

  public final Term makeCloneAndVarBound() {
    return this.makeCloneAndVarBound(new HashMap<>());
  }

  protected Term makeCloneAndVarBound(final Map<Integer, TermVar> vars) {
    return this;
  }

}
