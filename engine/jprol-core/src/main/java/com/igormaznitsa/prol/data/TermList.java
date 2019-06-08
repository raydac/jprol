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

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import static com.igormaznitsa.prol.data.TermType.LIST;
import static com.igormaznitsa.prol.data.TermType.VAR;

public final class TermList extends TermStruct {

  public static final String LIST_FUNCTOR = ".";
  public static final Term LIST_FUNCTOR_AS_TERM = new Term(LIST_FUNCTOR);
  public static final TermList NULLLIST = new TermList();

  private static final int INDEX_HEAD = 0;
  private static final int INDEX_TAIL = 1;

  private TermList() {
    super(LIST_FUNCTOR, null);
  }

  public TermList(final Term term) {
    super(LIST_FUNCTOR, new Term[] {term, NULLLIST});
  }

  public TermList(final Term head, final Term tail) {
    super(LIST_FUNCTOR, new Term[] {head, tail});
  }

  public static TermList appendItem(final TermList list, final Term term) {
    final TermList newList = new TermList(term);
    if (!list.isNullList()) {
      newList.setTail(list.getTail());
      list.setTail(newList);
    }
    return newList;
  }

  private static String elementToSourceString(final Term term) {
    switch (term.getTermType()) {
      case ATOM: {
        return term.getSourceLikeRepresentation();
      }
      default:
        return term.getSourceLikeRepresentation();
    }
  }

  public Term getHead() {
    return terms[INDEX_HEAD];
  }

  public Term getTail() {
    final Term tail = this.terms[INDEX_TAIL];
    switch (tail.getTermType()) {
      case VAR: {
        final Var var = (Var) tail;
        final Term val = var.getValue();
        return val == null ? var : val;
      }
      case LIST:
      default:
        return tail;
    }
  }

  public void setTail(final Term newTail) {
    if (newTail == null) {
      throw new NullPointerException("NULL as Tail in list");
    }
    this.terms[INDEX_TAIL] = newTail;
  }

  public int calculateLength() {
    if (this == NULLLIST) {
      return 0;
    }
    final Term tail = this.terms[INDEX_TAIL];
    switch (tail.getTermType()) {
      case LIST: {
        return ((TermList) tail).calculateLength() + 1;
      }
      default:
        return 2;
    }
  }

  @Override
  public Stream<Var> variables() {
    return this.isNullList() ? Stream.empty() :  super.variables();
  }

  @Override
  public Stream<Term> stream() {
    return this.isNullList() ? Stream.of(this.functor) : super.stream();
  }

  public boolean isNullList() {
    return this == NULLLIST;
  }

  @Override
  public Term makeClone() {
    return this.isNullList() ? this : this.doMakeClone(new HashMap<>());
  }

  @Override
  protected Term _makeCloneWithVarReplacement(final Map<Integer, Var> vars) {
    return this.isNullList() ? NULLLIST : new TermList(this.getHead()._makeCloneWithVarReplacement(vars), this.getTail()._makeCloneWithVarReplacement(vars));
  }

  @Override
  protected Term doMakeClone(Map<Integer, Var> vars) {
    return this.isNullList() ? NULLLIST : new TermList(this.getHead().doMakeClone(vars), this.getTail().doMakeClone(vars));
  }

  @Override
  public boolean isAllVariablesInstantiated() {
    if (isNullList()) {
      return true;
    }
    return this.stream().allMatch(Term::isAllVariablesInstantiated);
  }

  @Override
  public TermType getTermType() {
    return TermType.LIST;
  }

  @Override
  public String toString() {
    if (isNullList()) {
      return "[]";
    }
    final StringBuilder builder = new StringBuilder("[");

    boolean notfirst = false;
    Term list = this;

    while (list != NULLLIST) {
      if (list.getTermType() == LIST) {
        if (notfirst) {
          builder.append(',');
        }
        final TermList asList = (TermList) list;
        builder.append(asList.getHead().toString());
        list = asList.getTail();
      } else {
        if (notfirst) {
          builder.append('|');
        }
        builder.append(list.toString());
        break;
      }
      notfirst = true;
    }

    builder.append(']');
    return builder.toString();
  }

  @Override
  public String getSourceLikeRepresentation() {
    if (isNullList()) {
      return "[]";
    }
    StringBuilder builder = new StringBuilder("[");

    boolean notfirst = false;
    Term list = this;

    while (list != NULLLIST) {
      if (list.getTermType() == LIST) {
        if (notfirst) {
          builder.append(',');
        }
        final TermList asList = (TermList) list;
        builder.append(elementToSourceString(asList.getHead()));
        list = asList.getTail();
      } else {
        if (notfirst) {
          builder.append('|');
        }
        builder.append(elementToSourceString(list));
        break;
      }
      notfirst = true;
    }

    builder.append(']');
    return builder.toString();
  }

  public final void replaceLastElement(final Term newLastElement) {
    if (isNullList()) {
      throw new ProlCriticalError("Attemption to change Null list");
    }
    TermList curList = this;
    while (!Thread.currentThread().isInterrupted()) {
      Term tail = curList.getTail();
      if (tail == NULLLIST || tail.getTermType() != LIST) {
        curList.setTail(newLastElement);
        break;
      }
      curList = (TermList) tail;
    }
  }

  @Override
  public String getSignature() {
    return getSourceLikeRepresentation();
  }

  @Override
  public String forWrite() {
    if (isNullList()) {
      return "[]";
    }
    StringBuilder builder = new StringBuilder("[");

    boolean notfirst = false;
    Term list = this;

    while (list != NULLLIST) {
      if (list.getTermType() == LIST) {
        if (notfirst) {
          builder.append(',');
        }
        final TermList asList = (TermList) list;
        builder.append(asList.getHead().forWrite());
        list = asList.getTail();
      } else {
        if (notfirst) {
          builder.append('|');
        }
        builder.append(list.forWrite());
        break;
      }
      notfirst = true;
    }

    builder.append(']');
    return builder.toString();
  }

  @Override
  public boolean Equ(final Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case LIST: {
        TermList thatList = (TermList) atom;
        if (this.isNullList()) {
          return thatList.isNullList();
        } else if (thatList.isNullList()) {
          return this.isNullList();
        }

        return this.getHead().Equ(thatList.getHead()) && this.getTail().Equ(thatList.getTail());
      }
      case VAR: {
        final Var var = (Var) atom;
        final Term value = var.getValue();
        if (value == null) {
          return ((Var) atom).setValue(this);
        } else {
          return Equ(value);
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

    if (atom.getTermType() == VAR) {
      atom = ((Var) atom).getValue();
    }

    if (atom == null) {
      return true;
    }

    if (atom.getTermType() == LIST) {
      if (this == atom) {
        return true;
      }
      final TermList thisList = this;
      final TermList thatList = (TermList) atom;

      if (thisList.isNullList()) {
        return thatList.isNullList();
      } else {
        if (thatList.isNullList()) {
          return false;
        }
      }

      return thisList.getHead().equWithoutSet(thatList.getHead()) && thisList.getTail().equWithoutSet(thatList.getTail());
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
      case LIST: {
        final TermList thatList = (TermList) atom;
        if (isNullList() && thatList.isNullList()) {
          return 0;
        }

        final TermList thisList = this;

        if (thisList.isNullList() && !thatList.isNullList()) {
          return -1;
        }
        if (!thisList.isNullList() && thatList.isNullList()) {
          return 1;
        }

        final Term thisHead = thisList.getHead();
        final Term thatHead = thatList.getHead();

        int result = thisHead.termComparsion(thatHead);
        if (result != 0) {
          return result;
        }

        return thisList.getTail().termComparsion(thatList.getTail());
      }
      default:
        return 1;
    }
  }

  @Override
  public boolean hasAnyDifference(final Term atom) {
    if (atom.getTermType() != LIST) {
      return true;
    }

    TermList thisList = this;
    TermList thatList = (TermList) atom;

    if (thisList == NULLLIST) {
      return thisList != thatList;
    } else if (thatList == NULLLIST) {
      return true;
    }

    if (thisList.getHead().hasAnyDifference(thatList.getHead())) {
      return true;
    }
    return thisList.getTail().hasAnyDifference(thatList.getTail());
  }

  @Override
  public boolean hasVariableWithName(final String name) {
    if (this == NULLLIST) {
      return false;
    }
    return getHead().hasVariableWithName(name) || getTail().hasVariableWithName(name);
  }
}
