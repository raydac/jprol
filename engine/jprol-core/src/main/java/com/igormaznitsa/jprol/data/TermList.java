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

import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.NULL_LIST;
import static com.igormaznitsa.jprol.data.Terms.newList;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.utils.Utils.createOrAppendToList;

import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Stream;

public final class TermList extends TermStruct {

  private static final int INDEX_HEAD = 0;
  private static final int INDEX_TAIL = 1;

  TermList() {
    super(Terms.LIST_FUNCTOR, null);
  }

  TermList(final Term term) {
    super(Terms.LIST_FUNCTOR, new Term[] {term, Terms.NULL_LIST});
  }

  TermList(final Term head, final Term tail) {
    super(Terms.LIST_FUNCTOR, new Term[] {head, tail});
  }

  @Override
  public Spliterator<Term> spliteratorChildren() {
    return Spliterators.spliteratorUnknownSize(this.iterator(), 0);
  }

  @Override
  public Iterator<Term> iterator() {
    return new Iterator<Term>() {
      Term current = TermList.this;

      void findNext() {
        if (current == null) {
          throw new NoSuchElementException();
        }
        if (current.getTermType() == LIST) {
          final TermList asList = (TermList) current;
          current = asList.isNullList() ? null : asList.getTail();
        } else {
          current = null;
        }
      }

      @Override
      public boolean hasNext() {
        return current != null &&
            (current.getTermType() == LIST && !((TermList) current).isNullList());
      }

      @Override
      public Term next() {
        final Term result = this.current;
        this.findNext();
        if (result.getTermType() == LIST) {
          final TermList asList = (TermList) result;
          if (asList.isNullList()) {
            throw new NoSuchElementException();
          }
          return asList.getHead();
        } else {
          return result;
        }
      }
    };
  }

  public static TermList asList(final Term... elements) {
    return asList(Arrays.asList(elements));
  }

  public static TermList asList(final List<Term> elements) {
    if (elements.isEmpty()) {
      return NULL_LIST;
    } else if (elements.size() == 1) {
      return Terms.newList(elements.get(0), NULL_LIST);
    } else {
      final TermList result = newList(elements.get(0));
      TermList next = result;
      final int length = elements.size();
      for (int i = 1; i < length; i++) {
        next = createOrAppendToList(next, elements.get(i));
      }
      return result;
    }
  }

  public static TermList makeListFromElementWithSplitStructure(final Term element) {
    switch (element.getTermType()) {
      case LIST:
        return (TermList) element;
      case STRUCT: {
        final TermStruct struct = (TermStruct) element;
        final TermList result = newList(struct.getFunctor());
        TermList curResult = result;
        final int arity = struct.getArity();
        for (int li = 0; li < arity; li++) {
          curResult = createOrAppendToList(curResult, struct.getElement(li));
        }
        return result;
      }
      default:
        return newList(element);
    }
  }

  @SuppressWarnings("unchecked")
  public <T extends Term> T getHead() {
    return (T) this.terms[INDEX_HEAD];
  }

  @SuppressWarnings("unchecked")
  public <T extends Term> T getTail() {
    final Term tail = this.terms[INDEX_TAIL];
    switch (tail.getTermType()) {
      case VAR: {
        final TermVar var = (TermVar) tail;
        final Term val = var.getValue();
        return (T) (val == null ? var : val);
      }
      case LIST:
      default:
        return (T) tail;
    }
  }

  public void setTail(final Term newTail) {
    this.terms[INDEX_TAIL] = Objects.requireNonNull(newTail, "Null is not allowed as list tail");
  }

  public int calculateLength() {
    if (this == Terms.NULL_LIST) {
      return 0;
    }
    final Term tail = this.terms[INDEX_TAIL];
    if (tail.getTermType() == LIST) {
      return ((TermList) tail).calculateLength() + 1;
    }
    return 2;
  }

  @Override
  public Stream<Term> stream() {
    return this.isNullList() ? Stream.of(this.functor) : super.stream();
  }

  @Override
  public Stream<TermVar> variables() {
    return this.isNullList() ? Stream.empty() : super.variables();
  }

  @Override
  public Term makeClone() {
    return this.isNullList() ? this : this.doMakeClone(new HashMap<>());
  }

  public boolean isNullList() {
    return this.terms == EMPTY_ARRAY;
  }

  @Override
  protected Term makeCloneAndVarBound(final Map<Integer, TermVar> vars) {
    return this.isNullList() ? Terms.NULL_LIST : newList(this.getHead().makeCloneAndVarBound(vars),
        this.getTail().makeCloneAndVarBound(vars));
  }

  @Override
  public boolean isGround() {
    return this.isNullList() || this.stream().allMatch(Term::isGround);
  }

  @Override
  public TermType getTermType() {
    return TermType.LIST;
  }

  @Override
  protected Term doMakeClone(Map<Integer, TermVar> vars) {
    return this.isNullList() ? Terms.NULL_LIST :
        newList(this.getHead().doMakeClone(vars), this.getTail().doMakeClone(vars));
  }

  @Override
  public String toString() {
    if (isNullList()) {
      return "[]";
    }
    final StringBuilder builder = new StringBuilder("[");

    boolean notFirst = false;
    Term list = this;

    while (list != Terms.NULL_LIST) {
      if (list.getTermType() == LIST) {
        if (notFirst) {
          builder.append(',');
        }
        final TermList asList = (TermList) list;
        builder.append(asList.getHead().toString());
        list = asList.getTail();
      } else {
        if (notFirst) {
          builder.append('|');
        }
        builder.append(list);
        break;
      }
      notFirst = true;
    }

    builder.append(']');
    return builder.toString();
  }

  @Override
  public String toSrcString() {
    if (isNullList()) {
      return "[]";
    }
    StringBuilder builder = new StringBuilder("[");

    boolean notFirst = false;
    Term list = this;

    while (list != Terms.NULL_LIST) {
      if (list.getTermType() == LIST) {
        if (notFirst) {
          builder.append(',');
        }
        final TermList asList = (TermList) list;
        builder.append(asList.getHead().toSrcString());
        list = asList.getTail();
      } else {
        if (notFirst) {
          builder.append('|');
        }
        builder.append(list.toSrcString());
        break;
      }
      notFirst = true;
    }

    builder.append(']');
    return builder.toString();
  }

  @Override
  public String getSignature() {
    return "./2";
  }

  public void replaceLastElement(final Term newLastElement) {
    if (isNullList()) {
      throw new ProlCriticalError("Attempt to change Null list");
    }
    TermList curList = this;
    while (!Thread.currentThread().isInterrupted()) {
      Term tail = curList.getTail();
      if (tail == Terms.NULL_LIST || tail.getTermType() != LIST) {
        curList.setTail(newLastElement);
        break;
      }
      curList = (TermList) tail;
    }
  }

  @Override
  public boolean unifyTo(final Term atom) {
    if (this == atom) {
      return true;
    }

    switch (atom.getTermType()) {
      case LIST: {
        TermList thatList = (TermList) atom;
        if (this.isNullList()) {
          return thatList.isNullList();
        } else if (thatList.isNullList()) {
          return false;
        }

        return this.getHead().unifyTo(thatList.getHead()) &&
            this.getTail().unifyTo(thatList.getTail());
      }
      case VAR: {
        final TermVar var = (TermVar) atom;
        final Term value = var.getValue();
        if (value == null) {
          return ((TermVar) atom).setValue(this);
        } else {
          return unifyTo(value);
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
      atom = ((TermVar) atom).getValue();
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

      return thisList
          .getHead().dryUnifyTo(thatList.getHead())
          && thisList.getTail().dryUnifyTo(thatList.getTail());
    }
    return false;
  }

  @Override
  public String forWrite() {
    if (isNullList()) {
      return "[]";
    }
    StringBuilder builder = new StringBuilder("[");

    boolean notFirst = false;
    Term list = this;

    while (list != Terms.NULL_LIST) {
      if (list.getTermType() == LIST) {
        if (notFirst) {
          builder.append(',');
        }
        final TermList asList = (TermList) list;
        builder.append(asList.getHead().forWrite());
        list = asList.getTail();
      } else {
        if (notFirst) {
          builder.append('|');
        }
        builder.append(list.forWrite());
        break;
      }
      notFirst = true;
    }

    builder.append(']');
    return builder.toString();
  }

  public Term[] toArray() {
    if (this.isNullList()) {
      return EMPTY_ARRAY;
    }

    final ArrayList<Term> arraylist = new ArrayList<>();
    TermList currentList = this;
    while (!Thread.currentThread().isInterrupted()) {
      if (currentList.isNullList()) {
        break;
      }
      arraylist.add(currentList.getHead());
      final Term nextList = currentList.getTail();
      if (nextList.getTermType() == LIST) {
        currentList = (TermList) nextList;
      } else {
        arraylist.add(nextList);
        break;
      }
    }
    return arraylist.toArray(EMPTY_ARRAY);
  }

  public Term toAtom() {
    if (this.isNullList()) {
      return new Term("<empty>");
    }
    if (this.getTail() == Terms.NULL_LIST) {
      return this.getHead();
    } else {
      final int length = this.calculateLength();
      if (length == 3) {
        if (this.getHead() == Terms.LIST_FUNCTOR) {
          final TermList secondElement = this.getTail();
          final TermList thirdElement = secondElement.getTail();

          if (thirdElement.getHead().getTermType() == LIST) {
            return newList(secondElement.getHead(), thirdElement.getHead());
          }
        }
      }

      final TermStruct result;
      if (length == 1) {
        result = newStruct(this.getHead());
      } else {
        Term[] elements = new Term[length - 1];
        TermList lst = this.getTail();
        int index = 0;
        while (lst != Terms.NULL_LIST) {
          elements[index++] = lst.getHead();
          lst = lst.getTail();
        }
        result = newStruct(this.getHead(), elements);
      }
      return result;
    }
  }

  protected void doArrangeVars(final Map<String, TermVar> variables) {
    TermList list = this;
    while (!list.isNullList()) {
      list.getHead().doArrangeVars(variables);
      final Term tail = list.getTail();
      if (tail.getTermType() == LIST) {
        list = (TermList) tail;
      } else {
        tail.doArrangeVars(variables);
        break;
      }
    }
  }

  @Override
  public boolean hasVariableWithName(final String name) {
    if (this == Terms.NULL_LIST) {
      return false;
    }
    return getHead().hasVariableWithName(name) || getTail().hasVariableWithName(name);
  }
}
