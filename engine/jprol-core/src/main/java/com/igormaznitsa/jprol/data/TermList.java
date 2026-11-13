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
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newList;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.utils.ProlUtils.createOrAppendToList;
import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class TermList extends TermStruct {

  public static final TermList NULL_LIST = new TermList();
  private static final int INDEX_HEAD = 0;
  private static final int INDEX_TAIL = 1;

  private TermList() {
    this(EMPTY_ARRAY, SourcePosition.UNKNOWN);
  }

  TermList(final SourcePosition sourcePosition) {
    this(EMPTY_ARRAY, sourcePosition);
  }

  TermList(final Term term, final SourcePosition sourcePosition) {
    this(new Term[] {requireNonNull(term), NULL_LIST}, sourcePosition);
  }

  TermList(final Term head, final Term tail, final SourcePosition sourcePosition) {
    this(new Term[] {requireNonNull(head), requireNonNull(tail)}, sourcePosition);
  }

  private TermList(final Term[] elements,
                   final SourcePosition sourcePosition) {
    super(Terms.LIST_FUNCTOR, elements.length == 0 ? EMPTY_ARRAY : elements, sourcePosition,
        PredicateInvoker.NULL_INVOKER, "./2");
  }

  public static TermList listOf(final Term... elements) {
    return asList(Arrays.asList(elements), SourcePosition.UNKNOWN);
  }

  public static TermList listOf(final List<Term> elements) {
    return asList(elements, SourcePosition.UNKNOWN);
  }

  public static TermList asList(final List<Term> elements,
                                final SourcePosition sourcePosition) {
    if (elements.isEmpty()) {
      return Terms.newList(sourcePosition);
    } else if (elements.size() == 1) {
      return Terms.newList(elements.get(0), NULL_LIST, sourcePosition);
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
          curResult = createOrAppendToList(curResult, struct.getArgumentAt(li));
        }
        return result;
      }
      default:
        return newList(element);
    }
  }

  @Override
  public int getArity() {
    return 2;
  }

  @Override
  public Term replaceVar(final String varName, final Term targetTerm) {
    if (this.isNullList()) {
      return this;
    }

    final Term newHead = this.getHead().replaceVar(varName, targetTerm);
    final Term newTail = this.getTail().replaceVar(varName, targetTerm);

    if (newHead != this.getHead() || newTail != this.getTail()) {
      return Terms.newList(newHead, newTail, SourcePosition.UNKNOWN);
    } else {
      return this;
    }
  }

  public TermList sort(final Comparator<Term> comparator, final boolean removeDuplication) {
    final Term[] terms = this.toArray(false);
    Arrays.sort(terms, comparator);
    final TermList sortedList;
    if (terms.length > 1) {
      for (int i = terms.length - 1; i > 0; i--) {
        final Term term = terms[i];
        final Term termPrev = terms[i - 1];
        if (removeDuplication) {
          if (comparator.compare(term, termPrev) == 0) {
            terms[i] = null;
          }
        }
      }
      sortedList =
          TermList.listOf(
              Arrays.stream(terms).filter(Objects::nonNull).collect(Collectors.toList()));
    } else {
      sortedList = TermList.listOf(Arrays.asList(terms));
    }
    return sortedList;
  }

  public TermList reverse() {
    if (this.isNullList()) {
      return this;
    } else {
      TermList result = new TermList(this.getHead(), NULL_LIST, this.getSourcePosition());
      Term tail = this.getTail().tryGround();
      while (true) {
        if (tail.getTermType() == LIST) {
          final TermList thatList = (TermList) tail;
          if (thatList.isNullList()) {
            break;
          }
          result = new TermList(thatList.getHead(), result, result.getSourcePosition());
          tail = thatList.getTail();
        } else {
          break;
        }
      }
      return result;
    }
  }

  @Override
  public Spliterator<Term> spliteratorChildren() {
    return Spliterators.spliteratorUnknownSize(this.iterator(), 0);
  }

  @Override
  public Iterator<Term> iterator() {
    if (this.isNullList()) {
      return Collections.emptyIterator();
    }
    return new ListElementIterator(this);
  }

  @SuppressWarnings("unchecked")
  public <T extends Term> T getHead() {
    return (T) this.arguments[INDEX_HEAD];
  }

  @SuppressWarnings("unchecked")
  public <T extends Term> T getTail() {
    final Term tail = this.arguments[INDEX_TAIL];
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
    this.arguments[INDEX_TAIL] = requireNonNull(newTail, "Null is not allowed as list tail");
  }

  public int calculateLength() {
    int length = 0;
    TermList current = this;
    while (!current.isNullList()) {
      length++;
      final Term tail = current.arguments[INDEX_TAIL];
      if (tail.getTermType() != LIST) {
        break;
      }
      current = (TermList) tail;
    }
    return length;
  }

  @Override
  public Stream<TermVar> variables() {
    return this.isNullList() ? Stream.empty() : super.variables();
  }

  @Override
  public Term makeClone() {
    return this.arguments.length == 0 ? this : this.makeClone(TermVar.newVariableMap());
  }

  @Override
  public boolean isNullList() {
    return this.arguments == EMPTY_ARRAY;
  }

  @Override
  public Term cloneAndReplaceVariables(final Map<TermVar, TermVar> variables) {
    if (this.isNullList()) {
      return NULL_LIST;
    }

    final Term firstHead = this.getHead();
    final Term firstTail = this.getTail();
    final Term firstHeadClone = firstHead.cloneAndReplaceVariables(variables);

    if (firstTail.getTermType() != LIST) {
      final Term tailClone = firstTail.cloneAndReplaceVariables(variables);
      return firstHead == firstHeadClone && firstTail == tailClone
          ? this
          : newList(firstHeadClone, tailClone, this.getSourcePosition());
    }

    final TermList secondCell = (TermList) firstTail;
    if (secondCell.isNullList()) {
      return firstHead == firstHeadClone
          ? this
          : newList(firstHeadClone, NULL_LIST, this.getSourcePosition());
    }

    TermList[] spineCells = new TermList[8];
    Term[] clonedHeads = new Term[8];
    spineCells[0] = this;
    clonedHeads[0] = firstHeadClone;
    int count = 1;

    TermList current = secondCell;
    Term terminalTail;
    while (true) {
      if (count == spineCells.length) {
        final int grown = spineCells.length << 1;
        spineCells = Arrays.copyOf(spineCells, grown);
        clonedHeads = Arrays.copyOf(clonedHeads, grown);
      }
      spineCells[count] = current;
      clonedHeads[count] = current.getHead().cloneAndReplaceVariables(variables);
      count++;

      final Term nextTail = current.getTail();
      if (nextTail.getTermType() != LIST) {
        terminalTail = nextTail.cloneAndReplaceVariables(variables);
        break;
      }
      final TermList nextList = (TermList) nextTail;
      if (nextList.isNullList()) {
        terminalTail = NULL_LIST;
        break;
      }
      current = nextList;
    }

    Term rebuiltTail = terminalTail;
    for (int i = count - 1; i >= 0; i--) {
      final TermList origCell = spineCells[i];
      final Term clonedHead = clonedHeads[i];
      if (clonedHead == origCell.getHead() && rebuiltTail == origCell.getTail()) {
        rebuiltTail = origCell;
      } else {
        rebuiltTail = newList(clonedHead, rebuiltTail, origCell.getSourcePosition());
      }
    }
    return rebuiltTail;
  }

  @Override
  public boolean isGround() {
    TermList current = this;
    while (!current.isNullList()) {
      if (!current.getHead().isGround()) {
        return false;
      }
      final Term tail = current.getTail();
      if (tail.getTermType() != LIST) {
        return tail.isGround();
      }
      current = (TermList) tail;
    }
    return true;
  }

  @Override
  public boolean isUnground() {
    TermList current = this;
    while (!current.isNullList()) {
      if (current.getHead().isUnground()) {
        return true;
      }
      final Term tail = current.getTail();
      if (tail.getTermType() != LIST) {
        return tail.isUnground();
      }
      current = (TermList) tail;
    }
    return false;
  }

  @Override
  public TermType getTermType() {
    return TermType.LIST;
  }

  @Override
  protected Term makeClone(final Map<TermVar, TermVar> variables) {
    if (this.isNullList()) {
      return NULL_LIST;
    }
    final Term firstHead = this.getHead().makeClone(variables);
    final Term firstTail = this.getTail();
    if (firstTail.getTermType() != LIST) {
      return Terms.newList(firstHead, firstTail.makeClone(variables));
    }
    TermList tailList = (TermList) firstTail;
    if (tailList.isNullList()) {
      return Terms.newList(firstHead, NULL_LIST);
    }

    final TermList result = Terms.newList(firstHead, NULL_LIST);
    TermList cursor = result;
    while (true) {
      final Term clonedHead = tailList.getHead().makeClone(variables);
      final TermList cell = Terms.newList(clonedHead, NULL_LIST);
      cursor.arguments[INDEX_TAIL] = cell;
      cursor = cell;

      final Term nextTail = tailList.getTail();
      if (nextTail.getTermType() != LIST) {
        cursor.arguments[INDEX_TAIL] = nextTail.makeClone(variables);
        break;
      }
      tailList = (TermList) nextTail;
      if (tailList.isNullList()) {
        break;
      }
    }
    return result;
  }

  @Override
  public String toString() {
    if (isNullList()) {
      return "[]";
    }
    final StringBuilder builder = new StringBuilder("[");

    boolean notFirst = false;
    Term list = this;

    while (list != NULL_LIST) {
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

    while (!list.isNullList()) {
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

  public Term findLastTail() {
    if (this.isNullList()) {
      return NULL_LIST;
    }
    TermList curList = this;
    while (true) {
      Term tail = curList.getTail();
      if (tail == NULL_LIST || tail.getTermType() != LIST) {
        return tail;
      }
      curList = (TermList) tail;
    }
  }

  public void replaceLastElement(final Term newLastElement) {
    if (isNullList()) {
      throw new ProlCriticalError("Attempt to change Null list");
    }
    TermList curList = this;
    while (true) {
      Term tail = curList.getTail();
      if (tail == NULL_LIST || tail.getTermType() != LIST) {
        curList.setTail(newLastElement);
        break;
      }
      curList = (TermList) tail;
    }
  }

  @Override
  public boolean unifyWith(final Term atom) {
    if (this == atom) {
      return true;
    }

    Term currentLeft = this;
    Term currentRight = atom;
    while (currentLeft != currentRight) {
      if (currentLeft.getTermType() == LIST && currentRight.getTermType() == LIST) {
        final TermList leftList = (TermList) currentLeft;
        final TermList rightList = (TermList) currentRight;
        if (leftList.isNullList()) {
          return rightList.isNullList();
        }
        if (rightList.isNullList()) {
          return false;
        }
        if (!leftList.getHead().unifyWith(rightList.getHead())) {
          return false;
        }
        currentLeft = leftList.getTail();
        currentRight = rightList.getTail();
      } else if (currentLeft == this) {
        if (currentRight.getTermType() == VAR) {
          final TermVar thatVariable = (TermVar) currentRight;
          final Term value = thatVariable.getValue();
          if (value == null) {
            thatVariable.setImmediateValue(this);
            return true;
          }
          if (value.getTermType() == VAR) {
            ((TermVar) value).setImmediateValue(this);
            return true;
          }
          currentRight = value;
          continue;
        }
        return false;
      } else {
        return currentLeft.unifyWith(currentRight);
      }
    }
    return true;
  }

  @Override
  public boolean isUnifiableWith(final Term target) {
    if (this == target) {
      return true;
    }

    Term currentLeft = this;
    Term currentRight = target;
    while (currentLeft != currentRight) {
      if (currentLeft.getTermType() == VAR) {
        currentLeft = ((TermVar) currentLeft).getValue();
        if (currentLeft == null || currentLeft.getTermType() == VAR) {
          return true;
        }
        if (currentLeft == currentRight) {
          return true;
        }
      }
      if (currentRight.getTermType() == VAR) {
        currentRight = ((TermVar) currentRight).getValue();
        if (currentRight == null || currentRight.getTermType() == VAR) {
          return true;
        }
        if (currentLeft == currentRight) {
          return true;
        }
      }
      if (currentLeft.getTermType() != LIST || currentRight.getTermType() != LIST) {
        return currentLeft != this && currentLeft.isUnifiableWith(currentRight);
      }
      final TermList leftList = (TermList) currentLeft;
      final TermList rightList = (TermList) currentRight;
      if (leftList.isNullList()) {
        return rightList.isNullList();
      }
      if (rightList.isNullList()) {
        return false;
      }
      if (!leftList.getHead().isUnifiableWith(rightList.getHead())) {
        return false;
      }
      currentLeft = leftList.getTail();
      currentRight = rightList.getTail();
    }
    return true;
  }

  @Override
  public String forWrite() {
    if (isNullList()) {
      return "[]";
    }
    StringBuilder builder = new StringBuilder("[");

    boolean notFirst = false;
    Term list = this;

    while (list != NULL_LIST) {
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

  public Term[] toArray(final boolean includeNonListTail) {
    if (this.isNullList()) {
      return EMPTY_ARRAY;
    }

    final ArrayList<Term> arraylist = new ArrayList<>();
    TermList currentList = this;
    while (true) {
      if (currentList.isNullList()) {
        break;
      }
      arraylist.add(currentList.getHead());
      final Term nextList = currentList.getTail();
      if (nextList.getTermType() == LIST) {
        currentList = (TermList) nextList;
      } else {
        if (includeNonListTail) {
          arraylist.add(nextList);
        }
        break;
      }
    }
    return arraylist.toArray(EMPTY_ARRAY);
  }

  public Term toAtom() {
    if (this.isNullList()) {
      return new Term("<empty>", this.getSourcePosition());
    }
    if (this.getTail().isNullList()) {
      return this.getHead();
    } else {
      final Term functor = this.getHead().tryGround();
      final int length = this.calculateLength();
      if (length == 3) {
        if (functor == Terms.LIST_FUNCTOR) {
          final TermList secondElement = this.getTail();
          final TermList thirdElement = secondElement.getTail();

          if (thirdElement.getHead().getTermType() == LIST) {
            return newList(secondElement.getHead(), thirdElement.getHead());
          }
        }
      }

      final TermStruct result;
      if (length == 1) {
        result = newStruct(functor);
      } else {
        Term[] elements = new Term[length - 1];
        TermList lst = this.getTail();
        int index = 0;
        while (lst != NULL_LIST) {
          elements[index++] = lst.getHead();
          lst = lst.getTail();
        }
        result = newStruct(functor, elements);
      }
      return result;
    }
  }

  protected void arrangeVariableValues(final Map<String, TermVar> variables) {
    TermList list = this;
    while (!list.isNullList()) {
      list.getHead().arrangeVariableValues(variables);
      final Term tail = list.getTail();
      if (tail.getTermType() == LIST) {
        list = (TermList) tail;
      } else {
        tail.arrangeVariableValues(variables);
        break;
      }
    }
  }

  @Override
  public boolean containsNamedVariable(final String name) {
    TermList current = this;
    while (!current.isNullList()) {
      if (current.getHead().containsNamedVariable(name)) {
        return true;
      }
      final Term tail = current.getTail();
      if (tail.getTermType() != LIST) {
        return tail.containsNamedVariable(name);
      }
      current = (TermList) tail;
    }
    return false;
  }

  public boolean doesContainOnlyCharCodes() {
    TermList current = this;
    while (current != null) {
      final Term term = current.getHead().tryGround();
      if (term.getTermType() != ATOM) {
        return false;
      }

      if (term instanceof TermLong) {
        final long code = term.toNumber().longValue();
        if ((code & 0xFFFFFFFF00000000L) == 0L) {
          final int charCode = (int) (code & 0xFFFFFFFFL);
          if (!Character.isValidCodePoint(charCode)) {
            return false;
          }
        } else {
          return false;
        }
      } else if (term.getText().length() != 1) {
        return false;
      }

      final Term tail = current.getTail().tryGround();
      if (tail.getTermType() == VAR) {
        return true;
      }
      if (tail.getTermType() == LIST) {
        final TermList newList = (TermList) tail;
        current = newList.isNullList() ? null : newList;
      } else {
        return false;
      }
    }
    return true;
  }

  private static final class ListElementIterator implements Iterator<Term> {

    private Term current;

    private ListElementIterator(final TermList start) {
      this.current = start;
    }

    private void advance() {
      if (this.current == null) {
        throw new NoSuchElementException();
      }
      if (this.current.getTermType() == LIST) {
        final TermList asList = (TermList) this.current;
        this.current = asList.isNullList() ? null : asList.getTail();
      } else {
        this.current = null;
      }
    }

    @Override
    public boolean hasNext() {
      return this.current != null
          && this.current.getTermType() == LIST
          && !this.current.isNullList();
    }

    @Override
    public Term next() {
      final Term result = this.current;
      this.advance();
      if (result.getTermType() == LIST) {
        final TermList asList = (TermList) result;
        if (asList.isNullList()) {
          throw new NoSuchElementException();
        }
        return asList.getHead();
      }
      return result;
    }
  }
}
