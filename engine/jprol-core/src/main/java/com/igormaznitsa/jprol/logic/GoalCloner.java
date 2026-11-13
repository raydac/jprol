package com.igormaznitsa.jprol.logic;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.VAR;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import java.util.Arrays;
import java.util.IdentityHashMap;

final class GoalCloner {

  private static final int INITIAL_PAIRS_LENGTH = 16;
  private static final int MAX_PAIRS_LENGTH = 32;

  private TermVar[] pairs;
  private int pairCount;
  private IdentityHashMap<TermVar, TermVar> overflow;

  static Term cloneUnboundVariables(final Term goal) {
    if (!goal.canContainVariables()) {
      return goal;
    }
    return new GoalCloner().cloneTerm(goal);
  }

  private static TermVar createFreshVar(final TermVar source) {
    return source.isAnonymous()
        ? Terms.newAnonymousVar(source.getSourcePosition())
        : Terms.newVar(source.getText(), source.getSourcePosition());
  }

  private Term cloneTerm(final Term term) {
    switch (term.getTermType()) {
      case VAR:
        return this.cloneVariable((TermVar) term);
      case LIST:
        return this.cloneList((TermList) term);
      case STRUCT:
        return this.cloneStruct((TermStruct) term);
      default:
        return term;
    }
  }

  private Term cloneVariable(final TermVar var) {
    TermVar current = var;
    while (true) {
      final Term next = current.getImmediateValue();
      if (next == null) {
        return this.resolveClonedVar(current);
      }
      if (next.getTermType() != VAR) {
        final Term clonedValue = this.cloneTerm(next);
        return clonedValue == next ? var : clonedValue;
      }
      current = (TermVar) next;
    }
  }

  private TermVar resolveClonedVar(final TermVar unbound) {
    if (this.overflow != null) {
      TermVar found = this.overflow.get(unbound);
      if (found == null) {
        found = createFreshVar(unbound);
        this.overflow.put(unbound, found);
      }
      return found;
    }

    TermVar[] localPairs = this.pairs;
    if (localPairs == null) {
      localPairs = new TermVar[INITIAL_PAIRS_LENGTH];
      final TermVar created = createFreshVar(unbound);
      localPairs[0] = unbound;
      localPairs[1] = created;
      this.pairs = localPairs;
      this.pairCount = 1;
      return created;
    }

    final int limit = this.pairCount << 1;
    for (int i = 0; i < limit; i += 2) {
      if (localPairs[i] == unbound) {
        return localPairs[i + 1];
      }
    }

    final TermVar created = createFreshVar(unbound);
    if (limit == localPairs.length) {
      if (localPairs.length < MAX_PAIRS_LENGTH) {
        localPairs = Arrays.copyOf(localPairs, MAX_PAIRS_LENGTH);
        this.pairs = localPairs;
      } else {
        final IdentityHashMap<TermVar, TermVar> promoted = new IdentityHashMap<>(32);
        for (int i = 0; i < limit; i += 2) {
          promoted.put(localPairs[i], localPairs[i + 1]);
        }
        promoted.put(unbound, created);
        this.pairs = null;
        this.overflow = promoted;
        return created;
      }
    }

    localPairs[limit] = unbound;
    localPairs[limit + 1] = created;
    this.pairCount++;
    return created;
  }

  private Term cloneStruct(final TermStruct struct) {
    final int arity = struct.getArity();
    if (arity == 0) {
      return struct;
    }

    Term[] clonedArgs = null;
    for (int i = 0; i < arity; i++) {
      final Term origArg = struct.getArgumentAt(i);
      final Term clonedArg = this.cloneTerm(origArg);
      if (clonedArgs != null) {
        clonedArgs[i] = clonedArg;
      } else if (clonedArg != origArg) {
        clonedArgs = new Term[arity];
        for (int j = 0; j < i; j++) {
          clonedArgs[j] = struct.getArgumentAt(j);
        }
        clonedArgs[i] = clonedArg;
      }
    }

    if (clonedArgs == null) {
      return struct;
    }
    return Terms.newStruct(
        struct.getFunctor(),
        clonedArgs,
        struct.getPredicateProcessor(),
        struct.getSourcePosition()
    );
  }

  private Term cloneList(final TermList list) {
    if (list.isNullList()) {
      return NULL_LIST;
    }

    final Term firstHead = list.getHead();
    final Term firstHeadClone = this.cloneTerm(firstHead);
    final Term firstTail = list.getTail();

    if (firstTail.getTermType() != LIST) {
      final Term tailClone = this.cloneTerm(firstTail);
      return firstHead == firstHeadClone && firstTail == tailClone
          ? list
          : Terms.newList(firstHeadClone, tailClone, list.getSourcePosition());
    }

    final TermList secondCell = (TermList) firstTail;
    if (secondCell.isNullList()) {
      return firstHead == firstHeadClone
          ? list
          : Terms.newList(firstHeadClone, NULL_LIST, list.getSourcePosition());
    }

    TermList[] spineCells = new TermList[8];
    Term[] clonedHeads = new Term[8];
    spineCells[0] = list;
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
      clonedHeads[count] = this.cloneTerm(current.getHead());
      count++;

      final Term nextTail = current.getTail();
      if (nextTail.getTermType() != LIST) {
        terminalTail = this.cloneTerm(nextTail);
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
        rebuiltTail = Terms.newList(clonedHead, rebuiltTail, origCell.getSourcePosition());
      }
    }
    return rebuiltTail;
  }
}
