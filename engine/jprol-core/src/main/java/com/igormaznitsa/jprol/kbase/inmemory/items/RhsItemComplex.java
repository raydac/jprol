package com.igormaznitsa.jprol.kbase.inmemory.items;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;

final class RhsItemComplex extends InMemoryItem {

  RhsItemComplex(final TermStruct clause) {
    super(clause);
  }

  @Override
  public boolean matches(final Term rightHandSide) {
    return this.head.makeClone().unifyWith(rightHandSide.makeClone());
  }
}
