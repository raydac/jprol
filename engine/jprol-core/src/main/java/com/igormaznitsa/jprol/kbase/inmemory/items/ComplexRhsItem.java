package com.igormaznitsa.jprol.kbase.inmemory.items;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;

final class ComplexRhsItem extends InMemoryItem {

  ComplexRhsItem(final TermStruct clause) {
    super(clause);
  }

  @Override
  public boolean matches(final Term rightHandSide) {
    return this.rightHandSide.makeClone().unifyTo(rightHandSide.makeClone());
  }
}
