package com.igormaznitsa.prol.kbase.inmemory.items;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;

final class ComplexRhsItem extends InMemoryItem {

  ComplexRhsItem(final TermStruct clause) {
    super(clause);
  }

  @Override
  public boolean matches(final Term rightHandSide) {
    return this.rightHandSide.makeClone().unifyTo(rightHandSide.makeClone());
  }
}
