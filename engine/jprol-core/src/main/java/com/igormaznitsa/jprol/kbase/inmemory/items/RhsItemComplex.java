package com.igormaznitsa.jprol.kbase.inmemory.items;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.kbase.KnowledgeContext;

final class RhsItemComplex extends InMemoryItem {

  RhsItemComplex(final KnowledgeContext knowledgeContext, final TermStruct clause) {
    super(knowledgeContext, clause);
  }

  @Override
  public boolean matches(final Term rightHandSide) {
    return this.rightHandSide.makeClone().unifyTo(rightHandSide.makeClone());
  }
}
