package com.igormaznitsa.jprol.exceptions;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

import static com.igormaznitsa.jprol.data.Terms.newAtom;

public class ProlChoicePointInterruptedException extends ProlAbstractCatchableException {

  private static final Term ERROR_TERM = newAtom("interrupted_error");
  private final JProlChoicePoint choicePoint;

  public ProlChoicePointInterruptedException(final JProlChoicePoint choicePoint, final Term culprit) {
    super(culprit);
    this.choicePoint = choicePoint;
  }

  public JProlChoicePoint getChoicePoint() {
    return this.choicePoint;
  }

  @Override
  public Term getErrorTerm() {
    return this.getCulprit();
  }

  @Override
  public TermStruct getAsStruct() {
    return this.makeErrorStruct(ERROR_TERM,
            TermList.asTermList(ERROR_TERM, newAtom("interruption"), this.getCulprit()));
  }
}
