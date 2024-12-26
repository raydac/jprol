package com.igormaznitsa.jprol.exceptions;

import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

public class ProlChoicePointInterruptedException extends ProlInterruptException {

  private final JProlChoicePoint choicePoint;

  public ProlChoicePointInterruptedException(final String text, final JProlChoicePoint choicePoint) {
    super(text, null);
    this.choicePoint = choicePoint;
  }

  public JProlChoicePoint getChoicePoint() {
    return this.choicePoint;
  }

  @Override
  public SourcePosition getSourcePosition() {
    return this.choicePoint == null ? SourcePosition.UNKNOWN :
        this.choicePoint.getGoalTerm().getSourcePosition();
  }
}
