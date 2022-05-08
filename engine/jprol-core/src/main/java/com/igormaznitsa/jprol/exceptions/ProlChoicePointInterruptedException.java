package com.igormaznitsa.jprol.exceptions;

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
}
