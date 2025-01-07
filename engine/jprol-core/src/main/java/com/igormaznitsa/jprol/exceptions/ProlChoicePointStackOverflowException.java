package com.igormaznitsa.jprol.exceptions;

import com.igormaznitsa.jprol.logic.JProlChoicePoint;

public class ProlChoicePointStackOverflowException extends ProlChoicePointInterruptedException {

  public ProlChoicePointStackOverflowException(final String text,
                                               final JProlChoicePoint choicePoint) {
    super(text, choicePoint);
  }

}
