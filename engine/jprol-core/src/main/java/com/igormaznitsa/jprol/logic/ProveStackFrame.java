package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;

final class ProveStackFrame {

  final ProveStackFrame prev;
  final JProlChoicePoint choicePoint;

  JProlChoicePoint pendingResumeGoal;
  boolean subgoalOutcomePending;
  Term pendingSubgoalResult;

  ProveStackFrame(final ProveStackFrame prev, final JProlChoicePoint choicePoint) {
    this.prev = prev;
    this.choicePoint = choicePoint;
  }
}
