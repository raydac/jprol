package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;

final class ProveStackFrame {

  final JProlChoicePoint choicePoint;
  final JProlChoicePoint pendingResumeGoal;
  final boolean subgoalOutcomePending;
  final Term pendingSubgoalResult;

  private ProveStackFrame(
      final JProlChoicePoint choicePoint,
      final JProlChoicePoint pendingResumeGoal,
      final boolean subgoalOutcomePending,
      final Term pendingSubgoalResult) {
    this.choicePoint = choicePoint;
    this.pendingResumeGoal = pendingResumeGoal;
    this.subgoalOutcomePending = subgoalOutcomePending;
    this.pendingSubgoalResult = pendingSubgoalResult;
  }

  static ProveStackFrame fresh(final JProlChoicePoint choicePoint) {
    return new ProveStackFrame(choicePoint, null, false, null);
  }

  ProveStackFrame withPendingResumeGoal(final JProlChoicePoint resumeGoal) {
    return new ProveStackFrame(this.choicePoint, resumeGoal, false, null);
  }

  ProveStackFrame withSubgoalReturned(final Term result) {
    return new ProveStackFrame(
        this.choicePoint,
        this.pendingResumeGoal,
        true,
        result);
  }

  ProveStackFrame clearedSubgoalOutcome() {
    return new ProveStackFrame(this.choicePoint, null, false, null);
  }
}
