package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;

final class ProveStackFrame {

  JProlChoicePoint choicePoint;
  JProlChoicePoint pendingResumeGoal;
  boolean subgoalOutcomePending;
  Term pendingSubgoalResult;

  void prepare(final JProlChoicePoint choicePoint) {
    this.choicePoint = choicePoint;
    this.pendingResumeGoal = null;
    this.subgoalOutcomePending = false;
    this.pendingSubgoalResult = null;
  }

  void watchChild(final JProlChoicePoint resumeGoal) {
    this.pendingResumeGoal = resumeGoal;
    this.subgoalOutcomePending = false;
    this.pendingSubgoalResult = null;
  }

  void acceptSubgoal(final Term result) {
    this.subgoalOutcomePending = true;
    this.pendingSubgoalResult = result;
  }

  void clearOutcome() {
    this.pendingResumeGoal = null;
    this.subgoalOutcomePending = false;
    this.pendingSubgoalResult = null;
  }

  void release() {
    this.choicePoint = null;
    this.pendingResumeGoal = null;
    this.pendingSubgoalResult = null;
    this.subgoalOutcomePending = false;
  }
}
