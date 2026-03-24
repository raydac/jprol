package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;

final class ProveStackFrame {
  final ProveStackFrame prev;
  final JProlChoicePoint entry;
  JProlChoicePoint pendingResumeGoal;
  boolean subgoalOutcomePending;
  Term pendingSubgoalResult;

  ProveStackFrame(final ProveStackFrame prev, final JProlChoicePoint entry) {
    this.prev = prev;
    this.entry = entry;
  }
}
