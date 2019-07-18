package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermVar;

import java.util.Map;

public interface ConsultInteractor {
  boolean onFoundInteractiveGoal(JProlContext context, Term goal);

  boolean onSolution(JProlContext context, Term goal, Map<String, TermVar> varValues, int solutionCounter);

  void onFail(JProlContext context, Term goal, int foundSolutionCounter);
}