package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermVar;

import java.util.Map;

public interface ConsultInteractor {
  boolean onFoundInteractiveGoal(ProlContext context, Term goal);

  boolean onSolution(ProlContext context, Term goal, Map<String, TermVar> varValues, int solutionCounter);

  void onFail(ProlContext context, Term goal, int foundSolutionCounter);
}