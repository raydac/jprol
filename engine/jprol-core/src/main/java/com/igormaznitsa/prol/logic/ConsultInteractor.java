package com.igormaznitsa.prol.logic;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermVar;

import java.util.Map;

public interface ConsultInteractor {
  boolean onFoundInteractiveGoal(ProlContext context, Term goal);

  boolean onSolution(ProlContext context, Term goal, Map<String, TermVar> varValues, int solutionCounter);

  void onFail(ProlContext context, Term goal, int foundSolutionCounter);
}