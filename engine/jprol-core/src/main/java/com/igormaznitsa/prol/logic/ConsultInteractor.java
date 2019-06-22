package com.igormaznitsa.prol.logic;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermVar;

import java.util.Map;

public interface ConsultInteractor {
  boolean onFoundGoal(Term goal);

  boolean onSolution(Term goal, Map<String, TermVar> varValues, int solutionCounter);

  void onFail(Term goal, int foundSolutionCounter);
}