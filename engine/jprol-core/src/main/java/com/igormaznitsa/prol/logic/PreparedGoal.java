/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prol.logic;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermFloat;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.parser.ProlTreeBuilder;
import com.igormaznitsa.prol.trace.TraceListener;
import com.igormaznitsa.prol.utils.Utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class PreparedGoal {

  private final TraceListener tracer;
  private String goalAsText;
  private ProlContext context;
  private Term parsedGoal;
  private List<String> orderedVars;

  public PreparedGoal(final Term goal, final ProlContext workContext) {
    this(goal, workContext, null);
  }

  @SuppressWarnings("unchecked")
  public PreparedGoal(final Term goal, final ProlContext workContext, final TraceListener tracer) {
    if (goal == null || workContext == null) {
      throw new NullPointerException("Null argument detected");
    }
    this.tracer = tracer;
    setContext(workContext);
    setParsedGoal(goal);
    setOrderedVars(Collections.EMPTY_LIST);
  }

  public PreparedGoal(final String goal, final ProlContext workContext) throws IOException {
    this(goal, workContext, null);
  }

  public PreparedGoal(final String goal, final ProlContext workContext, final TraceListener tracer) throws IOException {
    if (goal == null || workContext == null) {
      throw new NullPointerException("Needed argument is null");
    }
    this.tracer = tracer;
    setContext(workContext);

    // find ordered vars
    final List<String> parametrizedNames = new ArrayList<>();

    final StringBuilder builder = new StringBuilder(goal);

    int varIndex = 1000;
    while (!Thread.currentThread().isInterrupted()) {
      final int indexOfOrderedVar = builder.indexOf("{?}");
      if (indexOfOrderedVar < 0) {
        break;
      }
      // found
      final String varName = "_Ord" + varIndex;
      varIndex++;

      builder.delete(indexOfOrderedVar, indexOfOrderedVar + 3).insert(indexOfOrderedVar, ' ' + varName + ' ');
      parametrizedNames.add(varName);
    }
    setGoalAsText(builder.toString());
    setOrderedVars(parametrizedNames);

    final ProlTreeBuilder treebuilder = new ProlTreeBuilder(getContext());
    setParsedGoal(treebuilder.readPhraseAndMakeTree(getGoalAsText()));
  }

  protected final List<String> getOrderedVars() {
    return this.orderedVars;
  }

  protected final void setOrderedVars(final List<String> list) {
    this.orderedVars = list;
  }

  public final Term getParsedGoal() {
    return this.parsedGoal;
  }

  private void setParsedGoal(final Term goal) {
    this.parsedGoal = goal;
  }

  public final ProlContext getContext() {
    return this.context;
  }

  private void setContext(final ProlContext context) {
    this.context = context;
  }

  public final String getGoalAsText() {
    return this.goalAsText;
  }

  private void setGoalAsText(final String text) {
    this.goalAsText = text;
  }

  public final Term processGoalOnce() throws InterruptedException {
    final Term target = getParsedGoal().makeClone();
    final Goal goal = new Goal(target, getContext(), tracer);
    return goal.solve();
  }

  public final Goal forIntegerParameters(final int... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = new TermInteger(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  public final Goal forStringParameters(final String... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = new Term(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  public final Goal forFloatParameters(final float... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = new TermFloat(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  public final Goal forParameters(final Term... parameters) {
    if (orderedVars.size() != parameters.length) {
      throw new IllegalArgumentException("Incompatible parameter number [" + orderedVars.size() + "!=" + parameters.length);
    }

    final Term goalClone = getParsedGoal().makeClone();

    final List<String> varNames = getOrderedVars();

    final int len = parameters.length;
    for (int li = 0; li < len; li++) {
      final String varName = varNames.get(li);
      final Term parameter = parameters[li];
      if (parameter == null) {
        throw new NullPointerException();
      }
      final Var varTerm = Utils.findVarInsideTerm(goalClone, varName);
      if (varTerm == null) {
        throw new IllegalArgumentException("Can't find a variable for \'" + varName + "\' name");
      }
      varTerm.setValue(parameter);
    }

    return new Goal(goalClone, getContext(), tracer);
  }

  public final Goal forParameters(final Map<String, Term> vars) {
    if (vars == null) {
      throw new NullPointerException();
    }

    final Term goalClone = getParsedGoal().makeClone();

    vars.entrySet().forEach((entry) -> {
        final Term value = entry.getValue();
        final Var foundVar = Utils.findVarInsideTerm(goalClone, entry.getKey());
        if (foundVar == null) {
            throw new IllegalArgumentException("Can't find variable for \'" + entry.getKey() + "\' name");
        }
        foundVar.setValue(value);
      });

    return new Goal(goalClone, getContext(), tracer);
  }

  public final Goal getNonparametrizedGoalInstance() {
    return new Goal(getParsedGoal(), getContext(), tracer);
  }
}
