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
import com.igormaznitsa.prol.data.TermVar;
import com.igormaznitsa.prol.data.Terms;
import com.igormaznitsa.prol.trace.TracingChoicePointListener;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class DeferredGoal {

  private String goalAsText;
  private ProlContext context;
  private Term parsedGoal;
  private List<String> orderedVars;

  public DeferredGoal(final Term goal, final ProlContext workContext) {
    if (goal == null || workContext == null) {
      throw new NullPointerException("Null argument detected");
    }
    setContext(workContext);
    setParsedGoal(goal);
    setOrderedVars(Collections.emptyList());
  }

  public DeferredGoal(final String goal, final ProlContext workContext) throws IOException {
    this(goal, workContext, null);
  }

  public DeferredGoal(final String goal, final ProlContext workContext, final TracingChoicePointListener tracer) throws IOException {
    if (goal == null || workContext == null) {
      throw new NullPointerException("Needed argument is null");
    }
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

    final ProlTreeBuilder treebuilder = new ProlTreeBuilder(this.context);
    setParsedGoal(treebuilder.readPhraseAndMakeTree(new StringReader(getGoalAsText())).term);
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

  public final Term processGoalOnce() {
    final Term target = getParsedGoal().makeClone();
    final ChoicePoint goal = new ChoicePoint(target, getContext());
    return goal.next();
  }

  public final ChoicePoint forIntegerParameters(final long... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = Terms.newLong(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  public final ChoicePoint forStringParameters(final String... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = Terms.newAtom(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  public final ChoicePoint forFloatParameters(final float... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = Terms.newDouble(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  public final ChoicePoint forParameters(final Term... parameters) {
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
      final TermVar varTerm = goalClone.variables().filter(x -> varName.equals(x.getText())).findFirst().orElse(null);
      if (varTerm == null) {
        throw new IllegalArgumentException("Can't find a variable for \'" + varName + "\' name");
      }
      varTerm.setValue(parameter);
    }

    return new ChoicePoint(goalClone, getContext());
  }

  public final ChoicePoint forParameters(final Map<String, Term> vars) {
    if (vars == null) {
      throw new NullPointerException();
    }

    final Term goalClone = getParsedGoal().makeClone();

    vars.forEach((key, value) -> {
      final TermVar foundVar = goalClone.variables().filter(x -> key.equals(x.getText())).findFirst().orElse(null);
      if (foundVar == null) {
        throw new IllegalArgumentException("Can't find variable for \'" + key + "\' name");
      }
      foundVar.setValue(value);
    });

    return new ChoicePoint(goalClone, getContext());
  }

  public final ChoicePoint getNonparametrizedGoalInstance() {
    return new ChoicePoint(getParsedGoal(), getContext());
  }
}
