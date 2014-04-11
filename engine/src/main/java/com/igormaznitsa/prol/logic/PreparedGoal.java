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
import java.util.Map.Entry;

/**
 * This class allows to prepare a goal for faster multiuse, also it gives
 * possibility to make parametrized goals
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class PreparedGoal {

  /**
   * The field saves prepared goal text (all {?} parameters will be changed to
   * inside form)
   */
  private String goalAsText;
  /**
   * The filed saves the work context for the goal
   */
  private ProlContext context;
  /**
   * The term is the parsed version of the goal text
   */
  private Term parsedGoal;
  /**
   * The list contains ordered inside names of parametrized variables
   */
  private List<String> orderedVars;

  /**
   * The variable contains tracer for the wrapped goal, can be null
   */
  private final TraceListener tracer;

  /**
   * Get the list of parametrized variable names, it is used for inside needs
   *
   * @return a list object containing ordered list of parametrized variables,
   * will not be null
   */
  protected final List<String> getOrderedVars() {
    return this.orderedVars;
  }

  /**
   * Set the list of parametrized var names, must not be null
   *
   * @param list a list containing ordered parametrized var names
   */
  protected final void setOrderedVars(final List<String> list) {
    this.orderedVars = list;
  }

  /**
   * Get the term representing parsed goal
   *
   * @return a term pbject which containing a tree of parsed original goal text
   */
  public final Term getParsedGoal() {
    return this.parsedGoal;
  }

  /**
   * Set the term representing parsed goal
   *
   * @param goal a term object containing parsed tree of original goal text,
   * must not be null
   */
  private final void setParsedGoal(final Term goal) {
    this.parsedGoal = goal;
  }

  /**
   * Get the work context for the goal
   *
   * @return the work prol context for the goal, must not be null
   */
  public final ProlContext getContext() {
    return this.context;
  }

  /**
   * Set the work prol context for the goal
   *
   * @param context the work prol context for the goal, must not be null
   */
  private final void setContext(final ProlContext context) {
    this.context = context;
  }

  /**
   * Get prepared (all {?} changed to inside var names) goal text
   *
   * @return the prepared goal text as String object, must not be null
   */
  public final String getGoalAsText() {
    return this.goalAsText;
  }

  /**
   * Set prepared goal text
   *
   * @param text a String object contains prepared goal text, must not be null
   */
  private final void setGoalAsText(final String text) {
    this.goalAsText = text;
  }

  /**
   * A constructor allows to make the prepared goal on already parsed goal, of
   * course without support any inside parametrized variables
   *
   * @param goal the parsed goal, must not be null
   * @param workContext the work context for the goal, must not be null
   * @throws NullPointerException it will be thrown if there is any null pointer
   * at argument list
   */
  public PreparedGoal(final Term goal, final ProlContext workContext) {
    this(goal, workContext, null);
  }

  /**
   * A constructor allows to make the prepared goal on already parsed goal, of
   * course without support any inside parametrized variables
   *
   * @param goal the parsed goal, must not be null
   * @param workContext the work context for the goal, must not be null
   * @param tracer the tracer for wrapped goal, can be null
   * @throws NullPointerException it will be thrown if there is any null pointer
   * at argument list
   */
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

  /**
   * A constructor. You can insert {?} parameters inside the goal text and these
   * parts will be changed to named inside vars, thus you will be able make
   * parametrized versions of the Goal through special functions of the class.
   * Be carefully in use of {?} strings they are used as macroses by the
   * constructor.
   *
   * @param goal the goal text, mist not be null
   * @param workContext the context which will be used for the goal, must not be
   * null
   * @throws IOException it will be thrown if there is any exception during
   * parsing
   * @throws NullPointerException if either the goal or the workContext (or
   * both) is null
   * @see ProlContext
   */
  public PreparedGoal(final String goal, final ProlContext workContext) throws IOException {
    this(goal, workContext, null);
  }

  /**
   * A constructor. You can insert {?} parameters inside the goal text and these
   * parts will be changed to named inside vars, thus you will be able make
   * parametrized versions of the Goal through special functions of the class.
   * Be carefully in use of {?} strings they are used as macroses by the
   * constructor.
   *
   * @param goal the goal text, mist not be null
   * @param workContext the context which will be used for the goal, must not be
   * null
   * @param tracer the tracer to be used for wrapped goal, can be null
   * @throws IOException it will be thrown if there is any exception during
   * parsing
   * @throws NullPointerException if either the goal or the workContext (or
   * both) is null
   * @see ProlContext
   */
  public PreparedGoal(final String goal, final ProlContext workContext, final TraceListener tracer) throws IOException {
    if (goal == null || workContext == null) {
      throw new NullPointerException("Needed argument is null");
    }
    this.tracer = tracer;
    setContext(workContext);

    // find ordered vars
    final List<String> parametrizedNames = new ArrayList<String>();

    final StringBuilder builder = new StringBuilder(goal);

    int varIndex = 1000;
    while (true) {
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
    try {
      setParsedGoal(treebuilder.readPhraseAndMakeTree(getGoalAsText()));
    }
    catch (InterruptedException ex) {
      throw new IOException("Parsing was interrupted", ex);
    }
  }

  /**
   * Process the goal once. The goal will be called once and the result will be
   * returned. If the goal fails then null will be returned.
   *
   * @return null if the goal fails else a resulting Term
   * @throws InterruptedException it will be thrown if the executing thread is
   * interrupted
   * @see Goal
   */
  public final Term processGoalOnce() throws InterruptedException {
    final Term target = getParsedGoal().makeClone();
    final Goal goal = new Goal(target, getContext(), tracer);
    return goal.solve();
  }

  /**
   * Make version of the goal for ordered integer parameters.
   *
   * @param parameters an integer array of values for inside goal parameters,
   * must not be null
   * @return the prepared goal for the parameters
   * @see Goal
   */
  public final Goal forIntegerParameters(final int... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = new TermInteger(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  /**
   * Make version of the goal for ordered String parameters.
   *
   * @param parameters a String array of values for inside goal parameters, must
   * not be null
   * @return the prepared goal for the parameters
   * @see Goal
   */
  public final Goal forStringParameters(final String... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = new Term(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  /**
   * Make version of the goal for ordered float parameters.
   *
   * @param parameters a float array of values for inside goal parameters, must
   * not be null
   * @return the prepared goal for the parameters
   */
  public final Goal forFloatParameters(final float... parameters) {
    final Term[] termarray = new Term[parameters.length];
    for (int li = 0; li < parameters.length; li++) {
      termarray[li] = new TermFloat(parameters[li]);
    }
    return this.forParameters(termarray);
  }

  /**
   * Make version of the goal for ordered Term parameters.
   *
   * @param parameters a Term array of values for inside goal parameters, must
   * not be null
   * @return the prepared goal for the parameters
   */
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

  /**
   * Make version of goal for named list of values
   *
   * @param vars a Map contains name-value pairs for inside parametrized
   * variables, must not be null
   * @return the prepared goal for the parameters
   */
  public final Goal forParameters(final Map<String, Term> vars) {
    if (vars == null) {
      throw new NullPointerException();
    }

    final Term goalClone = getParsedGoal().makeClone();

    for (Entry<String, Term> entry : vars.entrySet()) {
      final Term value = entry.getValue();
      final Var foundVar = Utils.findVarInsideTerm(goalClone, entry.getKey());
      if (foundVar == null) {
        throw new IllegalArgumentException("Can't find variable for \'" + entry.getKey() + "\' name");
      }
      foundVar.setValue(value);
    }

    return new Goal(goalClone, getContext(), tracer);
  }

  /**
   * Get the goal instance without any parametrization
   *
   * @return the goal instance, must not be null
   */
  public final Goal getNonparametrizedGoalInstance() {
    return new Goal(getParsedGoal(), getContext(), tracer);
  }
}
