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

import com.igormaznitsa.prol.containers.ClauseIterator;
import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.data.Var;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.prol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.prol.libraries.PredicateProcessor;
import com.igormaznitsa.prol.parser.ProlTreeBuilder;
import com.igormaznitsa.prol.trace.TraceListener;
import com.igormaznitsa.prol.utils.Utils;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * The class implements the main prolog logic mechanism to solve goals. You must
 * remember that the goal works and return direct values of terms (for speed) so
 * you have to be accurate in work with the values. To avoid risks you can use a
 * IsolatedGoal wrapper.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see IsolatedGoal
 * @see PreparedGoal
 */
public class Goal {

  /**
   * The inside constant shows that a goal is solved
   */
  private static final int GOALRESULT_SOLVED = 0;
  /**
   * The inside constant shows that a goal is failed
   */
  private static final int GOALRESULT_FAIL = 1;
  /**
   * The inside constant shows that the goal chain was changed
   */
  private static final int GOALRESULT_STACK_CHANGED = 2;
  /**
   * The variable contains the link to the root goal for the goal
   */
  private final Goal rootGoal;
  /**
   * The variable contains a map to get access to goal variables, the variable
   * has a value only for the root goal, for others it has null
   */
  private final Map<String, Var> variables;
  /**
   * The variable contains the variable state snapshot for the goal, it can be
   * null if the goal doesn't change variables
   */
  private final VariableStateSnapshot varSnapshot;
  /**
   * The variable contains the prol context for the goal
   */
  private final ProlContext context;
  /**
   * The variable saves an auxiliary object for the goal
   */
  private Object auxObject;
  /**
   * The flag shows that there ary not variants for the goal anymore
   */
  private boolean noMoreVariantsFlag;
  /**
   * The variable contains the goal term of the goal
   */
  private Term goalTerm;
  /**
   * The link field has the previous goal at the goal chain
   */
  private Goal prevGoalAtChain;
  /**
   * The variable is used only by a root goal. It has the last goal at the goal
   * chain
   */
  private Goal rootLastGoalAtChain;
  /**
   * The variable contains current subgoal of the goal
   */
  private Goal subGoal;
  /**
   * The variable contains the connector term which will be used to send the
   * data from a subgoal to the goal
   */
  private Term subGoalConnector;
  /**
   * The variable contains the connector term of the goal to get data from the
   * subgoal
   */
  private Term thisConnector;
  /**
   * The variable has the next term which should be added to the chain
   */
  private Term nextAndTerm;
  /**
   * The variable has the nextAndTerm field value for added goal
   */
  private Term nextAndTermForNextGoal;
  /**
   * The variable contains current clause iterator for the goal
   */
  private ClauseIterator clauseIterator;
  /**
   * This flag allows to cancel a clause iterator of the root goal from a child
   * goal
   */
  private boolean cutMeet;
  /**
   * This variable contains a listener for goal events, it can be null
   */
  private final TraceListener tracer;
  /**
   * The flag is used by tracer mechanism to see when the solve is being called
   * the first time
   */
  private boolean notFirstProve;

  @Override
  public String toString() {
    return (isCompleted() ? "Completed " : "Active ") + "Goal(" + goalTerm.toString() + ')';
  }

  /**
   * Get the current goal chain as a List
   *
   * @return a List object contains all goals at the goal chain of the root goal
   */
  public List<Goal> getChainAsList() {
    final List<Goal> result = new LinkedList<Goal>();

    Goal curgoal = rootGoal.rootLastGoalAtChain;
    while (curgoal != null) {
      result.add(0, curgoal);
      curgoal = curgoal.prevGoalAtChain;
    }

    return result;
  }

  /**
   * Get the value of a goal variable as a Number object.
   *
   * @param varName the name of the variable, must not be null;
   * @return null if the variable is not-instantiated one or its value as a
   * Number object
   * @throws IllegalArgumentException if the variable name is wrong
   * @throws NumberFormatException if the value is not-numeric one
   */
  public Number getVarAsNumber(final String varName) {
    final Var var = getVarForName(varName);
    if (var == null) {
      throw new IllegalArgumentException("Unknown variable for name \'" + varName + '\'');
    }
    final Term value = var.getValue();
    if (value == null) {
      return null;
    }
    else {
      if (value instanceof NumericTerm) {
        return ((NumericTerm) value).getNumericValue();
      }
      else {
        throw new NumberFormatException("The variable contains a non-numeric value");
      }
    }
  }

  /**
   * Get the value of a goal variable as a String object.
   *
   * @param varName the name of the variable, must not be null;
   * @return null if the variable is not-instantiated one or its text
   * representation (like it will be written by the write/1)
   * @throws IllegalArgumentException if the variable name is wrong
   */
  public String getVarAsText(final String varName) {
    final Var var = getVarForName(varName);
    if (var == null) {
      throw new IllegalArgumentException("Unknown variable for name \'" + varName + '\'');
    }
    final Term value = var.getValue();
    if (value == null) {
      return null;
    }
    else {
      return value.toString();
    }
  }

  /**
   * Get a variable goal object for its name
   *
   * @param name the name of the needed variable, must not be null;
   * @return the found variable as a Var object or null if the variable is not
   * found
   */
  public Var getVarForName(final String name) {
    if (name == null) {
      throw new NullPointerException("Variable name is null");
    }
    return variables == null ? null : variables.get(name);
  }

  /**
   * To replace the last chain goal by the new goal for the term
   *
   * @param goal the new goal term which will be processed by the new last chain
   * goal
   * @return the generated goal object
   */
  public Goal replaceLastGoalAtChain(final Term goal) {
    if (tracer != null) {
      tracer.onProlGoalExit(this.rootGoal.rootLastGoalAtChain);
    }

    final Goal newGoal = new Goal(this.rootGoal, goal, context, rootGoal.tracer);
    final Goal prevGoal = newGoal.prevGoalAtChain;
    if (prevGoal != null) {
      newGoal.prevGoalAtChain = prevGoal.prevGoalAtChain;
      newGoal.nextAndTerm = prevGoal.nextAndTerm;
      newGoal.nextAndTermForNextGoal = prevGoal.nextAndTermForNextGoal;
    }
    return newGoal;
  }

  /**
   * Inside special constructor
   *
   * @param rootGoal the root goal for the new goal, if it is null then the new
   * goal will be the root goal for itself
   * @param goal the term which will be goal for the new Goal, must not be null
   * @param context the context for the new goal
   * @param tracer the listener to listen events for the goal and its children
   * (it will replace the default listener of the context if it is presented)
   */
  private Goal(final Goal rootGoal, Term goal, final ProlContext context, final TraceListener tracer) {
    this.rootGoal = rootGoal == null ? this : rootGoal;
    this.goalTerm = goal.getTermType() == Term.TYPE_ATOM ? new TermStruct(goal) : goal;
    this.context = context;
    this.tracer = tracer == null ? context.getDefaultTraceListener() : tracer;

    if (goal.getTermType() == Term.TYPE_VAR) {
      goal = Utils.getTermFromElement(goal);
      if (goal.getTermType() == Term.TYPE_VAR) {
        throw new ProlInstantiationErrorException("callable", goal);
      }
    }

    switch (goal.getTermType()) {
      case Term.TYPE_ATOM: {
        if (goal instanceof NumericTerm) {
          throw new ProlTypeErrorException("callable", goal);
        }
      }
      break;
      case Term.TYPE_VAR: {
        if (((Var) goal).isUndefined()) {
          throw new ProlInstantiationErrorException(goal);
        }
      }
      break;
      case Term.TYPE_LIST: {
        throw new ProlTypeErrorException("callable", goal);
      }
    }

    if (rootGoal == null) {
      if (goal.getTermType() == Term.TYPE_ATOM) {
        varSnapshot = null;
        variables = null;
      }
      else {
        variables = Utils.fillTableWithVars(goal);
        varSnapshot = new VariableStateSnapshot(goal);
      }
      rootLastGoalAtChain = this;
      prevGoalAtChain = null;
    }
    else {
      variables = null;
      if (goal.getTermType() == Term.TYPE_ATOM) {
        varSnapshot = null;
      }
      else {
        varSnapshot = new VariableStateSnapshot(rootGoal.varSnapshot);// new VariableStateSnapshot(rootGoal.goalTerm);
      }
      this.prevGoalAtChain = rootGoal.rootLastGoalAtChain;
      rootGoal.rootLastGoalAtChain = this;
    }
  }

  /**
   * Get the saved auxiliary object. It can be any successor of Object or null
   *
   * @return the saved auxiliary object or null
   */
  public Object getAuxObject() {
    return auxObject;
  }

  /**
   * To get the tracer for the goal
   *
   * @return the tracer for the goal or null if the tracer is undefined
   */
  public TraceListener getTracer() {
    return tracer;
  }

  /**
   * Save an auxiliary object for the goal. It can be any successor of Object or
   * null
   *
   * @param obj the object to be saved as an auxiliary one or null
   */
  public void setAuxObject(final Object obj) {
    auxObject = obj;
  }

  /**
   * Get the goal term
   *
   * @return the goal term
   */
  public Term getGoalTerm() {
    return goalTerm;
  }

  /**
   * Get the context for the goal
   *
   * @return the prol context for the goal
   */
  public ProlContext getContext() {
    return context;
  }

  /**
   * Inside special constructor for special functions
   */
  protected Goal() {
    rootGoal = null;
    variables = null;
    varSnapshot = null;
    context = null;
    tracer = null;
  }

  /**
   * A Constructor
   *
   * @param goal the goal as a String, must not be null
   * @param context the context for the goal, must not be null
   * @param tracer the tracer to listen goal events, can be null
   * @throws IOException it will be thrown if the goal string can't be parsed
   * well
   * @throws InterruptedException it will be thrown if the process is
   * interrupted
   */
  public Goal(final String goal, final ProlContext context, final TraceListener tracer) throws IOException, InterruptedException {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(goal), context, tracer);
  }

  /**
   * A Constructor
   *
   * @param goal the goal as a String, must not be null
   * @param context the context for the goal, must not be null
   * @throws IOException it will be thrown if the goal string can't be parsed
   * well
   * @throws InterruptedException it will be thrown if the process is
   * interrupted
   */
  public Goal(final String goal, final ProlContext context) throws IOException, InterruptedException {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(goal), context, null);
  }

  /**
   * A Constructor
   *
   * @param goal the goal term, must not be null
   * @param context the context for the goal, must not be null
   * @param tracer the tracer to listen goal events, it can be null
   */
  public Goal(final Term goal, final ProlContext context, final TraceListener tracer) {
    this(null, goal, context, tracer);
  }

  /**
   * A Constructor
   *
   * @param goal the goal term, must not be null
   * @param context the context for the goal, must not be null
   */
  public Goal(final Term goal, final ProlContext context) {
    this(null, goal, context, null);
  }

  /**
   * Get next solution of the goal
   *
   * @return the solution as a term or null if there is not a solution anymore
   * @throws InterruptedException it will be thrown if the process is
   * interrupted
   * @throws ProlHaltExecutionException it will be thrown if the context is
   * halted
   */
  public Term solve() throws InterruptedException {
    Term result = null;

    boolean loop = true;
    final ProlContext localcontext = this.context;

    while (loop) {
      // check that the context is halted and throw an execption if it is
      if (localcontext.isHalted()) {
        throw new ProlHaltExecutionException();
      }

      Goal goalToProcess = rootGoal.rootLastGoalAtChain;
      if (goalToProcess == null) {
        break;
      }
      else {
        if (!goalToProcess.noMoreVariantsFlag) {
          switch (goalToProcess._solve()) {
            case GOALRESULT_FAIL: {
              if (tracer != null) {
                tracer.onProlGoalFail(goalToProcess);
                tracer.onProlGoalExit(goalToProcess);
              }

              rootGoal.rootLastGoalAtChain = goalToProcess.prevGoalAtChain;
            }
            break;
            case GOALRESULT_SOLVED: {
              // we have to renew data about last chain goal because it can be changed during the operation
              goalToProcess = rootGoal.rootLastGoalAtChain;

              if (goalToProcess.nextAndTerm != null) {
                final Goal nextGoal = new Goal(rootGoal, goalToProcess.nextAndTerm, localcontext, rootGoal.tracer);
                nextGoal.nextAndTerm = goalToProcess.nextAndTermForNextGoal;
              }
              else {
                result = rootGoal.goalTerm;
                loop = false;
              }
            }
            break;
            case GOALRESULT_STACK_CHANGED: {
            }
            break;
          }
        }
        else {
          if (tracer != null) {
            tracer.onProlGoalExit(goalToProcess);
          }
          rootGoal.rootLastGoalAtChain = goalToProcess.prevGoalAtChain;
        }
      }

    }

    return result;
  }

  /**
   * Inside solve function allows to work with the goal chain
   *
   * @return GOALRESULT_FAIL if the goal failed, GOALRESULT_SOLVED if the goal
   * solved and GOALRESULT_STACK_CHANGED if the goal stack is changed and need
   * to be resolved
   * @throws InterruptedException it will be thrown if the process is
   * interrupted
   */
  private int _solve() throws InterruptedException {
    if (Thread.currentThread().isInterrupted()) {
      throw new InterruptedException();
    }

    if (tracer != null) {
      if (notFirstProve) {
        if (!tracer.onProlGoalRedo(this)) {
          return GOALRESULT_FAIL;
        }
      }
      else {
        notFirstProve = true;
        if (!tracer.onProlGoalCall(this)) {
          return GOALRESULT_FAIL;
        }
      }
    }

    int result = GOALRESULT_FAIL;

    boolean loop = true;

    while (loop) {
      // reset the variables to their initing state
      if (varSnapshot != null) {
        varSnapshot.resetToState();
      }

      if (subGoal != null) {
        // solve subgoal
        final Term solvedTerm = subGoal.solve();

        if (subGoal.cutMeet) {
          clauseIterator = null;
        }

        if (solvedTerm == null) {
          subGoal = null;
          if (clauseIterator == null) {
            result = GOALRESULT_FAIL;
            break;
          }
        }
        else {
          if (!thisConnector.Equ(subGoalConnector)) {
            throw new ProlCriticalError("Critical error #980234");
          }
          result = GOALRESULT_SOLVED;
          break;
        }
      }

      if (clauseIterator != null) {
        // next clause
        if (clauseIterator.hasNext()) {

          final TermStruct structFromBase = clauseIterator.next();

          Term goalTermForEqu = null;

          if (((TermStruct) goalTerm).isFunctorLikeRuleDefinition()) {
            goalTermForEqu = ((TermStruct) goalTerm).getElement(0).makeClone();
          }
          else {
            goalTermForEqu = goalTerm.makeClone();
          }

          if (!goalTermForEqu.Equ(structFromBase.isFunctorLikeRuleDefinition() ? structFromBase.getElement(0) : structFromBase)) {
            throw new ProlCriticalError("impossible situation #2123123");
          }

          if (structFromBase.isFunctorLikeRuleDefinition()) {
            thisConnector = goalTerm;
            subGoalConnector = structFromBase.getElement(0);
            subGoal = new Goal(structFromBase.getElement(1), context, tracer);
            continue;
          }
          else {
            if (!goalTerm.Equ(structFromBase)) {
              throw new ProlCriticalError("Impossible situation #0009824");
            }
            result = GOALRESULT_SOLVED;
            break;
          }
        }
        else {
          clauseIterator = null;
          noMoreVariants();
          break;
        }
      }

      switch (goalTerm.getTermType()) {
        case Term.TYPE_ATOM: {
          final String text = goalTerm.getText();
          result = context.hasPredicateAtLibraryForSignature(text + "/0") ? GOALRESULT_SOLVED : GOALRESULT_FAIL;
          noMoreVariants();
          loop = false;
        }
        break;
        case Term.TYPE_STRUCT: {
          final TermStruct struct = (TermStruct) goalTerm;
          final int arity = struct.getArity();

          if (struct.isFunctorLikeRuleDefinition()) {
            final TermStruct structClone = (TermStruct) struct.makeClone();

            thisConnector = struct.getElement(0);
            subGoalConnector = structClone.getElement(0);

            if (arity == 1) {
              subGoal = new Goal(structClone.getElement(0), context, tracer);
            }
            else {
              subGoal = new Goal(structClone.getElement(1), context, tracer);
            }
          }
          else {

            final Term functor = struct.getFunctor();
            final String functorText = functor.getText();

            boolean process = true;

            switch (arity) {
              case 0: {
                final int len = functorText.length();
                if (len == 1 && functorText.charAt(0) == '!') {
                  // cut
                  cut();
                  process = false;
                  loop = false;
                  result = GOALRESULT_SOLVED;
                  noMoreVariantsFlag = true;
                }
                else if (len == 2 && "!!".equals(functorText)) {
                  // cut local
                  cutLocal();
                  process = false;
                  loop = false;
                  noMoreVariantsFlag = true;
                  result = GOALRESULT_SOLVED;
                }
              }
              break;
              case 2: {
                final int textLen = functorText.length();
                if (textLen == 1) {
                  switch (functorText.charAt(0)) {
                    case ',': {
                      // and
                      final Goal leftSubgoal = replaceLastGoalAtChain(struct.getElement(0));
                      leftSubgoal.nextAndTerm = struct.getElement(1);
                      leftSubgoal.nextAndTermForNextGoal = this.nextAndTerm;

                      result = GOALRESULT_STACK_CHANGED;

                      loop = false;
                      process = false;
                    }
                    break;
                    case ';': {
                      // or
                      if (getAuxObject() == null) {
                        // left subbranch
                        final Goal leftSubbranch = new Goal(rootGoal, struct.getElement(0), context, tracer);
                        leftSubbranch.nextAndTerm = this.nextAndTerm;
                        setAuxObject(leftSubbranch);
                      }
                      else {
                        // right subbranch
                        replaceLastGoalAtChain(struct.getElement(1));
                      }
                      result = GOALRESULT_STACK_CHANGED;
                      process = false;
                      loop = false;
                    }
                    break;
                  }
                }
              }
              break;
            }

            if (process) {
              final PredicateProcessor processor = checkProcessor(struct);
              if (processor == PredicateProcessor.NULL_PROCESSOR) {
                // just a struct
                // find it at knowledge base
                clauseIterator = context.getKnowledgeBase().getClauseIterator(struct);
                if (clauseIterator == null || !clauseIterator.hasNext()) {
                  loop = false;
                  noMoreVariants();
                  result = GOALRESULT_FAIL;
                }
              }
              else {
                // it is a processor
                if (processor.isEvaluable() || processor.isDetermined()) {
                  noMoreVariants();
                }

                if (processor.execute(this, struct)) {
                  result = GOALRESULT_SOLVED;
                }
                else {
                  result = GOALRESULT_FAIL;
                }

                if (result == GOALRESULT_SOLVED && processor.doesChangeGoalChain()) {
                  result = GOALRESULT_STACK_CHANGED;
                }

                loop = false;
              }
            }
          }
        }
        break;
        default: {
          result = GOALRESULT_FAIL;
          loop = false;
          noMoreVariants();
        }
        break;
      }
    }

    return result;
  }

  /**
   * Set the flag of the goal shows that the goal hasn't more variants
   */
  public void noMoreVariants() {
    noMoreVariantsFlag = true;
  }

  /**
   * Cut the goal chain with reaction of the root goal
   */
  public void cut() {
    rootGoal.cutMeet = true;
    rootGoal.clauseIterator = null;
    prevGoalAtChain = null;
  }

  /**
   * Cut the goal chain without reaction of the root chain
   */
  public void cutLocal() {
    prevGoalAtChain = null;
  }

  /**
   * Inside function to check processor for a term structure and to set the
   * processor if it is not presented
   *
   * @param structure the structure to be checked, must not be null
   * @return the predicate processor for the structure if it is found or null
   */
  private PredicateProcessor checkProcessor(final TermStruct structure) {
    PredicateProcessor processor = structure.getPredicateProcessor();
    if (processor == null) {
      processor = context.findProcessor(structure);
      if (processor == null) {
        processor = PredicateProcessor.NULL_PROCESSOR;
      }
      structure.setPredicateProcessor(processor);
    }
    return processor;
  }

  /**
   * Check that the goal is completed and doesn't have anymore variants
   *
   * @return true if the goal is completed else false
   */
  public boolean isCompleted() {
    return rootGoal.rootLastGoalAtChain == null || noMoreVariantsFlag;
  }

  /**
   * Get the variable snapshot of the goal
   *
   * @return the variable snapshot for the goal, it can be null if the goal
   * doesn't have any snapshot
   */
  public VariableStateSnapshot getVariableSnapshot() {
    return varSnapshot;
  }
}
