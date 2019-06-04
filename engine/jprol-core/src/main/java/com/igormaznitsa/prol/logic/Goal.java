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
import java.util.ArrayList;
import java.util.HashMap;
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
   * This variable contains a listener for goal events, it can be null
   */
  private final TraceListener tracer;
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
   * The flag is used by tracer mechanism to see when the solve is being called
   * the first time
   */
  private boolean notFirstProve;

  /**
   * Special internal constructor
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
      } else {
        variables = Utils.fillTableWithVars(goal);
        varSnapshot = new VariableStateSnapshot(goal);
      }
      rootLastGoalAtChain = this;
      prevGoalAtChain = null;
    } else {
      variables = null;
      if (goal.getTermType() == Term.TYPE_ATOM) {
        varSnapshot = null;
      } else {
        varSnapshot = new VariableStateSnapshot(rootGoal.varSnapshot);// new VariableStateSnapshot(rootGoal.goalTerm);
      }
      this.prevGoalAtChain = rootGoal.rootLastGoalAtChain;
      rootGoal.rootLastGoalAtChain = this;
    }
  }

  /**
   * Make map from values of all instantiated variables.
   * @return instantiated values as map.
   */
  public Map<String, Term> findAllInstantiatedVars(){
    final Map<String,Term> result = new HashMap<>();
    this.variables.entrySet().stream().filter((v) -> (!v.getValue().isUndefined())).forEach((v) -> {
      result.put(v.getKey(), v.getValue().getValue().makeClone());
    });
    return result;
  }
  
  /**
   * Inside special constructor for special functions
   */
  protected Goal() {
      this.rootGoal = null;
      this.variables = null;
      this.varSnapshot = null;
      this.context = null;
      this.tracer = null;
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
    final List<Goal> result = new ArrayList<>();

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
    } else {
      if (value instanceof NumericTerm) {
        return ((NumericTerm) value).getNumericValue();
      } else {
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
    } else {
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
    return this.variables == null ? null : this.variables.get(name);
  }

  /**
   * To replace the last chain goal by the new goal for the term
   *
   * @param goal the new goal term which will be processed by the new last chain
   * goal
   * @return the generated goal object
   */
  public Goal replaceLastGoalAtChain(final Term goal) {
    if (this.tracer != null) {
      this.tracer.onProlGoalExit(this.rootGoal.rootLastGoalAtChain);
    }

    final Goal newGoal = new Goal(this.rootGoal, goal, this.context, this.rootGoal.tracer);
    final Goal prevGoal = newGoal.prevGoalAtChain;
    if (prevGoal != null) {
      newGoal.prevGoalAtChain = prevGoal.prevGoalAtChain;
      newGoal.nextAndTerm = prevGoal.nextAndTerm;
      newGoal.nextAndTermForNextGoal = prevGoal.nextAndTermForNextGoal;
    }
    return newGoal;
  }

  /**
   * Get the saved auxiliary object. It can be any successor of Object or null
   *
   * @return the saved auxiliary object or null
   */
  public Object getAuxObject() {
    return this.auxObject;
  }

  /**
   * Save an auxiliary object for the goal. It can be any successor of Object or
   * null
   *
   * @param obj the object to be saved as an auxiliary one or null
   */
  public void setAuxObject(final Object obj) {
      this.auxObject = obj;
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
   * Get the goal term
   *
   * @return the goal term
   */
  public Term getGoalTerm() {
    return this.goalTerm;
  }

  /**
   * Get the context for the goal
   *
   * @return the prol context for the goal
   */
  public ProlContext getContext() {
    return this.context;
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

    final boolean tracingOn = this.tracer != null;

    while (loop && !Thread.currentThread().isInterrupted()) {
      if (localcontext.isHalted()) {
        throw new ProlHaltExecutionException();
      }

      Goal goalToProcess = this.rootGoal.rootLastGoalAtChain;
      if (goalToProcess == null) {
        break;
      } else {
        if (!goalToProcess.noMoreVariantsFlag) {
          switch (goalToProcess.resolve()) {
            case GOALRESULT_FAIL: {
              if (tracingOn) {
                this.tracer.onProlGoalFail(goalToProcess);
                this.tracer.onProlGoalExit(goalToProcess);
              }
              this.rootGoal.rootLastGoalAtChain = goalToProcess.prevGoalAtChain;
            }
            break;
            case GOALRESULT_SOLVED: {
              // we have to renew data about last chain goal because it can be changed during the operation
              goalToProcess = this.rootGoal.rootLastGoalAtChain;

              if (goalToProcess.nextAndTerm != null) {
                final Goal nextGoal = new Goal(this.rootGoal, goalToProcess.nextAndTerm, localcontext, this.rootGoal.tracer);
                nextGoal.nextAndTerm = goalToProcess.nextAndTermForNextGoal;
              } else {
                result = this.rootGoal.goalTerm;
                loop = false;
              }
            }
            break;
            case GOALRESULT_STACK_CHANGED: {
            }
            break;
            default:
              throw new Error("Unexpected result");
          }
        } else {
          if (tracingOn) {
            this.tracer.onProlGoalExit(goalToProcess);
          }
          this.rootGoal.rootLastGoalAtChain = goalToProcess.prevGoalAtChain;
        }
      }
    }

    return result;
  }

  /**
   * Internal resolving function allows to work with the goal chain
   *
   * @return GOALRESULT_FAIL if the goal failed, GOALRESULT_SOLVED if the goal
   * solved and GOALRESULT_STACK_CHANGED if the goal stack is changed and need
   * to be resolved
   * @throws InterruptedException it will be thrown if the process is
   * interrupted
   */
  private int resolve() throws InterruptedException {
    if (Thread.currentThread().isInterrupted()) {
      throw new InterruptedException();
    }

    if (this.tracer != null) {
      if (this.notFirstProve) {
        if (!this.tracer.onProlGoalRedo(this)) {
          return GOALRESULT_FAIL;
        }
      } else {
        this.notFirstProve = true;
        if (!this.tracer.onProlGoalCall(this)) {
          return GOALRESULT_FAIL;
        }
      }
    }

    int result = GOALRESULT_FAIL;

    boolean doLoop = true;

    while (doLoop) {
      // reset variables to their initial state
      if (this.varSnapshot != null) {
        this.varSnapshot.resetToState();
      }

      if (this.subGoal != null) {
        // solve subgoal
        final Term solvedTerm = this.subGoal.solve();

        if (this.subGoal.cutMeet) {
          this.clauseIterator = null;
        }

        if (solvedTerm == null) {
          this.subGoal = null;
          if (this.clauseIterator == null) {
            result = GOALRESULT_FAIL;
            break;
          }
        } else {
          if (!this.thisConnector.Equ(this.subGoalConnector)) {
            throw new ProlCriticalError("Critical error #980234");
          }
          result = GOALRESULT_SOLVED;
          break;
        }
      }

      if (this.clauseIterator != null) {
        // next clause
        if (this.clauseIterator.hasNext()) {
          final TermStruct structFromBase = this.clauseIterator.next();

          final Term goalTermForEqu;
          if (((TermStruct) this.goalTerm).isFunctorLikeRuleDefinition()) {
            goalTermForEqu = ((TermStruct) this.goalTerm).getElement(0).makeClone();
          } else {
            goalTermForEqu = this.goalTerm.makeClone();
          }

          if (!goalTermForEqu.Equ(structFromBase.isFunctorLikeRuleDefinition() ? structFromBase.getElement(0) : structFromBase)) {
            throw new ProlCriticalError("impossible situation #2123123");
          }

          if (structFromBase.isFunctorLikeRuleDefinition()) {
            this.thisConnector = goalTerm;
            this.subGoalConnector = structFromBase.getElement(0);
            this.subGoal = new Goal(structFromBase.getElement(1), this.context, this.tracer);
            continue;
          } else {
            if (!this.goalTerm.Equ(structFromBase)) {
              throw new ProlCriticalError("Impossible situation #0009824");
            }
            result = GOALRESULT_SOLVED;
            break;
          }
        } else {
          this.clauseIterator = null;
          noMoreVariants();
          break;
        }
      }

      switch (this.goalTerm.getTermType()) {
        case Term.TYPE_ATOM: {
          final String text = this.goalTerm.getText();
          result = this.context.hasZeroArityPredicateForName(text) ? GOALRESULT_SOLVED : GOALRESULT_FAIL;
          noMoreVariants();
          doLoop = false;
        }
        break;
        case Term.TYPE_STRUCT: {
          final TermStruct struct = (TermStruct) goalTerm;
          final int arity = struct.getArity();

          if (struct.isFunctorLikeRuleDefinition()) {
            final TermStruct structClone = (TermStruct) struct.makeClone();

            this.thisConnector = struct.getElement(0);
            this.subGoalConnector = structClone.getElement(0);

            if (arity == 1) {
              this.subGoal = new Goal(structClone.getElement(0), this.context, this.tracer);
            } else {
              this.subGoal = new Goal(structClone.getElement(1), this.context, this.tracer);
            }
          } else {

            final Term functor = struct.getFunctor();
            final String functorText = functor.getText();

            boolean nonConsumed = true;

            switch (arity) {
              case 0: {
                final int len = functorText.length();
                if (len == 1 && functorText.charAt(0) == '!') {
                  // cut
                  cut();
                  nonConsumed = false;
                  doLoop = false;
                  result = GOALRESULT_SOLVED;
                  this.noMoreVariantsFlag = true;
                } else if (len == 2 && "!!".equals(functorText)) {
                  // cut local
                  cutLocal();
                  nonConsumed = false;
                  doLoop = false;
                  this.noMoreVariantsFlag = true;
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

                      doLoop = false;
                      nonConsumed = false;
                    }
                    break;
                    case ';': {
                      // or
                      if (getAuxObject() == null) {
                        // left subbranch
                        final Goal leftSubbranch = new Goal(this.rootGoal, struct.getElement(0), this.context, this.tracer);
                        leftSubbranch.nextAndTerm = this.nextAndTerm;
                        setAuxObject(leftSubbranch);
                      } else {
                        // right subbranch
                        replaceLastGoalAtChain(struct.getElement(1));
                      }
                      result = GOALRESULT_STACK_CHANGED;
                      nonConsumed = false;
                      doLoop = false;
                    }
                    break;
                  }
                }
              }
              break;
            }

            if (nonConsumed) {
              final PredicateProcessor processor = ensureStructProcessor(struct);
              if (processor == PredicateProcessor.NULL_PROCESSOR) {
                // just a struct
                // find it at knowledge base
                this.clauseIterator = this.context.getKnowledgeBase().getClauseIterator(struct);
                if (this.clauseIterator == null || !this.clauseIterator.hasNext()) {
                  doLoop = false;
                  noMoreVariants();
                  result = GOALRESULT_FAIL;
                }
              } else {
                // it is a processor
                if (processor.isEvaluable() || processor.isDetermined()) {
                  noMoreVariants();
                }

                if (processor.execute(this, struct)) {
                  result = GOALRESULT_SOLVED;
                } else {
                  result = GOALRESULT_FAIL;
                }

                if (result == GOALRESULT_SOLVED && processor.doesChangeGoalChain()) {
                  result = GOALRESULT_STACK_CHANGED;
                }

                doLoop = false;
              }
            }
          }
        }
        break;
        default: {
          result = GOALRESULT_FAIL;
          doLoop = false;
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
    this.noMoreVariantsFlag = true;
  }

  /**
   * Cut the goal chain with reaction of the root goal
   */
  public void cut() {
      this.rootGoal.cutMeet = true;
      this.rootGoal.clauseIterator = null;
      this.prevGoalAtChain = null;
  }

  /**
   * Cut the goal chain without reaction of the root chain
   */
  public void cutLocal() {
      this.prevGoalAtChain = null;
  }

  private PredicateProcessor ensureStructProcessor(final TermStruct structure) {
    PredicateProcessor processor = structure.getPredicateProcessor();
    if (processor == null) {
      processor = this.context.findProcessor(structure);
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
    return this.rootGoal.rootLastGoalAtChain == null || noMoreVariantsFlag;
  }
}