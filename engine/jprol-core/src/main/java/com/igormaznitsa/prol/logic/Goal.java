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
import com.igormaznitsa.prol.parser.ProlReader;
import com.igormaznitsa.prol.parser.ProlTreeBuilder;
import com.igormaznitsa.prol.trace.TraceEvent;
import com.igormaznitsa.prol.trace.TraceListener;
import com.igormaznitsa.prol.utils.Utils;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.igormaznitsa.prol.data.TermType.ATOM;
import static com.igormaznitsa.prol.data.TermType.VAR;
import static com.igormaznitsa.prol.trace.TraceEvent.EXIT;

public class Goal {

  private static final int GOALRESULT_SOLVED = 0;
  private static final int GOALRESULT_FAIL = 1;
  private static final int GOALRESULT_STACK_CHANGED = 2;

  private final Goal rootGoal;
  private final Map<String, Var> variables;
  private final VariableStateSnapshot varSnapshot;
  private final ProlContext context;
  private final TraceListener tracer;
  private Object auxObject;
  private boolean noMoreVariantsFlag;
  private Term goalTerm;
  private Goal prevGoalAtChain;
  private Goal rootLastGoalAtChain;
  private Goal subGoal;
  private Term subGoalConnector;
  private Term thisConnector;
  private Term nextAndTerm;
  private Term nextAndTermForNextGoal;
  private ClauseIterator clauseIterator;
  private boolean cutMeet;
  private boolean notFirstProve;

  private Goal(final Goal rootGoal, Term goal, final ProlContext context, final Map<String, Term> predefinedVarValues, final TraceListener tracer) {
    this.rootGoal = rootGoal == null ? this : rootGoal;
    this.goalTerm = goal.getTermType() == ATOM ? new TermStruct(goal) : goal;
    this.context = context;
    this.tracer = tracer == null ? context.getDefaultTraceListener() : tracer;

    if (goal.getTermType() == VAR) {
      goal = Utils.getTermFromElement(goal);
      if (goal.getTermType() == VAR) {
        throw new ProlInstantiationErrorException("callable", goal);
      }
    }

    switch (goal.getTermType()) {
      case ATOM: {
        if (goal instanceof NumericTerm) {
          throw new ProlTypeErrorException("callable", goal);
        }
      }
      break;
      case VAR: {
        if (!goal.isBounded()) {
          throw new ProlInstantiationErrorException(goal);
        }
      }
      break;
      case LIST: {
        throw new ProlTypeErrorException("callable", goal);
      }
    }

    if (rootGoal == null) {
      if (goal.getTermType() == ATOM) {
        varSnapshot = null;
        variables = null;
      } else {
        variables = Utils.fillTableWithVars(goal);
        varSnapshot = new VariableStateSnapshot(goal, predefinedVarValues);
      }
      rootLastGoalAtChain = this;
      prevGoalAtChain = null;
    } else {
      variables = null;
      if (goal.getTermType() == ATOM) {
        varSnapshot = null;
      } else {
        varSnapshot = new VariableStateSnapshot(rootGoal.varSnapshot);
      }
      this.prevGoalAtChain = rootGoal.rootLastGoalAtChain;
      rootGoal.rootLastGoalAtChain = this;
    }
  }

  protected Goal() {
    this.rootGoal = null;
    this.variables = null;
    this.varSnapshot = null;
    this.context = null;
    this.tracer = null;
  }

  public Goal(final String goal, final ProlContext context, final TraceListener tracer) throws IOException {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(goal), context, tracer);
  }

  public Goal(final String goal, final ProlContext context) throws IOException, InterruptedException {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(goal), context, null);
  }

  public Goal(final ProlReader reader, final ProlContext context) throws IOException, InterruptedException {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(reader), context, null);
  }

  public Goal(final ProlReader reader, final ProlContext context, final Map<String, Term> predefVarValues) throws IOException {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(reader), context, predefVarValues, null);
  }

  public Goal(final Term goal, final ProlContext context, final TraceListener tracer) {
    this(null, goal, context, null, tracer);
  }

  public Goal(final Term goal, final ProlContext context, final Map<String, Term> predefinedVarValues, final TraceListener tracer) {
    this(null, goal, context, predefinedVarValues, tracer);
  }

  public Goal(final Term goal, final ProlContext context) {
    this(null, goal, context, null, null);
  }

  public Map<String, Term> findAllInstantiatedVars() {
    final Map<String, Term> result = new HashMap<>();
    this.variables.entrySet().stream().filter((v) -> (v.getValue().isBounded())).forEach((v) -> result.put(v.getKey(), v.getValue().getValue().makeClone()));
    return result;
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
   * @throws NumberFormatException    if the value is not-numeric one
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

  public Var getVarForName(final String name) {
    if (name == null) {
      throw new NullPointerException("Variable name is null");
    }
    return this.variables == null ? null : this.variables.get(name);
  }

  public Goal replaceLastGoalAtChain(final Term goal) {
    if (this.tracer != null) {
      this.tracer.onTraceEvet(EXIT, this.rootGoal.rootLastGoalAtChain);
    }

    final Goal newGoal = new Goal(this.rootGoal, goal, this.context, null, this.rootGoal.tracer);
    final Goal prevGoal = newGoal.prevGoalAtChain;
    if (prevGoal != null) {
      newGoal.prevGoalAtChain = prevGoal.prevGoalAtChain;
      newGoal.nextAndTerm = prevGoal.nextAndTerm;
      newGoal.nextAndTermForNextGoal = prevGoal.nextAndTermForNextGoal;
    }
    return newGoal;
  }

  @SuppressWarnings("unchecked")
  public <T> T getAuxObject() {
    return (T) this.auxObject;
  }

  public void setAuxObject(final Object obj) {
    this.auxObject = obj;
  }

  public TraceListener getTracer() {
    return tracer;
  }

  public Term getGoalTerm() {
    return this.goalTerm;
  }

  public ProlContext getContext() {
    return this.context;
  }

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
                this.tracer.onTraceEvet(TraceEvent.FAIL, goalToProcess);
                this.tracer.onTraceEvet(EXIT, goalToProcess);
              }
              this.rootGoal.rootLastGoalAtChain = goalToProcess.prevGoalAtChain;
            }
            break;
            case GOALRESULT_SOLVED: {
              // we have to renew data about last chain goal because it can be changed during the operation
              goalToProcess = this.rootGoal.rootLastGoalAtChain;

              if (goalToProcess.nextAndTerm != null) {
                final Goal nextGoal = new Goal(this.rootGoal, goalToProcess.nextAndTerm, localcontext, null, this.rootGoal.tracer);
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
            this.tracer.onTraceEvet(EXIT, goalToProcess);
          }
          this.rootGoal.rootLastGoalAtChain = goalToProcess.prevGoalAtChain;
        }
      }
    }

    return result;
  }

  private int resolve() throws InterruptedException {
    if (Thread.currentThread().isInterrupted()) {
      throw new InterruptedException();
    }

    if (this.tracer != null) {
      if (this.notFirstProve) {
        this.tracer.onTraceEvet(TraceEvent.REDO, this);
      } else {
        this.notFirstProve = true;
        this.tracer.onTraceEvet(TraceEvent.CALL, this);
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
          if (!this.thisConnector.unifyTo(this.subGoalConnector)) {
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

          if (!goalTermForEqu.unifyTo(structFromBase.isFunctorLikeRuleDefinition() ? structFromBase.getElement(0) : structFromBase)) {
            throw new ProlCriticalError("impossible situation #2123123");
          }

          if (structFromBase.isFunctorLikeRuleDefinition()) {
            this.thisConnector = goalTerm;
            this.subGoalConnector = structFromBase.getElement(0);
            this.subGoal = new Goal(structFromBase.getElement(1), this.context, this.tracer);
            continue;
          } else {
            if (!this.goalTerm.unifyTo(structFromBase)) {
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
        case ATOM: {
          final String text = this.goalTerm.getText();
          result = this.context.hasZeroArityPredicateForName(text) ? GOALRESULT_SOLVED : GOALRESULT_FAIL;
          noMoreVariants();
          doLoop = false;
        }
        break;
        case STRUCT: {
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
                        final Goal leftSubbranch = new Goal(this.rootGoal, struct.getElement(0), this.context, null, this.tracer);
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

  public void noMoreVariants() {
    this.noMoreVariantsFlag = true;
  }

  public void cut() {
    this.rootGoal.cutMeet = true;
    this.rootGoal.clauseIterator = null;
    this.prevGoalAtChain = null;
  }

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

  public boolean isCompleted() {
    return this.rootGoal.rootLastGoalAtChain == null || noMoreVariantsFlag;
  }
}
