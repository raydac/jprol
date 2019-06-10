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
import com.igormaznitsa.prol.utils.Utils;

import java.io.IOException;
import java.util.Map;

import static com.igormaznitsa.prol.data.TermType.ATOM;
import static com.igormaznitsa.prol.trace.TraceEvent.EXIT;
import static java.util.stream.Collectors.toMap;

public final class ChoicePoint {

  private final ChoicePoint rootGoal;
  private final Map<String, Var> variables;
  private final VariableStateSnapshot varSnapshot;
  private final ProlContext context;
  private Object auxObject;
  private boolean noVariants;
  private Term goalTerm;
  private ChoicePoint prevChoicePoint;
  private ChoicePoint rootLastGoalAtChain;
  private ChoicePoint subChoicePoint;
  private Term subChoicePointConnector;
  private Term thisConnector;
  private Term nextAndTerm;
  private Term nextAndTermForNextGoal;
  private ClauseIterator clauseIterator;
  private boolean cutMeet;
  private boolean notFirstProve;

  private ChoicePoint(
      final ChoicePoint rootGoal,
      final Term goalToSolve,
      final ProlContext context,
      final Map<String, Term> predefinedVarValues
  ) {
    this.rootGoal = rootGoal == null ? this : rootGoal;
    this.goalTerm = goalToSolve.getTermType() == ATOM ? new TermStruct(goalToSolve) : goalToSolve;
    this.context = context;

    final Term goal = assertCallable(goalToSolve.findNonVarOrDefault(goalToSolve));

    if (rootGoal == null) {
      if (goal.getTermType() == ATOM) {
        this.varSnapshot = null;
        this.variables = null;
      } else {
        this.variables = Utils.fillTableWithVars(goal);
        this.varSnapshot = new VariableStateSnapshot(goal, predefinedVarValues);
      }
      this.rootLastGoalAtChain = this;
      this.prevChoicePoint = null;
    } else {
      this.variables = null;
      if (goal.getTermType() == ATOM) {
        this.varSnapshot = null;
      } else {
        this.varSnapshot = new VariableStateSnapshot(rootGoal.varSnapshot);
      }
      this.prevChoicePoint = rootGoal.rootLastGoalAtChain;
      rootGoal.rootLastGoalAtChain = this;
    }
  }

  public ChoicePoint(final String goal, final ProlContext context) throws IOException {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(goal), context, null);
  }

  public ChoicePoint(final ProlReader reader, final ProlContext context, final Map<String, Term> predefVarValues) throws IOException {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(reader), context, predefVarValues);
  }

  public ChoicePoint(final Term goal, final ProlContext context) {
    this(null, goal, context, null);
  }

  public ChoicePoint(final Term goal, final ProlContext context, final Map<String, Term> predefinedVarValues) {
    this(null, goal, context, predefinedVarValues);
  }

  private static Term assertCallable(final Term term) {
    switch (term.getTermType()) {
      case ATOM: {
        if (term instanceof NumericTerm) {
          throw new ProlTypeErrorException("callable", term);
        }
      }
      break;
      case VAR: {
        if (!term.isGround()) {
          throw new ProlInstantiationErrorException("callable", term);
        }
      }
      break;
      case LIST: {
        throw new ProlTypeErrorException("callable", term);
      }
      default:
        break;
    }
    return term;
  }

  public Map<String, Term> findAllGroundedVars() {
    return this.variables.entrySet()
        .stream()
        .filter(v -> v.getValue().isGround())
        .collect(toMap(Map.Entry::getKey, e -> e.getValue().makeClone()));
  }

  @Override
  public String toString() {
    return (this.hasVariants() ? "Completed " : "Active ") + "Goal(" + this.goalTerm.toString() + ')';
  }

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

  public ChoicePoint replaceLastGoalAtChain(final Term goal) {
    this.context.fireTraceEvent(EXIT, this.rootGoal.rootLastGoalAtChain);

    final ChoicePoint newGoal = new ChoicePoint(this.rootGoal, goal, this.context, null);
    final ChoicePoint prevGoal = newGoal.prevChoicePoint;
    if (prevGoal != null) {
      newGoal.prevChoicePoint = prevGoal.prevChoicePoint;
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

  public Term getGoalTerm() {
    return this.goalTerm;
  }

  public ProlContext getContext() {
    return this.context;
  }

  public Term next() throws InterruptedException {
    Term result = null;

    boolean loop = true;
    final ProlContext localcontext = this.context;

    while (loop && !Thread.currentThread().isInterrupted()) {
      if (localcontext.isHalted()) {
        throw new ProlHaltExecutionException();
      }

      ChoicePoint goalToProcess = this.rootGoal.rootLastGoalAtChain;
      if (goalToProcess == null) {
        break;
      } else {
        if (!goalToProcess.noVariants) {
          switch (goalToProcess.resolve()) {
            case FAIL: {
              this.context.fireTraceEvent(TraceEvent.FAIL, goalToProcess);
              this.context.fireTraceEvent(EXIT, goalToProcess);
              this.rootGoal.rootLastGoalAtChain = goalToProcess.prevChoicePoint;
            }
            break;
            case SUCCESS: {
              // we have to renew data about last chain goal because it can be changed during the operation
              goalToProcess = this.rootGoal.rootLastGoalAtChain;

              if (goalToProcess.nextAndTerm != null) {
                final ChoicePoint nextGoal = new ChoicePoint(this.rootGoal, goalToProcess.nextAndTerm, localcontext, null);
                nextGoal.nextAndTerm = goalToProcess.nextAndTermForNextGoal;
              } else {
                result = this.rootGoal.goalTerm;
                loop = false;
              }
            }
            break;
            case STACK_CHANGED: {
            }
            break;
            default:
              throw new Error("Unexpected status");
          }
        } else {
          this.context.fireTraceEvent(EXIT, goalToProcess);
          this.rootGoal.rootLastGoalAtChain = goalToProcess.prevChoicePoint;
        }
      }
    }

    return result;
  }

  private ChoicePointResult resolve() throws InterruptedException {
    if (Thread.currentThread().isInterrupted()) {
      throw new InterruptedException();
    }
      if (this.notFirstProve) {
        this.context.fireTraceEvent(TraceEvent.REDO, this);
      } else {
        this.notFirstProve = true;
        this.context.fireTraceEvent(TraceEvent.CALL, this);
      }

    ChoicePointResult result = ChoicePointResult.FAIL;

    boolean doLoop = true;

    while (doLoop) {
      // reset variables to their initial state
      if (this.varSnapshot != null) {
        this.varSnapshot.resetToState();
      }

      if (this.subChoicePoint != null) {
        // solve subgoal
        final Term solvedTerm = this.subChoicePoint.next();

        if (this.subChoicePoint.cutMeet) {
          this.clauseIterator = null;
        }

        if (solvedTerm == null) {
          this.subChoicePoint = null;
          if (this.clauseIterator == null) {
            result = ChoicePointResult.FAIL;
            break;
          }
        } else {
          if (!this.thisConnector.unifyTo(this.subChoicePointConnector)) {
            throw new ProlCriticalError("Critical error #980234");
          }
          result = ChoicePointResult.SUCCESS;
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
            this.subChoicePointConnector = structFromBase.getElement(0);
            this.subChoicePoint = new ChoicePoint(structFromBase.getElement(1), this.context);
            continue;
          } else {
            if (!this.goalTerm.unifyTo(structFromBase)) {
              throw new ProlCriticalError("Impossible situation #0009824");
            }
            result = ChoicePointResult.SUCCESS;
            break;
          }
        } else {
          this.clauseIterator = null;
          resetVariants();
          break;
        }
      }

      switch (this.goalTerm.getTermType()) {
        case ATOM: {
          final String text = this.goalTerm.getText();
          result = this.context.hasZeroArityPredicateForName(text) ? ChoicePointResult.SUCCESS : ChoicePointResult.FAIL;
          resetVariants();
          doLoop = false;
        }
        break;
        case STRUCT: {
          final TermStruct struct = (TermStruct) goalTerm;
          final int arity = struct.getArity();

          if (struct.isFunctorLikeRuleDefinition()) {
            final TermStruct structClone = (TermStruct) struct.makeClone();

            this.thisConnector = struct.getElement(0);
            this.subChoicePointConnector = structClone.getElement(0);

            if (arity == 1) {
              this.subChoicePoint = new ChoicePoint(structClone.getElement(0), this.context);
            } else {
              this.subChoicePoint = new ChoicePoint(structClone.getElement(1), this.context);
            }
          } else {

            final Term functor = struct.getFunctor();
            final String functorText = functor.getText();

            boolean nonConsumed = true;

            if (arity == 0) {
              final int len = functorText.length();
              if (len == 1 && functorText.charAt(0) == '!') {
                // cut
                cut();
                nonConsumed = false;
                doLoop = false;
                result = ChoicePointResult.SUCCESS;
                this.noVariants = true;
              } else if (len == 2 && "!!".equals(functorText)) {
                // cut local
                cutLocally();
                nonConsumed = false;
                doLoop = false;
                this.noVariants = true;
                result = ChoicePointResult.SUCCESS;
              }
            } else if (arity == 2) {
              final int textLen = functorText.length();
              if (textLen == 1) {
                if (functorText.charAt(0) == ',') {// and
                  final ChoicePoint leftSubgoal = replaceLastGoalAtChain(struct.getElement(0));
                  leftSubgoal.nextAndTerm = struct.getElement(1);
                  leftSubgoal.nextAndTermForNextGoal = this.nextAndTerm;

                  result = ChoicePointResult.STACK_CHANGED;

                  doLoop = false;
                  nonConsumed = false;
                } else if (functorText.charAt(0) == ';') {// or
                  if (getAuxObject() == null) {
                    // left subbranch
                    final ChoicePoint leftSubbranch = new ChoicePoint(this.rootGoal, struct.getElement(0), this.context, null);
                    leftSubbranch.nextAndTerm = this.nextAndTerm;
                    setAuxObject(leftSubbranch);
                  } else {
                    // right subbranch
                    replaceLastGoalAtChain(struct.getElement(1));
                  }
                  result = ChoicePointResult.STACK_CHANGED;
                  nonConsumed = false;
                  doLoop = false;
                }
              }
            }

            if (nonConsumed) {
              final PredicateProcessor processor = ensureProcessor(struct);
              if (processor == PredicateProcessor.NULL_PROCESSOR) {
                // just a struct
                // find it at knowledge base
                this.clauseIterator = this.context.getKnowledgeBase().getClauseIterator(struct);
                if (this.clauseIterator == null || !this.clauseIterator.hasNext()) {
                  doLoop = false;
                  resetVariants();
                  result = ChoicePointResult.FAIL;
                }
              } else {
                // it is a processor
                if (processor.isEvaluable() || processor.isDetermined()) {
                  resetVariants();
                }

                if (processor.execute(this, struct)) {
                  result = ChoicePointResult.SUCCESS;
                } else {
                  result = ChoicePointResult.FAIL;
                }

                if (result == ChoicePointResult.SUCCESS && processor.doesChangeGoalChain()) {
                  result = ChoicePointResult.STACK_CHANGED;
                }

                doLoop = false;
              }
            }
          }
        }
        break;
        default: {
          result = ChoicePointResult.FAIL;
          doLoop = false;
          resetVariants();
        }
        break;
      }
    }

    return result;
  }

  public void resetVariants() {
    this.noVariants = true;
  }

  public void cut() {
    this.rootGoal.cutMeet = true;
    this.rootGoal.clauseIterator = null;
    this.prevChoicePoint = null;
  }

  public void cutLocally() {
    this.prevChoicePoint = null;
  }

  private PredicateProcessor ensureProcessor(final TermStruct structure) {
    PredicateProcessor processor = structure.getPredicateProcessor();
    if (processor == null) {
      processor = this.context.findProcessor(structure);
      structure.setPredicateProcessor(processor);
    }
    return processor;
  }

  public boolean hasVariants() {
    return this.rootGoal.rootLastGoalAtChain == null || this.noVariants;
  }
}
