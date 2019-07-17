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

package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.kbase.IteratorType;
import com.igormaznitsa.jprol.trace.TraceEvent;

import java.io.StringReader;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.trace.TraceEvent.EXIT;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toMap;

public final class ChoicePoint {

  private final Map<String, TermVar> variables;
  private final VariableStateSnapshot varSnapshot;
  private final ProlContext context;
  private final ChoicePoint rootCp;
  private boolean noVariants;
  private final Term goalTerm;
  private Object payload;
  private ChoicePoint prevCp;
  private ChoicePoint rootLastGoalAtChain;
  private ChoicePoint subCp;

  private Term subChoicePointConnector;
  private Term thisConnector;
  private Term nextAndTerm;
  private Term nextAndTermForNextGoal;
  private Iterator<TermStruct> clauseIterator;
  private boolean cutMeet;
  private boolean notFirstProve;

  private static final AtomicLong UID_GEN = new AtomicLong();
  private final long uid;

  private ChoicePoint(
      final ChoicePoint rootCp,
      final Term goalToSolve,
      final ProlContext context,
      final Map<String, Term> predefinedVarValues
  ) {
    this.uid = UID_GEN.incrementAndGet();

    this.rootCp = rootCp == null ? this : rootCp;
    this.goalTerm = goalToSolve.getTermType() == ATOM ? newStruct(goalToSolve) : goalToSolve;
    this.context = context;

    final Term goal = assertCallable(goalToSolve.findNonVarOrDefault(goalToSolve));

    if (rootCp == null) {
      if (goal.getTermType() == ATOM) {
        this.varSnapshot = null;
        this.variables = null;
      } else {
        this.variables = goal.allNamedVarsAsMap();
        this.varSnapshot = new VariableStateSnapshot(goal, predefinedVarValues);
      }
      this.rootLastGoalAtChain = this;
      this.prevCp = null;
    } else {
      this.variables = null;
      if (goal.getTermType() == ATOM) {
        this.varSnapshot = null;
      } else {
        this.varSnapshot = new VariableStateSnapshot(rootCp.varSnapshot);
      }
      this.prevCp = rootCp.rootLastGoalAtChain;
      rootCp.rootLastGoalAtChain = this;
    }
  }

  public ChoicePoint(final String goal, final ProlContext context) {
    this(new ProlTreeBuilder(context).readPhraseAndMakeTree(new StringReader(goal)).term, context, null);
  }

  public ChoicePoint(final Term goal, final ProlContext context) {
    this(null, goal, context, null);
  }

  public ChoicePoint(final Term goal, final ProlContext context, final Map<String, Term> predefinedVarValues) {
    this(null, goal, context, predefinedVarValues);
  }

  public long getUid() {
    return this.uid;
  }

  @Override
  public boolean equals(final Object that) {
    if (this == that) {
      return true;
    }
    if (that instanceof ChoicePoint) {
      return this.uid == ((ChoicePoint) that).uid;
    }
    return false;
  }

  @Override
  public int hashCode() {
    return (int) ((this.uid >>> 32) ^ this.uid);
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
    final TermVar var = getVarForName(varName);
    if (var == null) {
      throw new IllegalArgumentException("Unknown variable for name \'" + varName + '\'');
    }
    return var.toNumber();
  }

  public String getVarAsText(final String varName) {
    final TermVar var = getVarForName(varName);
    if (var == null) {
      throw new IllegalArgumentException("Unknown variable for name \'" + varName + '\'');
    }
    final Term value = var.getValue();
    if (value == null) {
      return null;
    } else {
      return value.toSrcString();
    }
  }

  public TermVar getVarForName(final String name) {
    return this.variables == null ? null : this.variables.get(requireNonNull(name));
  }

  public ChoicePoint replaceLastGoalAtChain(final Term goal) {
    this.context.fireTraceEvent(EXIT, this.rootCp.rootLastGoalAtChain);

    final ChoicePoint newGoal = new ChoicePoint(this.rootCp, goal, this.context, null);
    final ChoicePoint prevGoal = newGoal.prevCp;
    if (prevGoal != null) {
      newGoal.prevCp = prevGoal.prevCp;
      newGoal.nextAndTerm = prevGoal.nextAndTerm;
      newGoal.nextAndTermForNextGoal = prevGoal.nextAndTermForNextGoal;
    }
    return newGoal;
  }

  @SuppressWarnings("unchecked")
  public <T> T getPayload() {
    return (T) this.payload;
  }

  public void setPayload(final Object obj) {
    this.payload = obj;
  }

  public Term getGoalTerm() {
    return this.goalTerm;
  }

  public ProlContext getContext() {
    return this.context;
  }

  public Term next() {
    Term result = null;

    boolean loop = true;
    final ProlContext localcontext = this.context;

    while (loop && !Thread.currentThread().isInterrupted()) {
      if (localcontext.isDisposed()) {
        throw new ProlHaltExecutionException();
      }

      ChoicePoint goalToProcess = this.rootCp.rootLastGoalAtChain;
      if (goalToProcess == null) {
        break;
      } else {
        if (!goalToProcess.noVariants) {
          switch (goalToProcess.resolve()) {
            case FAIL: {
              this.context.fireTraceEvent(TraceEvent.FAIL, goalToProcess);
              this.context.fireTraceEvent(EXIT, goalToProcess);
              this.rootCp.rootLastGoalAtChain = goalToProcess.prevCp;
            }
            break;
            case SUCCESS: {
              // we have to renew data about last chain goal because it can be changed during the operation
              goalToProcess = this.rootCp.rootLastGoalAtChain;

              if (goalToProcess.nextAndTerm != null) {
                final ChoicePoint nextGoal = new ChoicePoint(this.rootCp, goalToProcess.nextAndTerm, localcontext, null);
                nextGoal.nextAndTerm = goalToProcess.nextAndTermForNextGoal;
              } else {
                result = this.rootCp.goalTerm;
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
          this.rootCp.rootLastGoalAtChain = goalToProcess.prevCp;
        }
      }
    }

    return result;
  }

  private ChoicePointResult resolve() {
    if (Thread.currentThread().isInterrupted()) {
      return ChoicePointResult.FAIL;
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

      if (this.subCp != null) {
        // solve subgoal
        final Term solvedTerm = this.subCp.next();

        if (this.subCp.cutMeet) {
          this.clauseIterator = null;
        }

        if (solvedTerm == null) {
          this.subCp = null;
          if (this.clauseIterator == null) {
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
          final TermStruct nextClause = this.clauseIterator.next();

          final Term goalTermForEqu;
          if (((TermStruct) this.goalTerm).isClause()) {
            goalTermForEqu = ((TermStruct) this.goalTerm).getElement(0).makeClone();
          } else {
            goalTermForEqu = this.goalTerm.makeClone();
          }

          if (!goalTermForEqu.unifyTo(nextClause.isClause() ? nextClause.getElement(0) : nextClause)) {
            throw new ProlCriticalError("Unexpectedly can't unify term with prvided by knowledge base!");
          }

          if (nextClause.isClause()) {
            this.thisConnector = goalTerm;
            this.subChoicePointConnector = nextClause.getElement(0);
            this.subCp = new ChoicePoint(nextClause.getElement(1), this.context);
            continue;
          } else {
            if (!this.goalTerm.unifyTo(nextClause)) {
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

          if (struct.isClause()) {
            final TermStruct structClone = (TermStruct) struct.makeClone();

            this.thisConnector = struct.getElement(0);
            this.subChoicePointConnector = structClone.getElement(0);

            if (arity == 1) {
              this.subCp = new ChoicePoint(structClone.getElement(0), this.context);
            } else {
              this.subCp = new ChoicePoint(structClone.getElement(1), this.context);
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
                  if (getPayload() == null) {
                    final ChoicePoint leftSubbranch = new ChoicePoint(this.rootCp, struct.getElement(0), this.context, null);
                    leftSubbranch.nextAndTerm = this.nextAndTerm;
                    setPayload(leftSubbranch);
                  } else {
                    replaceLastGoalAtChain(struct.getElement(1));
                  }
                  result = ChoicePointResult.STACK_CHANGED;
                  nonConsumed = false;
                  doLoop = false;
                }
              }
            }

            if (nonConsumed) {
              final PredicateInvoker processor = ensureProcessor(struct);
              if (processor == PredicateInvoker.NULL_PROCESSOR) {
                this.clauseIterator = this.context.getKnowledgeBase().iterate(IteratorType.ANY, struct);
                if (this.clauseIterator == null || !this.clauseIterator.hasNext()) {
                  doLoop = false;
                  resetVariants();
                  result = ChoicePointResult.FAIL;
                }
              } else {
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
    this.rootCp.cutMeet = true;
    this.rootCp.clauseIterator = null;
    this.prevCp = null;
  }

  public void cutLocally() {
    this.prevCp = null;
  }

  private PredicateInvoker ensureProcessor(final TermStruct structure) {
    PredicateInvoker processor = structure.getPredicateProcessor();
    if (processor == null) {
      processor = this.context.findProcessor(structure);
      structure.setPredicateProcessor(processor);
    }
    return processor;
  }

  public boolean hasVariants() {
    return this.rootCp.rootLastGoalAtChain == null || this.noVariants;
  }
}
