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

import com.igormaznitsa.jprol.data.*;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.jprol.kbase.IteratorType;
import com.igormaznitsa.jprol.trace.TraceEvent;
import com.igormaznitsa.jprol.utils.ProlAssertions;

import java.io.StringReader;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.trace.TraceEvent.EXIT;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toMap;

public final class JProlChoicePoint implements Comparator<Term> {

  private static final AtomicLong UID_GEN = new AtomicLong();
  private static final Consumer<String> NULL_UNDEFINED_PREDICATE_CONSUMER = x -> {
  };
  private final Map<String, TermVar> variables;
  private final VariableStateSnapshot varSnapshot;
  private final JProlContext context;
  private final JProlChoicePoint rootChoicePoint;
  private final Term goalTerm;
  private final long uid;
  private final boolean validate;
  private final boolean debug;
  private boolean thereAreVariants;
  private Object payload;
  private JProlChoicePoint prevCp;
  private JProlChoicePoint rootLastGoalAtChain;
  private JProlChoicePoint subCp;
  private Term subChoicePointConnector;
  private Term thisConnector;
  private Term nextAndTerm;
  private Term nextAndTermForNextGoal;
  private Iterator<TermStruct> clauseIterator;
  private boolean cutMeet;
  private boolean notFirstProve;

  private JProlChoicePoint(
      final JProlChoicePoint rootChoicePoint,
      final Term goalToSolve,
      final JProlContext context,
      final boolean debug,
      final boolean validate,
      final Map<String, Term> presetVarValues
  ) {
    this.uid = UID_GEN.incrementAndGet();

    this.thereAreVariants = true;
    this.validate = validate;
    this.debug = debug;

    this.rootChoicePoint = rootChoicePoint == null ? this : rootChoicePoint;
    this.goalTerm = goalToSolve.getTermType() == ATOM ? newStruct(goalToSolve) : goalToSolve;
    this.context = context;

    final Term goal = goalToSolve.findNonVarOrSame();

    if (this.isArgsValidate()) {
      ProlAssertions.assertCallable(goal);
    }

    if (rootChoicePoint == null) {
      if (goal.getTermType() == ATOM) {
        this.varSnapshot = null;
        this.variables = null;
      } else {
        this.variables = goal.allNamedVarsAsMap();
        this.varSnapshot = new VariableStateSnapshot(goal, presetVarValues);
      }
      this.rootLastGoalAtChain = this;
      this.prevCp = null;
    } else {
      this.variables = null;
      if (goal.getTermType() == ATOM) {
        this.varSnapshot = null;
      } else {
        this.varSnapshot = new VariableStateSnapshot(rootChoicePoint.varSnapshot);
      }
      this.prevCp = rootChoicePoint.rootLastGoalAtChain;
      rootChoicePoint.rootLastGoalAtChain = this;
    }
  }

  public JProlChoicePoint(final String goal, final JProlContext context) {
    this(new JProlTreeBuilder(context).readPhraseAndMakeTree(new StringReader(goal)).term, context, null);
  }

  public JProlChoicePoint(final Term goal, final JProlContext context) {
    this(null, goal, context, context.isDebug(), context.isTemplateValidate(), null);
  }

  public JProlChoicePoint(final Term goal, final JProlContext context, final Map<String, Term> predefinedVarValues) {
    this(null, goal, context, context.isDebug(), context.isTemplateValidate(), predefinedVarValues);
  }

  public JProlChoicePoint makeForGoal(final Term goal) {
    return new JProlChoicePoint(null, goal, this.context, this.debug, this.validate, null);
  }

  public long getUid() {
    return this.uid;
  }

  public boolean isDebug() {
    return this.debug;
  }

  public boolean isArgsValidate() {
    return this.validate;
  }

  @Override
  public boolean equals(final Object that) {
    if (this == that) {
      return true;
    }
    if (that instanceof JProlChoicePoint) {
      return this.uid == ((JProlChoicePoint) that).uid;
    }
    return false;
  }

  @Override
  public int hashCode() {
    return (int) ((this.uid >>> 32) ^ this.uid);
  }

  public Map<String, Term> findAllGroundedVars() {
    return this.variables.entrySet()
        .stream()
        .filter(v -> v.getValue().isGround())
        .collect(toMap(Map.Entry::getKey, e -> e.getValue().makeClone()));
  }

  @Override
  public String toString() {
    return (this.isCompleted() ? "Completed " : "Active ") + "Goal(" + this.goalTerm.toString() + ')';
  }

  public Optional<TermVar> findVar(final String name) {
    return this.variables == null ? Optional.empty() : Optional.ofNullable(this.variables.get(requireNonNull(name)));
  }

  public JProlChoicePoint replaceLastGoalAtChain(final Term goal) {
    if (this.debug) {
      this.context.fireTraceEvent(EXIT, this.rootChoicePoint.rootLastGoalAtChain);
    }

    final JProlChoicePoint newGoal = new JProlChoicePoint(this.rootChoicePoint, goal, this.context, this.debug, this.validate, null);
    final JProlChoicePoint prevGoal = newGoal.prevCp;
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

  public JProlContext getContext() {
    return this.context;
  }

  public Term prove() {
    return this.proveNext(x -> this.context.notifyAboutUndefinedPredicate(this, x));
  }

  public Term proveWithFailForUnknown() {
    return this.proveNext(NULL_UNDEFINED_PREDICATE_CONSUMER);
  }

  private Term proveNext(final Consumer<String> unknownPredicateConsumer) {
    Term result = null;

    boolean loop = true;

    while (loop && !Thread.currentThread().isInterrupted()) {
      if (this.context.isDisposed()) {
        throw new ProlHaltExecutionException();
      }

      JProlChoicePoint goalToProcess = this.rootChoicePoint.rootLastGoalAtChain;
      if (goalToProcess == null) {
        break;
      } else {
        if (goalToProcess.thereAreVariants) {
          switch (goalToProcess.resolve(unknownPredicateConsumer)) {
            case FAIL: {
              if (this.debug) {
                this.context.fireTraceEvent(TraceEvent.FAIL, goalToProcess);
                this.context.fireTraceEvent(EXIT, goalToProcess);
              }
              this.rootChoicePoint.rootLastGoalAtChain = goalToProcess.prevCp;
            }
            break;
            case SUCCESS: {
              // we have to renew data about last chain goal because it can be changed during the operation
              goalToProcess = this.rootChoicePoint.rootLastGoalAtChain;

              if (goalToProcess.nextAndTerm == null) {
                result = this.rootChoicePoint.goalTerm;
                loop = false;
              } else {
                final JProlChoicePoint nextGoal = new JProlChoicePoint(this.rootChoicePoint, goalToProcess.nextAndTerm, this.context, this.debug, this.validate, null);
                nextGoal.nextAndTerm = goalToProcess.nextAndTermForNextGoal;
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
          if (this.debug) {
            this.context.fireTraceEvent(EXIT, goalToProcess);
          }
          this.rootChoicePoint.rootLastGoalAtChain = goalToProcess.prevCp;
        }
      }
    }

    return result;
  }

  private JProlChoicePointResult resolve(final Consumer<String> unknownPredicateConsumer) {
    if (Thread.currentThread().isInterrupted()) {
      return JProlChoicePointResult.FAIL;
    }
    final TraceEvent traceEvent;
    if (this.notFirstProve) {
      traceEvent = TraceEvent.REDO;
    } else {
      traceEvent = TraceEvent.CALL;
      this.notFirstProve = true;
    }
    if (this.debug) {
      this.context.fireTraceEvent(traceEvent, this);
    }

    JProlChoicePointResult result = JProlChoicePointResult.FAIL;

    boolean doLoop = true;

    while (doLoop) {
      // reset variables to their initial state
      if (this.varSnapshot != null) {
        this.varSnapshot.resetToState();
      }

      if (this.subCp != null) {
        // solve subgoal
        final Term solvedTerm = this.subCp.proveNext(unknownPredicateConsumer);

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
          result = JProlChoicePointResult.SUCCESS;
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
            this.subCp = new JProlChoicePoint(nextClause.getElement(1), this.context);
            continue;
          } else {
            if (!this.goalTerm.unifyTo(nextClause)) {
              throw new ProlCriticalError("Impossible situation #0009824");
            }
            result = JProlChoicePointResult.SUCCESS;
            break;
          }
        } else {
          this.clauseIterator = null;
          cutVariants();
          break;
        }
      }

      switch (this.goalTerm.getTermType()) {
        case ATOM: {
          final String text = this.goalTerm.getText();
          if (this.context.hasZeroArityPredicateForName(text)) {
            result = JProlChoicePointResult.SUCCESS;
          } else {
            this.context.notifyAboutUndefinedPredicate(this, this.goalTerm.getSignature());
            result = JProlChoicePointResult.FAIL;
          }
          cutVariants();
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
              this.subCp = new JProlChoicePoint(structClone.getElement(0), this.context);
            } else {
              this.subCp = new JProlChoicePoint(structClone.getElement(1), this.context);
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
                result = JProlChoicePointResult.SUCCESS;
                this.thereAreVariants = false;
              } else if (len == 2 && "!!".equals(functorText)) {
                // cut local
                this.cutLocally();
                nonConsumed = false;
                doLoop = false;
                this.thereAreVariants = false;
                result = JProlChoicePointResult.SUCCESS;
              }
            } else if (arity == 2) {
              final int textLen = functorText.length();
              if (textLen == 1) {
                if (functorText.charAt(0) == ',') {// and
                  final JProlChoicePoint leftSubgoal = replaceLastGoalAtChain(struct.getElement(0));
                  leftSubgoal.nextAndTerm = struct.getElement(1);
                  leftSubgoal.nextAndTermForNextGoal = this.nextAndTerm;

                  result = JProlChoicePointResult.STACK_CHANGED;

                  doLoop = false;
                  nonConsumed = false;
                } else if (functorText.charAt(0) == ';') {// or
                  if (getPayload() == null) {
                    final JProlChoicePoint leftSubbranch = new JProlChoicePoint(this.rootChoicePoint, struct.getElement(0), this.context, this.debug, this.validate, null);
                    leftSubbranch.nextAndTerm = this.nextAndTerm;
                    setPayload(leftSubbranch);
                  } else {
                    replaceLastGoalAtChain(struct.getElement(1));
                  }
                  result = JProlChoicePointResult.STACK_CHANGED;
                  nonConsumed = false;
                  doLoop = false;
                }
              }
            }

            if (nonConsumed) {
              final PredicateInvoker foundProcessor = findProcessorInLibraries(struct);
              if (foundProcessor == PredicateInvoker.NULL_PROCESSOR) {
                this.clauseIterator = this.context.getKnowledgeBase().iterate(
                    this.context.getKnowledgeContext(),
                    IteratorType.ANY,
                    struct,
                    unknownPredicateConsumer
                );
                if (!this.clauseIterator.hasNext()) {
                  doLoop = false;
                  this.cutVariants();
                  result = JProlChoicePointResult.FAIL;
                }
              } else {
                if (foundProcessor.isEvaluable() || foundProcessor.isDetermined()) {
                  this.cutVariants();
                }

                if (foundProcessor.execute(this, struct)) {
                  result = JProlChoicePointResult.SUCCESS;
                } else {
                  result = JProlChoicePointResult.FAIL;
                }

                if (result == JProlChoicePointResult.SUCCESS && foundProcessor.doesChangeGoalChain()) {
                  result = JProlChoicePointResult.STACK_CHANGED;
                }

                doLoop = false;
              }
            }
          }
        }
        break;
        default: {
          result = JProlChoicePointResult.FAIL;
          doLoop = false;
          this.cutVariants();
        }
        break;
      }
    }

    return result;
  }

  public void cutVariants() {
    this.thereAreVariants = false;
  }

  public void cut() {
    this.rootChoicePoint.cutMeet = true;
    this.rootChoicePoint.clauseIterator = null;
    this.prevCp = null;
  }

  public void cutLocally() {
    this.prevCp = null;
  }

  private PredicateInvoker findProcessorInLibraries(final TermStruct structure) {
    PredicateInvoker processor = structure.getPredicateProcessor();
    if (processor == null) {
      processor = this.context.findProcessor(structure);
      structure.setPredicateProcessor(processor);
    }
    return processor;
  }

  public boolean isCompleted() {
    return this.rootChoicePoint.rootLastGoalAtChain == null || !this.thereAreVariants;
  }

  @Override
  public int compare(Term term1, Term term2) {
    if (term1 == term2) {
      return 0;
    }

    term1 = term1.findNonVarOrSame();
    term2 = term2.findNonVarOrSame();

    final int result;
    switch (term1.findNonVarOrSame().getTermType()) {
      case ATOM: {
        if (term2 instanceof CompoundTerm) {
          result = -1;
        } else if (term2.getTermType() == ATOM) {
          if (term1 instanceof NumericTerm) {
            if (term2 instanceof NumericTerm) {
              if (term1 instanceof TermDouble || term2 instanceof TermDouble) {
                result = Double.compare(term1.toNumber().doubleValue(), term2.toNumber().doubleValue());
              } else {
                result = Long.compare(term1.toNumber().longValue(), term2.toNumber().longValue());
              }
            } else {
              result = -1;
            }
          } else {
            result = term2 instanceof NumericTerm ? 1 : term1.getText().compareTo(term2.getText());
          }
        } else {
          result = 1;
        }
      }
      break;
      case LIST:
      case STRUCT: {
        if (term2 instanceof CompoundTerm) {
          final TermStruct struct1 = (TermStruct) term1;
          final TermStruct struct2 = (TermStruct) term2;
          int res = Integer.compare(struct1.getArity(), struct2.getArity());
          if (res == 0) {
            res = struct1.getFunctor().getText().compareTo(struct2.getFunctor().getText());
            if (res == 0) {
              for (int i = 0; i < struct1.getArity() && res == 0; i++) {
                res = this.compare(struct1.getElement(i), struct2.getElement(i));
              }
            }
          }
          result = res;
        } else {
          result = 1;
        }
      }
      break;
      case VAR: {
        if (term2.getTermType() == VAR) {
          result = term1.getText().compareTo(term2.getText());
        } else {
          result = -1;
        }
      }
      break;
      default:
        result = 1;
    }
    return result;
  }
}
