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

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.OPERATOR;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.trace.TraceEvent.EXIT;
import static java.util.Objects.requireNonNull;
import static java.util.stream.Collectors.toMap;

import com.igormaznitsa.jprol.data.CompoundTerm;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlChoicePointInterruptedException;
import com.igormaznitsa.jprol.exceptions.ProlChoicePointStackOverflowException;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.jprol.kbase.IteratorType;
import com.igormaznitsa.jprol.trace.TraceEvent;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;

public final class JProlChoicePoint implements Comparator<Term> {

  private static final BiConsumer<String, Term> NULL_UNDEFINED_PREDICATE_CONSUMER =
      (signature, term) -> {
      };
  private final Map<String, TermVar> variables;
  private final VariableStateSnapshot varSnapshot;
  private final JProlContext context;
  private final JProlChoicePoint rootChoicePoint;
  private final Term goalTerm;
  private final boolean verify;
  private final boolean trace;
  private boolean hasLogicalAlternatives;
  private Object internalAuxiliaryObject;
  private Object payload;
  private JProlChoicePoint prevChoicePoint;
  private JProlChoicePoint parentChoicePoint;
  private JProlChoicePoint childChoicePoint;
  private Term childConnectingTerm;
  private Term thisConnector;
  private Term nextAndTerm;
  private Term nextAndTermForNextGoal;
  private Iterator<TermStruct> clauseIterator;
  private boolean cutActivated;
  private boolean firstResolveCall = true;

  private JProlChoicePoint(
      final JProlChoicePoint rootChoicePoint,
      final Term goalToSolve,
      final JProlContext context,
      final boolean trace,
      final boolean verify,
      final Map<String, Term> presetVarValues,
      final Object internalAuxiliaryObject,
      final Object payload
  ) {
    this.internalAuxiliaryObject = internalAuxiliaryObject;
    this.payload = payload;

    this.hasLogicalAlternatives = true;
    this.verify = verify;
    this.trace = trace;

    this.rootChoicePoint = rootChoicePoint == null ? this : rootChoicePoint;
    this.goalTerm = goalToSolve.getTermType() == ATOM ? newStruct(goalToSolve) : goalToSolve;
    this.context = context;

    final Term goal = goalToSolve.tryGround();

    if (this.verify) {
      ProlAssertions.assertCallable(goal);
    }

    if (rootChoicePoint == null) {
      if (goal.getTermType() == ATOM) {
        this.varSnapshot = null;
        this.variables = null;
      } else {
        this.variables = goal.findAllNamedVariables();
        this.varSnapshot = new VariableStateSnapshot(goal, presetVarValues);
      }
      this.parentChoicePoint = this;
      this.prevChoicePoint = null;
    } else {
      this.variables = null;
      if (goal.getTermType() == ATOM) {
        this.varSnapshot = null;
      } else {
        this.varSnapshot = new VariableStateSnapshot(rootChoicePoint.varSnapshot);
      }
      this.prevChoicePoint = rootChoicePoint.parentChoicePoint;
      rootChoicePoint.parentChoicePoint = this;
      this.payload = rootChoicePoint.payload;
    }
  }

  JProlChoicePoint(final Term goal, final JProlContext context,
                   final Map<String, Term> predefinedVarValues, final Object payload) {
    this(null, goal, context, context.isTrace(), context.isVerify(), predefinedVarValues, null,
        payload);
  }

  public boolean isTrace() {
    return this.trace;
  }

  public boolean isVerify() {
    return this.verify;
  }

  public Map<String, Term> findAllGroundedVars() {
    return this.variables == null ? Map.of() : this.variables.entrySet()
        .stream()
        .filter(v -> v.getValue().isGround())
        .collect(toMap(Map.Entry::getKey, e -> e.getValue().makeClone()));
  }

  @Override
  public String toString() {
    return (this.isCompleted() ? "Completed " : "Active ") + "Goal(" + this.goalTerm.toString() +
        ')';
  }

  public Optional<TermVar> findVar(final String name) {
    return this.variables == null ? Optional.empty() :
        Optional.ofNullable(this.variables.get(requireNonNull(name)));
  }

  public JProlChoicePoint replaceLastGoalAtChain(final Term goal) {
    if (this.trace) {
      this.context.fireTraceEvent(EXIT, this.rootChoicePoint.parentChoicePoint);
    }

    final JProlChoicePoint newGoal =
        new JProlChoicePoint(this.rootChoicePoint, goal, this.context, this.trace,
            this.verify,
            null, null, this.payload);

    final JProlChoicePoint prevGoal = newGoal.prevChoicePoint;
    if (prevGoal != null) {
      newGoal.prevChoicePoint = prevGoal.prevChoicePoint;
      newGoal.nextAndTerm = prevGoal.nextAndTerm;
      newGoal.nextAndTermForNextGoal = prevGoal.nextAndTermForNextGoal;
    }
    return newGoal;
  }

  /**
   * Get payload object.
   *
   * @return payload object, can be null
   */
  @SuppressWarnings("unchecked")
  public <T> T getPayload() {
    return (T) this.payload;
  }

  /**
   * Payload value to be carried and accessible.
   *
   * @param payload payload object, can be null
   */
  public void setPayload(Object payload) {
    this.payload = payload;
  }

  /**
   * Get the special field value, it saves some temporary data between calls.
   *
   * @return saved value
   */
  @SuppressWarnings("unchecked")
  public <T> T getInternalObject() {
    return (T) this.internalAuxiliaryObject;
  }

  /**
   * Special field to save some temporary data between calls.
   *
   * @param obj object to be saved, can be null
   */
  public void setInternalObject(final Object obj) {
    this.internalAuxiliaryObject = obj;
  }

  public Term getGoalTerm() {
    return this.goalTerm;
  }

  public JProlContext getContext() {
    return this.context;
  }

  /**
   * Prove current goal.
   *
   * @return proved term or null if no result.
   */
  public Term prove() {
    if (this.context.isDisposed()) {
      throw new ProlHaltExecutionException("Context disposed", 0);
    }
    return this.proveNext((s, t) -> this.context.notifyAboutUndefinedPredicate(this, s, t));
  }

  public Term proveIgnoringUnknownPredicates() {
    return this.proveNext(NULL_UNDEFINED_PREDICATE_CONSUMER);
  }

  private Term proveNext(final BiConsumer<String, Term> unknownPredicateConsumer) {
    Term result = null;
    boolean loop = true;

    while (loop) {
      if (this.context.isDisposed()) {
        throw new ProlChoicePointInterruptedException("context disposed", this);
      }

      JProlChoicePoint goalToProcess = this.rootChoicePoint.parentChoicePoint;
      if (goalToProcess == null) {
        break;
      } else {
        if (goalToProcess.hasLogicalAlternatives) {
          try {
            switch (goalToProcess.resolve(unknownPredicateConsumer)) {
              case FAIL: {
                if (this.trace) {
                  this.context.fireTraceEvent(TraceEvent.FAIL, goalToProcess);
                  this.context.fireTraceEvent(EXIT, goalToProcess);
                }
                this.rootChoicePoint.parentChoicePoint = goalToProcess.prevChoicePoint;
              }
              break;
              case SUCCESS: {
                // we have to renew data about last chain goal because it can be changed during the operation
                goalToProcess = this.rootChoicePoint.parentChoicePoint;

                if (goalToProcess.nextAndTerm == null) {
                  result = this.rootChoicePoint.goalTerm;
                  loop = false;
                } else {
                  final JProlChoicePoint nextGoal =
                      new JProlChoicePoint(this.rootChoicePoint, goalToProcess.nextAndTerm,
                          this.context, this.trace, this.verify, null, null,
                          this.payload);
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
          } catch (StackOverflowError ex) {
            throw new ProlChoicePointStackOverflowException(
                "Caught stack overflow error during prove", this);
          }
        } else {
          if (this.trace) {
            this.context.fireTraceEvent(EXIT, goalToProcess);
          }
          this.rootChoicePoint.parentChoicePoint = goalToProcess.prevChoicePoint;
        }
      }
    }

    return result;
  }

  private JProlChoicePointResult resolve(final BiConsumer<String, Term> unknownPredicateConsumer) {
    final TraceEvent traceEvent;
    if (this.firstResolveCall) {
      traceEvent = TraceEvent.CALL;
      this.firstResolveCall = false;
    } else {
      traceEvent = TraceEvent.REDO;
    }
    if (this.trace) {
      this.context.fireTraceEvent(traceEvent, this);
    }

    JProlChoicePointResult result = JProlChoicePointResult.FAIL;

    boolean doLoop = true;

    while (doLoop) {
      if (this.context.isDisposed()) {
        throw new ProlChoicePointInterruptedException("context disposed", this);
      }

      // reset variables to their initial state
      if (this.varSnapshot != null) {
        this.varSnapshot.resetToState();
      }

      if (this.childChoicePoint != null) {
        // solve sub-goal
        final Term solvedTerm = this.childChoicePoint.proveNext(unknownPredicateConsumer);

        if (this.childChoicePoint.cutActivated) {
          this.clauseIterator = null;
        }

        if (solvedTerm == null) {
          this.childChoicePoint = null;
          if (this.clauseIterator == null) {
            break;
          }
        } else {
          if (!this.thisConnector.unifyWith(this.childConnectingTerm)) {
            throw new ProlCriticalError("Critical error #980234");
          }
          result = JProlChoicePointResult.SUCCESS;
          break;
        }
      }

      final Term theTerm = this.goalTerm.tryGroundOrDefault(this.goalTerm);


      if (this.clauseIterator != null) {
        // next clause
        if (this.clauseIterator.hasNext()) {
          final TermStruct nextClause = this.clauseIterator.next();

          final Term goalTermForEqu;
          if (((TermStruct) theTerm).isClause()) {
            goalTermForEqu = ((TermStruct) theTerm).getArgumentAt(0).makeClone();
          } else {
            goalTermForEqu = theTerm.makeClone();
          }

          if (!goalTermForEqu
              .unifyWith(nextClause.isClause() ? nextClause.getArgumentAt(0) : nextClause)) {
            throw new ProlCriticalError(
                "Unexpectedly can't unify term with provided by knowledge base!");
          }

          if (nextClause.isClause()) {
            this.thisConnector = theTerm;
            this.childConnectingTerm = nextClause.getArgumentAt(0);
            this.childChoicePoint =
                this.context.makeChoicePoint(nextClause.getArgumentAt(1), this.payload);
            continue;
          } else {
            if (!theTerm.unifyWith(nextClause)) {
              throw new ProlCriticalError("Impossible situation #0009824");
            }
            result = JProlChoicePointResult.SUCCESS;
            break;
          }
        } else {
          this.clauseIterator = null;
          resetLogicalAlternativesFlag();
          break;
        }
      }

      switch (theTerm.getTermType()) {
        case ATOM: {
          final String text = theTerm.getText();
          if (this.context.hasZeroArityPredicateForName(text)) {
            result = JProlChoicePointResult.SUCCESS;
          } else {
            this.context.notifyAboutUndefinedPredicate(this, theTerm.getSignature(),
                theTerm);
            result = JProlChoicePointResult.FAIL;
          }
          resetLogicalAlternativesFlag();
          doLoop = false;
        }
        break;
        case STRUCT: {
          final TermStruct struct = (TermStruct) theTerm;
          final int arity = struct.getArity();

          if (struct.isClause()) {
            final TermStruct structClone = (TermStruct) struct.makeClone();

            this.thisConnector = struct.getArgumentAt(0);
            this.childConnectingTerm = structClone.getArgumentAt(0);

            if (arity == 1) {
              this.childChoicePoint =
                  this.context.makeChoicePoint(structClone.getArgumentAt(0), this.payload);
            } else {
              this.childChoicePoint =
                  this.context.makeChoicePoint(structClone.getArgumentAt(1), this.payload);
            }
          } else {
            final Term functor = struct.getFunctor();
            final String functorText = functor.getText();
            boolean nonConsumed = true;

            if (arity < 3) {
              final int functorTextLength = functorText.length();
              switch (functorText.charAt(0)) {
                case '!': {
                  if (arity == 0) {
                    switch (functorTextLength) {
                      case 1: { // standard cut !/0
                        this.fullCut();
                        nonConsumed = false;
                        doLoop = false;
                        result = JProlChoicePointResult.SUCCESS;
                      }
                      break;
                      case 2: { // local cut !!/9
                        if (functorText.charAt(1) == '!') {
                          this.localCut();
                          nonConsumed = false;
                          doLoop = false;
                          result = JProlChoicePointResult.SUCCESS;
                        }
                      }
                      break;
                    }
                  }
                }
                break;
                case ',': { // and ,/2
                  if (arity == 2 && functorTextLength == 1) {
                    final JProlChoicePoint leftSubGoal =
                        replaceLastGoalAtChain(struct.getArgumentAt(0));
                    leftSubGoal.nextAndTerm = struct.getArgumentAt(1);
                    leftSubGoal.nextAndTermForNextGoal = this.nextAndTerm;

                    result = JProlChoicePointResult.STACK_CHANGED;

                    doLoop = false;
                    nonConsumed = false;
                  }
                }
                break;
                case ';': { // or  ;/2
                  if (arity == 2 && functorTextLength == 1) {
                    if (getInternalObject() == null) {
                      final JProlChoicePoint leftSubbranch =
                          new JProlChoicePoint(this.rootChoicePoint, struct.getArgumentAt(0),
                              this.context, this.trace, this.verify, null, null,
                              this.payload);
                      leftSubbranch.nextAndTerm = this.nextAndTerm;
                      this.setInternalObject(leftSubbranch);
                    } else {
                      this.replaceLastGoalAtChain(struct.getArgumentAt(1));
                    }
                    result = JProlChoicePointResult.STACK_CHANGED;
                    nonConsumed = false;
                    doLoop = false;
                  }
                }
                break;
              }
            }

            if (nonConsumed) {
              final PredicateInvoker foundProcessor = findProcessorInLibraries(struct);
              if (foundProcessor == PredicateInvoker.NULL_INVOKER ||
                  foundProcessor.isEvaluable()) {
                this.clauseIterator = this.context.getKnowledgeBase().iterate(
                    IteratorType.ANY,
                    struct,
                    struct.getFunctor().getTermType() == OPERATOR ?
                        NULL_UNDEFINED_PREDICATE_CONSUMER : unknownPredicateConsumer
                );
                if (!this.clauseIterator.hasNext()) {
                  doLoop = false;
                  this.resetLogicalAlternativesFlag();
                  result = JProlChoicePointResult.FAIL;
                }
              } else {
                if (foundProcessor.isDetermined()) {
                  this.resetLogicalAlternativesFlag();
                }

                if (foundProcessor.execute(this, struct)) {
                  result = JProlChoicePointResult.SUCCESS;
                } else {
                  result = JProlChoicePointResult.FAIL;
                }

                if (result == JProlChoicePointResult.SUCCESS &&
                    foundProcessor.doesChangeGoalChain()) {
                  result = JProlChoicePointResult.STACK_CHANGED;
                }

                doLoop = false;
              }
            }
          }
        }
        break;
        default: {
          doLoop = false;
          this.resetLogicalAlternativesFlag();
        }
        break;
      }
    }

    return result;
  }

  /**
   * Reset only internal flag that the choice point has logical alternatives.
   */
  public void resetLogicalAlternativesFlag() {
    this.hasLogicalAlternatives = false;
  }

  public void localCut() {
    this.hasLogicalAlternatives = false;
    this.prevChoicePoint = null;
  }

  public void fullCut() {
    this.hasLogicalAlternatives = false;
    this.rootChoicePoint.cutActivated = true;
    this.rootChoicePoint.clauseIterator = null;
    this.prevChoicePoint = null;
  }

  public void resetPreviousChoicePoint() {
    this.prevChoicePoint = null;
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
    return this.rootChoicePoint.parentChoicePoint == null || !this.hasLogicalAlternatives;
  }

  @Override
  public int compare(Term term1, Term term2) {
    if (term1 == term2) {
      return 0;
    }

    term1 = term1.tryGround();
    term2 = term2.tryGround();

    if (term1 == term2) {
      return 0;
    }

    final int result;
    switch (term1.getTermType()) {
      case ATOM: {
        if (term2 instanceof CompoundTerm) {
          if (term2.isNullList()) {
            result = term1 instanceof NumericTerm ? -1 : 1;
          } else {
            result = -1;
          }
        } else if (term2.getTermType() == ATOM) {
          if (term1 instanceof NumericTerm) {
            if (term2 instanceof NumericTerm) {
              if (term1 instanceof TermDouble || term2 instanceof TermDouble) {
                result =
                    Double.compare(term1.toNumber().doubleValue(), term2.toNumber().doubleValue());
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
          final int arity1 = struct1.isNullList() ? 0 : struct1.getArity();
          final int arity2 = struct2.isNullList() ? 0 : struct2.getArity();

          int compareResult = Integer.compare(arity1, arity2);
          if (compareResult == 0) {
            compareResult =
                struct1.getFunctor().getText().compareTo(struct2.getFunctor().getText());
            if (compareResult == 0) {
              for (int i = 0; i < arity1 && compareResult == 0; i++) {
                compareResult = this.compare(struct1.getArgumentAt(i), struct2.getArgumentAt(i));
              }
            }
          }
          result = compareResult;
        } else {
          if (term1.isNullList()) {
            result = term2 instanceof NumericTerm ? 1 : -1;
          } else {
            result = -1;
          }
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
