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
import com.igormaznitsa.jprol.data.SourcePosition;
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

  private static final int CP_SUCCESS = 0;
  private static final int CP_FAIL = 1;
  private static final int CP_STACK_CHANGED = 2;

  private static final int RS_CONTINUE = 0;
  private static final int RS_SUCCESS = 1;
  private static final int RS_FAIL = 2;

  private final Map<String, TermVar> variables;
  private final VariableStateSnapshot varSnapshot;
  private final JProlContext context;
  private final JProlChoicePoint rootChoicePoint;
  private final Term goalTerm;
  private final boolean verify;
  private final boolean trace;
  private final Object associatedObject;
  private boolean hasAlternatives;
  private Object internalAuxiliaryObject;
  private JProlChoicePoint prevCp;
  private JProlChoicePoint parentCp;
  private JProlChoicePoint nextCp;
  private Term childConnectingTerm;
  private Term thisConnector;
  private Term nextAndTerm;
  private Term nextAndTermForNextGoal;
  private Iterator<TermStruct> clauseIterator;
  private boolean cutActivated;
  private boolean firstResolveCall = true;

  private JProlChoicePoint(
      final JProlContext context,
      final Term goal,
      final Map<String, Term> variableMap,
      final Object associatedObject,
      final JProlChoicePoint rootChoicePoint,
      final boolean trace,
      final boolean verify,
      final Object internalAuxiliaryObject
  ) {
    this.internalAuxiliaryObject = internalAuxiliaryObject;
    this.associatedObject = associatedObject;

    this.hasAlternatives = true;
    this.verify = verify;
    this.trace = trace;

    this.rootChoicePoint = rootChoicePoint == null ? this : rootChoicePoint;
    this.goalTerm = goal.getTermType() == ATOM ? newStruct(goal) : goal;
    this.context = context;

    final Term grounded = goal.tryGround();

    if (this.verify) {
      ProlAssertions.assertCallable(grounded);
    }

    if (rootChoicePoint == null) {
      if (grounded.getTermType() == ATOM) {
        this.varSnapshot = null;
        this.variables = null;
      } else {
        this.variables = grounded.findAllNamedVariables();
        this.varSnapshot = new VariableStateSnapshot(grounded, variableMap);
      }
      this.parentCp = this;
      this.prevCp = null;
    } else {
      this.variables = null;
      if (grounded.getTermType() == ATOM) {
        this.varSnapshot = null;
      } else {
        this.varSnapshot = new VariableStateSnapshot(rootChoicePoint.varSnapshot);
      }
      this.prevCp = rootChoicePoint.parentCp;
      rootChoicePoint.parentCp = this;
    }
  }

  JProlChoicePoint(
      final JProlContext context,
      final Term goal,
      final Map<String, Term> variableMap,
      final Object associatedObject
  ) {
    this(
        context,
        goal,
        variableMap,
        associatedObject,
        null,
        context.isTrace(),
        context.isVerify(),
        null
    );
  }

  private static void traceCp(JProlChoicePoint goalToProcess) {
    if (goalToProcess.trace) {
      final TraceEvent traceEvent;
      if (goalToProcess.firstResolveCall) {
        traceEvent = TraceEvent.CALL;
        goalToProcess.firstResolveCall = false;
      } else {
        traceEvent = TraceEvent.REDO;
      }
      goalToProcess.context.fireTraceEvent(traceEvent, goalToProcess);
    }
  }

  /**
   * Create a child choice point for the same root (no preset vars, no internal object).
   */
  private JProlChoicePoint createChildChoicePoint(final Term goal) {
    return new JProlChoicePoint(
        this.context, goal,
        null,
        this.associatedObject,
        this.rootChoicePoint,
        this.trace,
        this.verify,
        null
    );
  }

  private void fireExitIfTrace(final JProlChoicePoint goal) {
    if (this.trace) {
      this.context.fireTraceEvent(EXIT, goal);
    }
  }

  private void fireFailAndExitIfTrace(final JProlChoicePoint goal) {
    if (this.trace) {
      this.context.fireTraceEvent(TraceEvent.FAIL, goal);
      this.context.fireTraceEvent(EXIT, goal);
    }
  }

  /**
   * Shared cut cleanup: clear alternatives and prev chain.
   */
  private void clearAlternativesAndPrev() {
    this.hasAlternatives = false;
    this.prevCp = null;
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
    this.fireExitIfTrace(this.rootChoicePoint.parentCp);

    final JProlChoicePoint newGoal = this.createChildChoicePoint(goal);
    final JProlChoicePoint prevGoal = newGoal.prevCp;
    if (prevGoal != null) {
      newGoal.prevCp = prevGoal.prevCp;
      newGoal.nextAndTerm = prevGoal.nextAndTerm;
      newGoal.nextAndTermForNextGoal = prevGoal.nextAndTermForNextGoal;
    }
    return newGoal;
  }

  /**
   * Get associated object.
   *
   * @return associated object, can be null
   * @since 3.0.0
   */
  @SuppressWarnings("unchecked")
  public <T> T getAssociatedObject() {
    return (T) this.associatedObject;
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
      throw new ProlHaltExecutionException("Context disposed", 0, SourcePosition.UNKNOWN);
    }
    return this.proveNext((s, t) -> this.context.notifyAboutUndefinedPredicate(this, s, t));
  }

  public Term proveIgnoringUnknownPredicates() {
    return this.proveNext(NULL_UNDEFINED_PREDICATE_CONSUMER);
  }

  private Term proveNext(final BiConsumer<String, Term> unknownPredicateConsumer) {
    final JProlChoicePoint root = this.rootChoicePoint;

    Term result = null;
    boolean mainLoop = true;

    while (mainLoop) {
      if (this.context.isDisposed()) {
        throw new ProlChoicePointInterruptedException("detected context dispose during prove",
            this);
      }

      JProlChoicePoint goalToProcess = root.parentCp;
      if (goalToProcess == null) {
        break;
      }

      if (goalToProcess.hasAlternatives) {
        try {
          traceCp(goalToProcess);
          int cpResult = CP_FAIL;
          boolean internalLoop = true;

          while (internalLoop) {
            if (goalToProcess.context.isDisposed()) {
              throw new ProlChoicePointInterruptedException("context disposed", goalToProcess);
            }

            // reset variables to their initial state
            if (goalToProcess.varSnapshot != null) {
              goalToProcess.varSnapshot.resetToState();
            }

            if (goalToProcess.nextCp != null) {
              // solve sub-goal
              final Term solvedTerm = goalToProcess.nextCp.proveNext(
                  unknownPredicateConsumer);

              if (goalToProcess.nextCp.cutActivated) {
                goalToProcess.clauseIterator = null;
              }

              if (solvedTerm == null) {
                goalToProcess.nextCp = null;
                if (goalToProcess.clauseIterator == null) {
                  break;
                }
              } else {
                if (!goalToProcess.thisConnector.unifyWith(goalToProcess.childConnectingTerm)) {
                  throw new ProlCriticalError("Critical error #980234");
                }
                cpResult = CP_SUCCESS;
                break;
              }
            }

            final Term theTerm = goalToProcess.goalTerm.tryGroundOrDefault(goalToProcess.goalTerm);

            if (goalToProcess.clauseIterator != null) {
              final int stepResult = goalToProcess.processNextClause(theTerm);
              if (stepResult == RS_SUCCESS) {
                cpResult = CP_SUCCESS;
                break;
              }
              if (stepResult == RS_FAIL) {
                break;
              }
              continue;
            }

            switch (theTerm.getTermType()) {
              case ATOM: {
                cpResult = goalToProcess.resolveAtom(theTerm);
                internalLoop = false;
              }
              break;
              case STRUCT: {
                final TermStruct struct = (TermStruct) theTerm;
                final int arity = struct.getArity();
                final Term functor = struct.getFunctor();

                if (struct.isClause()) {
                  final TermStruct structClone = (TermStruct) struct.makeClone();
                  final Term head = structClone.getArgumentAt(0);

                  goalToProcess.thisConnector = struct.getArgumentAt(0);
                  goalToProcess.childConnectingTerm = head;

                  goalToProcess.nextCp = arity == 1
                      ? goalToProcess.context.makeChoicePoint(head, goalToProcess.associatedObject)
                      : goalToProcess.context.makeChoicePoint(structClone.getArgumentAt(1),
                      goalToProcess.associatedObject);
                } else {
                  final String functorText = functor.getText();
                  boolean nonConsumed = true;

                  if (arity < 3) {
                    final int functorTextLength = functorText.length();
                    switch (functorText.charAt(0)) {
                      case '!': {
                        if (arity == 0) {
                          switch (functorTextLength) {
                            case 1: { // standard cut !/0
                              goalToProcess.fullCut();
                              nonConsumed = false;
                              internalLoop = false;
                              cpResult = CP_SUCCESS;
                            }
                            break;
                            case 2: { // local cut !!/0
                              if (functorText.charAt(1) == '!') {
                                goalToProcess.localCut();
                                nonConsumed = false;
                                internalLoop = false;
                                cpResult = CP_SUCCESS;
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
                              goalToProcess.replaceLastGoalAtChain(struct.getArgumentAt(0));
                          leftSubGoal.nextAndTerm = struct.getArgumentAt(1);
                          leftSubGoal.nextAndTermForNextGoal = goalToProcess.nextAndTerm;

                          cpResult = CP_STACK_CHANGED;

                          internalLoop = false;
                          nonConsumed = false;
                        }
                      }
                      break;
                      case ';': { // or  ;/2
                        if (arity == 2 && functorTextLength == 1) {
                          if (goalToProcess.getInternalObject() == null) {
                            final JProlChoicePoint leftSubbranch =
                                goalToProcess.createChildChoicePoint(struct.getArgumentAt(0));
                            leftSubbranch.nextAndTerm = goalToProcess.nextAndTerm;
                            goalToProcess.setInternalObject(leftSubbranch);
                          } else {
                            goalToProcess.replaceLastGoalAtChain(struct.getArgumentAt(1));
                          }
                          cpResult = CP_STACK_CHANGED;
                          nonConsumed = false;
                          internalLoop = false;
                        }
                      }
                      break;
                    }
                  }

                  if (nonConsumed) {
                    final PredicateInvoker foundProcessor =
                        goalToProcess.findProcessorInLibraries(struct);
                    if (foundProcessor == PredicateInvoker.NULL_INVOKER
                        || foundProcessor.isEvaluable()) {
                      goalToProcess.clauseIterator =
                          goalToProcess.context.getKnowledgeBase().iterate(
                              IteratorType.ANY,
                              struct,
                              functor.getTermType() == OPERATOR
                                  ? NULL_UNDEFINED_PREDICATE_CONSUMER
                                  : unknownPredicateConsumer
                          );
                      if (!goalToProcess.clauseIterator.hasNext()) {
                        internalLoop = false;
                        goalToProcess.resetLogicalAlternativesFlag();
                      }
                    } else {
                      if (foundProcessor.isDetermined()) {
                        goalToProcess.resetLogicalAlternativesFlag();
                      }

                      if (foundProcessor.execute(goalToProcess, struct)) {
                        cpResult = CP_SUCCESS;
                      }

                      if (cpResult == CP_SUCCESS
                          && foundProcessor.doesChangeGoalChain()) {
                        cpResult = CP_STACK_CHANGED;
                      }

                      internalLoop = false;
                    }
                  }
                }
              }
              break;
              default: {
                internalLoop = false;
                goalToProcess.resetLogicalAlternativesFlag();
              }
              break;
            }
          }

          switch (cpResult) {
            case CP_FAIL: {
              this.fireFailAndExitIfTrace(goalToProcess);
              root.parentCp = goalToProcess.prevCp;
            }
            break;
            case CP_SUCCESS: {
              goalToProcess = root.parentCp;

              if (goalToProcess.nextAndTerm == null) {
                result = root.goalTerm;
                mainLoop = false;
              } else {
                final JProlChoicePoint nextGoal =
                    this.createChildChoicePoint(goalToProcess.nextAndTerm);
                nextGoal.nextAndTerm = goalToProcess.nextAndTermForNextGoal;
              }
            }
            break;
            case CP_STACK_CHANGED: {
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
        this.fireExitIfTrace(goalToProcess);
        root.parentCp = goalToProcess.prevCp;
      }
    }

    return result;
  }

  /**
   * Reset only internal flag that the choice point has logical alternatives.
   */
  public void resetLogicalAlternativesFlag() {
    this.hasAlternatives = false;
  }

  public void localCut() {
    this.clearAlternativesAndPrev();
  }

  public void fullCut() {
    this.clearAlternativesAndPrev();
    this.rootChoicePoint.cutActivated = true;
    this.rootChoicePoint.clauseIterator = null;
  }

  public void resetPreviousChoicePoint() {
    this.prevCp = null;
  }

  private int resolveAtom(final Term theTerm) {
    final String text = theTerm.getText();

    if (this.context.hasZeroArityPredicateForName(text)) {
      this.resetLogicalAlternativesFlag();
      return CP_SUCCESS;
    }

    this.context.notifyAboutUndefinedPredicate(this, theTerm.getSignature(), theTerm);
    this.resetLogicalAlternativesFlag();

    return CP_FAIL;
  }

  /**
   * Try to apply the next clause from the iterator. Sets child choice point on clause match.
   *
   * @return CONTINUE to loop (sub-goal set), SUCCESS when fact matched, FAIL when no more clauses
   */
  private int processNextClause(final Term theTerm) {
    if (!this.clauseIterator.hasNext()) {
      this.clauseIterator = null;
      this.resetLogicalAlternativesFlag();
      return RS_FAIL;
    }

    final TermStruct nextClause = this.clauseIterator.next();
    final TermStruct goalStruct = (TermStruct) theTerm;
    final Term goalTermForEqu =
        goalStruct.isClause() ? goalStruct.getArgumentAt(0).makeClone() : theTerm.makeClone();

    if (!goalTermForEqu
        .unifyWith(nextClause.isClause() ? nextClause.getArgumentAt(0) : nextClause)) {
      throw new ProlCriticalError(
          String.format(
              "Unexpectedly can't unify term with provided by knowledge base: arg1 = %s <-> arg2 = %s",
              goalTermForEqu.toSrcString(),
              nextClause.toSrcString()));
    }

    if (nextClause.isClause()) {
      this.thisConnector = theTerm;
      this.childConnectingTerm = nextClause.getArgumentAt(0);
      this.nextCp =
          this.context.makeChoicePoint(nextClause.getArgumentAt(1), this.associatedObject);
      return RS_CONTINUE;
    }

    if (!theTerm.unifyWith(nextClause)) {
      throw new ProlCriticalError("Impossible situation #0009824 (unifyWith): "
          + theTerm.toSrcString() + " <> " + nextClause.toSrcString());
    }

    return RS_SUCCESS;
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
    return this.rootChoicePoint.parentCp == null || !this.hasAlternatives;
  }

  @Override
  public int compare(final Term term1, final Term term2) {
    if (term1 == term2) {
      return 0;
    }

    final Term g1 = term1.tryGround();
    final Term g2 = term2.tryGround();

    if (g1 == g2) {
      return 0;
    }

    final int result;
    switch (g1.getTermType()) {
      case ATOM: {
        if (g2 instanceof CompoundTerm) {
          if (g2.isNullList()) {
            result = g1 instanceof NumericTerm ? -1 : 1;
          } else {
            result = -1;
          }
        } else if (g2.getTermType() == ATOM) {
          if (g1 instanceof NumericTerm) {
            if (g2 instanceof NumericTerm) {
              if (g1 instanceof TermDouble || g2 instanceof TermDouble) {
                result =
                    Double.compare(g1.toNumber().doubleValue(), g2.toNumber().doubleValue());
              } else {
                result = Long.compare(g1.toNumber().longValue(), g2.toNumber().longValue());
              }
            } else {
              result = -1;
            }
          } else {
            result = g2 instanceof NumericTerm ? 1 : g1.getText().compareTo(g2.getText());
          }
        } else {
          result = 1;
        }
      }
      break;
      case LIST:
      case STRUCT: {
        if (g2 instanceof CompoundTerm) {
          result = this.compareStructs((TermStruct) g1, (TermStruct) g2);
        } else {
          if (g1.isNullList()) {
            result = g2 instanceof NumericTerm ? 1 : -1;
          } else {
            result = -1;
          }
        }
      }
      break;
      case VAR: {
        if (g2.getTermType() == VAR) {
          result = g1.getText().compareTo(g2.getText());
        } else {
          result = -1;
        }
      }
      break;
      default: {
        result = 1;
      }
      break;
    }
    return result;
  }

  private int compareStructs(final TermStruct struct1, final TermStruct struct2) {
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

    return compareResult;
  }

}
