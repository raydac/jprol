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
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.trace.TraceEvent.EXIT;
import static java.lang.System.identityHashCode;
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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
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
  private static final int CP_PUSH_SUBGOAL = 3;
  private static final int CP_TAIL_CALL = 4;
  private static final int NOT_CONTROL = -1;

  private static final int RS_PUSH_SUBGOAL = 0;
  private static final int RS_SUCCESS = 1;
  private static final int RS_FAIL = 2;
  private static final int RS_TAIL_CALL = 3;

  private final Term rootGroundedGoal;
  private final JProlContext context;
  private final JProlChoicePoint rootChoicePoint;
  private final Term goalTerm;
  private final boolean verify;
  private final boolean trace;
  private final Object associatedObject;

  private VariableStateSnapshot varSnapshot;
  private boolean snapshotInitialized;
  private Map<String, TermVar> variables;
  private ProveStack reusableProveStack;
  private BiConsumer<String, Term> notifyingUndefinedConsumer;
  private boolean proveStackInUse;
  private boolean hasAlternatives;
  private boolean needsSnapshotReset;
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
  private boolean propagatesCutToCaller = true;
  private boolean firstResolveCall = true;

  private JProlChoicePoint(
      final JProlContext context,
      final Term goal,
      final Map<String, Term> variableMap,
      final Object associatedObject,
      final JProlChoicePoint rootChoicePoint,
      final boolean trace,
      final boolean verify,
      final Object internalAuxiliaryObject,
      final boolean internalSubGoal
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
    if (this.verify && grounded.getTermType() != STRUCT) {
      ProlAssertions.assertCallable(grounded);
    }

    if (rootChoicePoint == null) {
      this.rootGroundedGoal = grounded.getTermType() == ATOM ? null : grounded;
      if (internalSubGoal) {
        this.varSnapshot = null;
        this.snapshotInitialized = grounded.getTermType() == ATOM;
        this.needsSnapshotReset = false;
      } else {
        this.varSnapshot = grounded.getTermType() == ATOM
            ? null
            : VariableStateSnapshot.ofRoot(grounded, variableMap);
        this.snapshotInitialized = true;
        this.needsSnapshotReset = this.varSnapshot != null;
      }
      this.parentCp = this;
      this.prevCp = null;
    } else {
      this.rootGroundedGoal = null;
      this.varSnapshot = null;
      this.snapshotInitialized = grounded.getTermType() == ATOM;
      this.needsSnapshotReset = false;
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
        null,
        false
    );
  }

  private static void traceCp(final JProlChoicePoint goalToProcess) {
    if (goalToProcess.trace) {
      final TraceEvent traceEvent =
          goalToProcess.firstResolveCall ? TraceEvent.CALL : TraceEvent.REDO;
      if (goalToProcess.firstResolveCall) {
        goalToProcess.firstResolveCall = false;
      }
      goalToProcess.context.fireTraceEvent(traceEvent, goalToProcess);
    }
  }

  private static boolean isConjunctionStruct(final Term term) {
    if (term.getTermType() != STRUCT) {
      return false;
    }
    final TermStruct struct = (TermStruct) term;
    return struct.getArity() == 2 && ",".equals(struct.getFunctor().getText());
  }

  private static void replaceTopFrameForTailCall(
      final ProveStack proveStack,
      final JProlChoicePoint activeSelf,
      final JProlChoicePoint replacement
  ) {
    final JProlChoicePoint parentGoal = proveStack.frameAt(proveStack.size() - 2).pendingResumeGoal;
    if (activeSelf.cutActivated && activeSelf.propagatesCutToCaller) {
      parentGoal.clauseIterator = null;
    }
    replacement.propagatesCutToCaller = false;
    parentGoal.nextCp = replacement;
    proveStack.replaceTopChoicePoint(replacement);
  }

  private static void propagateCutAndTryCollapseParent(
      final ProveStack proveStack,
      final JProlChoicePoint activeSelf,
      final JProlChoicePoint root,
      final int cpResult
  ) {
    final int stackSize = proveStack.size();
    if (stackSize < 2) {
      return;
    }
    final ProveStackFrame parentFrame = proveStack.frameAt(stackSize - 2);
    final JProlChoicePoint parentGoal = parentFrame.pendingResumeGoal;
    if (parentGoal == null) {
      return;
    }

    parentGoal.clauseIterator = null;
    activeSelf.propagatesCutToCaller = false;

    if (cpResult == CP_FAIL || stackSize < 3 || activeSelf != root || activeSelf.trace) {
      return;
    }

    final JProlChoicePoint parentSelf = parentFrame.choicePoint;
    final JProlChoicePoint parentRoot = parentSelf.rootChoicePoint;
    if (parentSelf != parentRoot
        || parentSelf.trace
        || parentGoal != parentRoot.parentCp
        || parentGoal.nextAndTerm != null
        || parentGoal.nextAndTermForNextGoal != null
        || !parentGoal.hasNoPriorAlternatives()) {
      return;
    }

    final JProlChoicePoint grandParentGoal = proveStack.frameAt(stackSize - 3).pendingResumeGoal;
    if (parentSelf.cutActivated && parentSelf.propagatesCutToCaller) {
      grandParentGoal.clauseIterator = null;
    }
    if (parentGoal.thisConnector != null
        && !parentGoal.childConnectingTerm.unifyWith(parentGoal.thisConnector)) {
      throw new ProlCriticalError("Critical error #980235");
    }
    grandParentGoal.nextCp = activeSelf;
    proveStack.collapseParentWithTop(activeSelf);
  }

  private JProlChoicePoint createChildChoicePoint(final Term goal) {
    return new JProlChoicePoint(
        this.context,
        goal,
        null,
        this.associatedObject,
        this.rootChoicePoint,
        this.trace,
        this.verify,
        null,
        true
    );
  }

  private JProlChoicePoint makeSubGoalChoicePoint(final Term subGoal) {
    return new JProlChoicePoint(
        this.context,
        subGoal,
        null,
        this.associatedObject,
        null,
        this.trace,
        this.verify,
        null,
        true
    );
  }

  private ProveStack newProveStack() {
    return new ProveStack(this.context.getMaxProveStackDepth());
  }

  private void ensureVarSnapshot() {
    if (this.snapshotInitialized) {
      return;
    }
    this.snapshotInitialized = true;
    JProlChoicePoint ancestor = this.prevCp;
    while (ancestor != null && !ancestor.snapshotInitialized) {
      ancestor = ancestor.prevCp;
    }
    if (ancestor != null) {
      this.varSnapshot = ancestor.varSnapshot == null
          ? null
          : ancestor.varSnapshot.snapshotCurrentState();
      return;
    }
    if (this != this.rootChoicePoint && this.rootChoicePoint.snapshotInitialized) {
      this.varSnapshot = this.rootChoicePoint.varSnapshot == null
          ? null
          : this.rootChoicePoint.varSnapshot.snapshotCurrentState();
      return;
    }
    this.varSnapshot = VariableStateSnapshot.ofSubgoalTerms(
        this.rootChoicePoint.rootGroundedGoal,
        this.goalTerm,
        this.nextAndTerm);
  }

  private boolean hasNoPriorAlternatives() {
    JProlChoicePoint cursor = this.prevCp;
    while (cursor != null) {
      if (cursor.hasAlternatives) {
        return false;
      }
      cursor = cursor.prevCp;
    }
    this.prevCp = null;
    return true;
  }

  private boolean isInTailPosition(
      final ProveStack proveStack,
      final JProlChoicePoint activeSelf,
      final JProlChoicePoint root
  ) {
    return proveStack.size() >= 2
        && activeSelf == root
        && !activeSelf.trace
        && this == root.parentCp
        && this.nextAndTerm == null
        && this.nextAndTermForNextGoal == null
        && this.hasNoPriorAlternatives();
  }

  private Map<String, TermVar> resolveVariablesMap() {
    Map<String, TermVar> resolved = this.variables;
    if (resolved == null) {
      resolved = this.rootGroundedGoal == null
          ? Collections.emptyMap()
          : this.rootGroundedGoal.findAllNamedVariables();
      this.variables = resolved;
    }
    return resolved;
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
    final Map<String, TermVar> namedVars = this.resolveVariablesMap();
    return namedVars.isEmpty()
        ? new HashMap<>()
        : namedVars.entrySet()
        .stream()
        .filter(v -> v.getValue().isGround())
        .collect(toMap(Map.Entry::getKey, e -> e.getValue().makeClone()));
  }

  @Override
  public String toString() {
    return (this.isCompleted() ? "Completed " : "Active ") + "Goal(" + this.goalTerm.toString()
        + ')';
  }

  public Optional<TermVar> findVar(final String name) {
    requireNonNull(name);
    final Map<String, TermVar> namedVars = this.resolveVariablesMap();
    return namedVars.isEmpty() ? Optional.empty() : Optional.ofNullable(namedVars.get(name));
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
   * @param <T> expected type of the associated object
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
   * @param <T> expected type of the internal value
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
    if (this.notifyingUndefinedConsumer == null) {
      this.notifyingUndefinedConsumer =
          (s, t) -> this.context.notifyAboutUndefinedPredicate(this, s, t);
    }
    return this.proveWithReusableStack(this.notifyingUndefinedConsumer);
  }

  public Term proveIgnoringUnknownPredicates() {
    return this.proveWithReusableStack(NULL_UNDEFINED_PREDICATE_CONSUMER);
  }

  private Term proveWithReusableStack(final BiConsumer<String, Term> unknownPredicateConsumer) {
    final ProveStack stack;
    final boolean borrowedReusable;
    if (!this.proveStackInUse) {
      if (this.reusableProveStack == null) {
        this.reusableProveStack = this.newProveStack();
      }
      this.proveStackInUse = true;
      borrowedReusable = true;
      stack = this.reusableProveStack;
    } else {
      borrowedReusable = false;
      stack = this.newProveStack();
    }

    try {
      return this.proveNext(unknownPredicateConsumer, stack);
    } finally {
      stack.clear();
      if (borrowedReusable) {
        this.proveStackInUse = false;
      }
    }
  }

  private Term proveNext(
      final BiConsumer<String, Term> unknownPredicateConsumer,
      final ProveStack proveStack
  ) {
    proveStack.push(this);

    stackLoop:
    while (!proveStack.isEmpty()) {
      Term frameResult = null;

      mainWhile:
      while (true) {
        final ProveStackFrame topFrame = proveStack.peek();
        final JProlChoicePoint activeSelf = topFrame.choicePoint;
        final JProlChoicePoint root = activeSelf.rootChoicePoint;
        final JProlContext ctx = activeSelf.context;

        if (ctx.isDisposed()) {
          throw new ProlChoicePointInterruptedException("detected context dispose during prove",
              activeSelf);
        }

        JProlChoicePoint goalToProcess;
        final boolean resumeAfterSubgoal = topFrame.subgoalOutcomePending;
        if (resumeAfterSubgoal) {
          goalToProcess = topFrame.pendingResumeGoal;
        } else {
          goalToProcess = root.parentCp;
          if (goalToProcess == null) {
            break mainWhile;
          }
          if (!goalToProcess.hasAlternatives) {
            activeSelf.fireExitIfTrace(goalToProcess);
            root.parentCp = goalToProcess.prevCp;
            continue mainWhile;
          }
        }

        try {
          if (!resumeAfterSubgoal) {
            traceCp(goalToProcess);
          }
          int cpResult = CP_FAIL;
          boolean internalLoop = true;

          while (internalLoop) {
            if (ctx.isDisposed()) {
              throw new ProlChoicePointInterruptedException("context disposed", goalToProcess);
            }

            if (goalToProcess.needsSnapshotReset) {
              if (goalToProcess.varSnapshot != null) {
                goalToProcess.varSnapshot.resetToState();
              }
            } else {
              goalToProcess.needsSnapshotReset = true;
            }

            final ProveStackFrame innerTop = proveStack.peek();
            if (innerTop.subgoalOutcomePending) {
              goalToProcess = innerTop.pendingResumeGoal;
              final Term solvedTerm = innerTop.pendingSubgoalResult;
              innerTop.clearOutcome();

              final JProlChoicePoint finishedChild = goalToProcess.nextCp;
              if (finishedChild.cutActivated && finishedChild.propagatesCutToCaller) {
                goalToProcess.clauseIterator = null;
              }

              if (solvedTerm == null) {
                goalToProcess.nextCp = null;
                if (goalToProcess.clauseIterator == null) {
                  goalToProcess.resetLogicalAlternativesFlag();
                  break;
                }
                continue;
              }

              final boolean childCompleted = finishedChild.isCompleted();
              if (!childCompleted || goalToProcess.clauseIterator != null) {
                goalToProcess.ensureVarSnapshot();
              }
              if (goalToProcess.thisConnector != null
                  && !goalToProcess.thisConnector.unifyWith(goalToProcess.childConnectingTerm)) {
                throw new ProlCriticalError("Critical error #980234");
              }
              if (childCompleted) {
                goalToProcess.nextCp = null;
                if (goalToProcess.clauseIterator == null) {
                  goalToProcess.resetLogicalAlternativesFlag();
                }
              }
              cpResult = CP_SUCCESS;
              break;
            }

            if (goalToProcess.nextCp != null) {
              innerTop.watchChild(goalToProcess);
              proveStack.push(goalToProcess.nextCp);
              continue stackLoop;
            }

            final Term theTerm =
                goalToProcess.goalTerm.tryGroundOrDefault(goalToProcess.goalTerm);

            if (goalToProcess.clauseIterator != null) {
              final int stepResult = goalToProcess.processNextClause(
                  theTerm,
                  goalToProcess.isInTailPosition(proveStack, activeSelf, root));
              if (stepResult == RS_TAIL_CALL) {
                replaceTopFrameForTailCall(proveStack, activeSelf, goalToProcess.nextCp);
                continue stackLoop;
              }
              if (stepResult == RS_PUSH_SUBGOAL) {
                innerTop.watchChild(goalToProcess);
                proveStack.push(goalToProcess.nextCp);
                continue stackLoop;
              }
              if (stepResult == RS_SUCCESS) {
                cpResult = CP_SUCCESS;
              }
              break;
            }

            switch (theTerm.getTermType()) {
              case ATOM: {
                cpResult = goalToProcess.resolveAtom(theTerm);
                internalLoop = false;
              }
              break;
              case STRUCT: {
                final TermStruct struct = (TermStruct) theTerm;
                if (struct.isClause()) {
                  goalToProcess.enqueueClauseSubgoal(struct);
                  innerTop.watchChild(goalToProcess);
                  proveStack.push(goalToProcess.nextCp);
                  continue stackLoop;
                }
                final int solved = goalToProcess.solveNonClauseStruct(
                    struct,
                    unknownPredicateConsumer,
                    proveStack,
                    activeSelf,
                    root);
                if (solved == CP_TAIL_CALL) {
                  replaceTopFrameForTailCall(proveStack, activeSelf, goalToProcess.nextCp);
                  continue stackLoop;
                }
                if (solved == CP_PUSH_SUBGOAL) {
                  innerTop.watchChild(goalToProcess);
                  proveStack.push(goalToProcess.nextCp);
                  continue stackLoop;
                }
                cpResult = solved;
                internalLoop = false;
              }
              break;
              default: {
                internalLoop = false;
                goalToProcess.resetLogicalAlternativesFlag();
              }
              break;
            }
          }

          if (activeSelf.cutActivated && activeSelf.propagatesCutToCaller) {
            propagateCutAndTryCollapseParent(proveStack, activeSelf, root, cpResult);
          }

          if (!this.applyCpResult(cpResult, activeSelf, root, goalToProcess)) {
            frameResult = root.goalTerm;
            break mainWhile;
          }
        } catch (StackOverflowError ex) {
          throw new ProlChoicePointStackOverflowException(
              "Caught stack overflow error during prove", activeSelf);
        }
      }

      proveStack.drop();
      if (proveStack.isEmpty()) {
        return frameResult;
      }
      proveStack.peek().acceptSubgoal(frameResult);
    }

    return null;
  }

  private boolean applyCpResult(
      final int cpResult,
      final JProlChoicePoint activeSelf,
      final JProlChoicePoint root,
      final JProlChoicePoint goalToProcess
  ) {
    switch (cpResult) {
      case CP_FAIL:
        activeSelf.fireFailAndExitIfTrace(goalToProcess);
        root.parentCp = goalToProcess.prevCp;
        if (root.parentCp == null && root.varSnapshot != null) {
          root.varSnapshot.resetToState();
        }
        return true;
      case CP_SUCCESS: {
        final JProlChoicePoint current = root.parentCp;
        final Term pendingAnd = current.nextAndTerm;
        if (pendingAnd == null) {
          if (!current.hasAlternatives && !activeSelf.trace) {
            root.parentCp = current.prevCp;
          }
          return false;
        }
        this.advanceToNextConjunct(activeSelf, current, pendingAnd);
        return true;
      }
      case CP_STACK_CHANGED:
        return true;
      default:
        throw new AssertionError("Unexpected cpResult: " + cpResult);
    }
  }

  private void advanceToNextConjunct(
      final JProlChoicePoint activeSelf,
      final JProlChoicePoint current,
      final Term pendingAnd
  ) {
    if (!activeSelf.trace && current.nextAndTermForNextGoal == null) {
      final Term groundedNext = pendingAnd.tryGround();
      if (isConjunctionStruct(groundedNext)) {
        final TermStruct conj = (TermStruct) groundedNext;
        final JProlChoicePoint nextGoal =
            activeSelf.createChildChoicePoint(conj.getArgumentAt(0));
        nextGoal.nextAndTerm = conj.getArgumentAt(1);
        if (!current.hasAlternatives) {
          nextGoal.prevCp = current.prevCp;
        }
        return;
      }
    }

    final JProlChoicePoint nextGoal = activeSelf.createChildChoicePoint(pendingAnd);
    nextGoal.nextAndTerm = current.nextAndTermForNextGoal;
    if (!current.hasAlternatives && !activeSelf.trace) {
      nextGoal.prevCp = current.prevCp;
    }
  }

  private void enqueueClauseSubgoal(final TermStruct struct) {
    final TermStruct structClone = (TermStruct) struct.makeClone();
    final Term head = structClone.getArgumentAt(0);
    final int arity = struct.getArity();

    this.thisConnector = struct.getArgumentAt(0);
    this.childConnectingTerm = head;
    this.nextCp = arity == 1
        ? this.makeSubGoalChoicePoint(head)
        : this.makeSubGoalChoicePoint(structClone.getArgumentAt(1));
  }

  private int solveNonClauseStruct(
      final TermStruct struct,
      final BiConsumer<String, Term> unknownPredicateConsumer,
      final ProveStack proveStack,
      final JProlChoicePoint activeSelf,
      final JProlChoicePoint root
  ) {
    final int control = this.tryDispatchBuiltInControl(struct);
    if (control != NOT_CONTROL) {
      return control;
    }
    final Term functor = struct.getFunctor();
    final PredicateInvoker foundProcessor = this.findProcessorInLibraries(struct);
    if (foundProcessor == PredicateInvoker.NULL_INVOKER || foundProcessor.isEvaluable()) {
      this.clauseIterator = this.context.getKnowledgeBase().iterate(
          IteratorType.ANY,
          struct,
          functor.getTermType() == OPERATOR
              ? NULL_UNDEFINED_PREDICATE_CONSUMER
              : unknownPredicateConsumer
      );
      if (!this.clauseIterator.hasNext()) {
        this.clauseIterator = null;
        this.resetLogicalAlternativesFlag();
        return CP_FAIL;
      }
      final int stepResult = this.processNextClause(
          struct,
          this.isInTailPosition(proveStack, activeSelf, root));
      if (stepResult == RS_TAIL_CALL) {
        return CP_TAIL_CALL;
      }
      if (stepResult == RS_PUSH_SUBGOAL) {
        return CP_PUSH_SUBGOAL;
      }
      return stepResult == RS_SUCCESS ? CP_SUCCESS : CP_FAIL;
    }
    if (foundProcessor.isDetermined()) {
      this.resetLogicalAlternativesFlag();
    } else if (!foundProcessor.doesChangeGoalChain()) {
      this.ensureVarSnapshot();
    }
    if (foundProcessor.execute(this, struct)) {
      return foundProcessor.doesChangeGoalChain() ? CP_STACK_CHANGED : CP_SUCCESS;
    }
    return CP_FAIL;
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

  public void discardBindings() {
    if (this.varSnapshot != null) {
      this.varSnapshot.resetToState();
    }
  }

  public void retainChoicesOutsideIfThen(final JProlChoicePoint thenGoal) {
    final JProlChoicePoint disjunction = thenGoal.prevCp;
    if (disjunction != null && disjunction.internalAuxiliaryObject == this) {
      thenGoal.prevCp = disjunction.prevCp;
      disjunction.hasAlternatives = false;
    }
  }

  private int tryDispatchBuiltInControl(final TermStruct struct) {
    final int arity = struct.getArity();
    if (arity >= 3) {
      return NOT_CONTROL;
    }
    final String name = struct.getFunctor().getText();
    if (arity == 0) {
      if ("!".equals(name)) {
        this.fullCut();
        return CP_SUCCESS;
      }
      if ("!!".equals(name)) {
        this.localCut();
        return CP_SUCCESS;
      }
      return NOT_CONTROL;
    }
    if (arity != 2) {
      return NOT_CONTROL;
    }
    if (",".equals(name)) {
      return this.scheduleConjunction(struct);
    }
    if (";".equals(name)) {
      return this.scheduleDisjunction(struct);
    }
    return NOT_CONTROL;
  }

  private int scheduleConjunction(final TermStruct struct) {
    final Term existingAnd = this.nextAndTerm;
    final Term existingAfterAnd = this.nextAndTermForNextGoal;
    final JProlChoicePoint leftSubGoal = this.replaceLastGoalAtChain(struct.getArgumentAt(0));
    leftSubGoal.nextAndTerm = struct.getArgumentAt(1);
    leftSubGoal.nextAndTermForNextGoal = existingAnd == null
        ? null
        : (existingAfterAnd == null
        ? existingAnd
        : newStruct(struct.getFunctor(), existingAnd, existingAfterAnd));
    return CP_STACK_CHANGED;
  }

  private int scheduleDisjunction(final TermStruct struct) {
    if (this.getInternalObject() == null) {
      this.ensureVarSnapshot();
      final JProlChoicePoint leftSubbranch = this.createChildChoicePoint(struct.getArgumentAt(0));
      leftSubbranch.nextAndTerm = this.nextAndTerm;
      this.setInternalObject(leftSubbranch);
    } else {
      this.replaceLastGoalAtChain(struct.getArgumentAt(1));
    }
    return CP_STACK_CHANGED;
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

  private int processNextClause(final Term theTerm, final boolean tailPosition) {
    while (this.clauseIterator.hasNext()) {
      final TermStruct nextClause = this.clauseIterator.next();
      if (!nextClause.isClause()) {
        final boolean moreClauses = this.clauseIterator.hasNext();
        if (moreClauses) {
          this.ensureVarSnapshot();
        }
        if (theTerm.unifyWith(nextClause)) {
          if (!moreClauses) {
            this.clauseIterator = null;
            this.resetLogicalAlternativesFlag();
          }
          return RS_SUCCESS;
        }
        if (!moreClauses) {
          this.clauseIterator = null;
          this.resetLogicalAlternativesFlag();
          return RS_FAIL;
        }
        this.discardBindings();
        continue;
      }

      final boolean isClauseQuery = ((TermStruct) theTerm).isClause();
      final Term goalStructTarget = isClauseQuery
          ? ((TermStruct) theTerm).getArgumentAt(0)
          : theTerm;
      final Term clauseHead = nextClause.getArgumentAt(0);

      if (tailPosition && !isClauseQuery && !this.clauseIterator.hasNext()) {
        this.clauseIterator = null;
        this.resetLogicalAlternativesFlag();
        if (!clauseHead.unifyWith(goalStructTarget)) {
          return RS_FAIL;
        }
        this.thisConnector = null;
        this.childConnectingTerm = null;
        this.nextCp = this.makeSubGoalChoicePoint(nextClause.getArgumentAt(1));
        return RS_TAIL_CALL;
      }

      final Term goalTermForEqu = GoalCloner.cloneUnboundVariables(goalStructTarget);
      if (!goalTermForEqu.unifyWith(clauseHead)) {
        continue;
      }

      if (goalTermForEqu == goalStructTarget) {
        this.thisConnector = null;
        this.childConnectingTerm = null;
      } else {
        this.thisConnector = goalStructTarget;
        this.childConnectingTerm = clauseHead;
      }

      if (this.clauseIterator.hasNext()) {
        this.ensureVarSnapshot();
      } else {
        this.clauseIterator = null;
      }
      this.nextCp = this.makeSubGoalChoicePoint(nextClause.getArgumentAt(1));
      return RS_PUSH_SUBGOAL;
    }

    this.clauseIterator = null;
    this.resetLogicalAlternativesFlag();
    return RS_FAIL;
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
    return this.compareGroundedTerms(g1, g2);
  }

  private int compareGroundedTerms(final Term g1, final Term g2) {
    switch (g1.getTermType()) {
      case ATOM:
        return this.compareGroundedAtomTo(g1, g2);
      case LIST:
      case STRUCT:
        return this.compareGroundedCompoundTo(g1, g2);
      case VAR: {
        if (g2.getTermType() != VAR) {
          return -1;
        }

        final TermVar canonicalLeft = ((TermVar) g1).findUngroundVariable();
        final TermVar canonicalRight = ((TermVar) g2).findUngroundVariable();
        if (canonicalLeft != null && canonicalLeft == canonicalRight) {
          return 0;
        }
        final Term orderedLeft = canonicalLeft == null ? g1 : canonicalLeft;
        final Term orderedRight = canonicalRight == null ? g2 : canonicalRight;
        final int byName = orderedLeft.getText().compareTo(orderedRight.getText());
        if (byName != 0) {
          return byName;
        }
        if (!orderedLeft.isAnonymous() && !orderedRight.isAnonymous()) {
          return 0;
        }

        return Integer.compare(identityHashCode(orderedLeft), identityHashCode(orderedRight));
      }
      default:
        return 1;
    }
  }

  private int compareGroundedAtomTo(final Term g1, final Term g2) {
    if (g2 instanceof CompoundTerm) {
      if (g2.isNullList()) {
        return g1 instanceof NumericTerm ? -1 : 1;
      }
      return -1;
    }
    if (g2.getTermType() == ATOM) {
      return this.compareTwoAtoms(g1, g2);
    }
    return 1;
  }

  private int compareTwoAtoms(final Term g1, final Term g2) {
    if (g1 instanceof NumericTerm) {
      if (g2 instanceof NumericTerm) {
        return this.compareTwoNumerics((NumericTerm) g1, (NumericTerm) g2);
      }
      return -1;
    }
    return g2 instanceof NumericTerm ? 1 : g1.getText().compareTo(g2.getText());
  }

  private int compareTwoNumerics(final NumericTerm n1, final NumericTerm n2) {
    if (n1 instanceof TermDouble || n2 instanceof TermDouble) {
      return Double.compare(n1.toNumber().doubleValue(), n2.toNumber().doubleValue());
    }
    return Long.compare(n1.toNumber().longValue(), n2.toNumber().longValue());
  }

  private int compareGroundedCompoundTo(final Term g1, final Term g2) {
    if (g2 instanceof CompoundTerm) {
      return this.compareStructs((TermStruct) g1, (TermStruct) g2);
    }
    if (g1.isNullList()) {
      return g2 instanceof NumericTerm || g2.getTermType() == VAR ? 1 : -1;
    }
    return 1;
  }

  private int compareStructs(final TermStruct first, final TermStruct second) {
    TermStruct struct1 = first;
    TermStruct struct2 = second;

    while (true) {
      final int arity1 = struct1.isNullList() ? 0 : struct1.getArity();
      final int arity2 = struct2.isNullList() ? 0 : struct2.getArity();

      int compareResult = Integer.compare(arity1, arity2);
      if (compareResult != 0) {
        return compareResult;
      }

      compareResult = struct1.getFunctor().getText().compareTo(struct2.getFunctor().getText());
      if (compareResult != 0 || arity1 == 0) {
        return compareResult;
      }

      final int lastIndex = arity1 - 1;
      for (int i = 0; i < lastIndex; i++) {
        compareResult = this.compare(struct1.getArgumentAt(i), struct2.getArgumentAt(i));
        if (compareResult != 0) {
          return compareResult;
        }
      }

      final Term lastLeft = struct1.getArgumentAt(lastIndex);
      final Term lastRight = struct2.getArgumentAt(lastIndex);
      if (lastLeft == lastRight) {
        return 0;
      }
      final Term g1 = lastLeft.tryGround();
      final Term g2 = lastRight.tryGround();
      if (g1 == g2) {
        return 0;
      }
      if (g1 instanceof CompoundTerm && g2 instanceof CompoundTerm) {
        struct1 = (TermStruct) g1;
        struct2 = (TermStruct) g2;
        continue;
      }
      return this.compareGroundedTerms(g1, g2);
    }
  }
}
