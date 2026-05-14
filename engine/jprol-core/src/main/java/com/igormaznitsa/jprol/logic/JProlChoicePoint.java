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
import java.util.OptionalInt;
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
      final RootVarState rootVar = RootVarState.of(grounded, variableMap);
      this.variables = rootVar.variables;
      this.varSnapshot = rootVar.snapshot;
      this.parentCp = this;
      this.prevCp = null;
    } else {
      this.variables = null;
      this.varSnapshot = grounded.getTermType() == ATOM
          ? null
          : new VariableStateSnapshot(rootChoicePoint.varSnapshot);
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

  private static boolean isBinaryFunctor(final TermStruct struct, final String functorText) {
    return struct.getArity() == 2 && struct.getFunctor().getText().equals(functorText);
  }

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

  private ProveStack newProveStack() {
    return new ProveStack(this.context.getMaxProveStackDepth());
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
    return this.proveNext((s, t) -> this.context.notifyAboutUndefinedPredicate(this, s, t),
        this.newProveStack());
  }

  public Term proveIgnoringUnknownPredicates() {
    return this.proveNext(NULL_UNDEFINED_PREDICATE_CONSUMER, this.newProveStack());
  }

  private Term proveNext(final BiConsumer<String, Term> unknownPredicateConsumer,
                         final ProveStack proveStack) {
    proveStack.push(this);

    stackLoop:
    while (!proveStack.isEmpty()) {
      Term result = null;
      boolean mainLoop = true;

      mainWhile:
      while (mainLoop) {
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
        }

        if (!resumeAfterSubgoal && !goalToProcess.hasAlternatives) {
          activeSelf.fireExitIfTrace(goalToProcess);
          root.parentCp = goalToProcess.prevCp;
          continue mainWhile;
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

            if (goalToProcess.varSnapshot != null) {
              goalToProcess.varSnapshot.resetToState();
            }

            final ProveStackFrame innerTop = proveStack.peek();
            if (innerTop.subgoalOutcomePending) {
              goalToProcess = innerTop.pendingResumeGoal;
              final Term solvedTerm = innerTop.pendingSubgoalResult;
              proveStack.replaceTop(innerTop.clearedSubgoalOutcome());

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
              continue;
            }

            if (goalToProcess.nextCp != null) {
              final ProveStackFrame beforeChild = proveStack.peek();
              proveStack.replaceTop(beforeChild.withPendingResumeGoal(goalToProcess));
              proveStack.push(goalToProcess.nextCp);
              continue stackLoop;
            }

            final Term theTerm =
                goalToProcess.goalTerm.tryGroundOrDefault(goalToProcess.goalTerm);

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
                if (struct.isClause()) {
                  goalToProcess.enqueueClauseSubgoal(struct);
                } else {
                  final NonClauseSolve solved =
                      goalToProcess.solveNonClauseStruct(struct, unknownPredicateConsumer);
                  cpResult = solved.cpResult;
                  internalLoop = solved.continueInnerWhile;
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

          final ProveMainStep step =
              this.applyCpResult(cpResult, activeSelf, root, goalToProcess);
          result = step.resultOrNull;
          mainLoop = step.continueMainWhile;
        } catch (StackOverflowError ex) {
          throw new ProlChoicePointStackOverflowException(
              "Caught stack overflow error during prove", activeSelf);
        }
      }

      proveStack.drop();
      if (proveStack.isEmpty()) {
        return result;
      }
      final ProveStackFrame caller = proveStack.peek();
      proveStack.replaceTop(caller.withSubgoalReturned(result));
    }

    return null;
  }

  private ProveMainStep applyCpResult(
      final int cpResult,
      final JProlChoicePoint activeSelf,
      final JProlChoicePoint root,
      final JProlChoicePoint goalToProcess) {
    switch (cpResult) {
      case CP_FAIL:
        activeSelf.fireFailAndExitIfTrace(goalToProcess);
        root.parentCp = goalToProcess.prevCp;
        return new ProveMainStep(null, true);
      case CP_SUCCESS: {
        final JProlChoicePoint current = root.parentCp;
        if (current.nextAndTerm == null) {
          return new ProveMainStep(root.goalTerm, false);
        }
        final JProlChoicePoint nextGoal =
            activeSelf.createChildChoicePoint(current.nextAndTerm);
        nextGoal.nextAndTerm = current.nextAndTermForNextGoal;
        return new ProveMainStep(null, true);
      }
      case CP_STACK_CHANGED:
        return new ProveMainStep(null, true);
      default:
        throw new AssertionError("Unexpected cpResult: " + cpResult);
    }
  }

  private void enqueueClauseSubgoal(final TermStruct struct) {
    final TermStruct structClone = (TermStruct) struct.makeClone();
    final Term head = structClone.getArgumentAt(0);
    final int arity = struct.getArity();

    this.thisConnector = struct.getArgumentAt(0);
    this.childConnectingTerm = head;
    this.nextCp = arity == 1
        ? this.context.makeChoicePoint(head, this.associatedObject)
        : this.context.makeChoicePoint(structClone.getArgumentAt(1), this.associatedObject);
  }

  private NonClauseSolve solveNonClauseStruct(
      final TermStruct struct,
      final BiConsumer<String, Term> unknownPredicateConsumer) {
    final OptionalInt control = this.tryDispatchBuiltInControl(struct);
    if (control.isPresent()) {
      return new NonClauseSolve(control.getAsInt(), false);
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
        this.resetLogicalAlternativesFlag();
        return new NonClauseSolve(CP_FAIL, false);
      }
      return new NonClauseSolve(CP_FAIL, true);
    }
    if (foundProcessor.isDetermined()) {
      this.resetLogicalAlternativesFlag();
    }
    int cpResult = CP_FAIL;
    if (foundProcessor.execute(this, struct)) {
      cpResult = CP_SUCCESS;
    }
    if (cpResult == CP_SUCCESS && foundProcessor.doesChangeGoalChain()) {
      cpResult = CP_STACK_CHANGED;
    }
    return new NonClauseSolve(cpResult, false);
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

  private OptionalInt tryDispatchBuiltInControl(final TermStruct struct) {
    if (struct.getArity() >= 3) {
      return OptionalInt.empty();
    }
    final OptionalInt cut = this.tryResolveCut(struct);
    if (cut.isPresent()) {
      return cut;
    }
    if (isBinaryFunctor(struct, ",")) {
      return this.scheduleConjunction(struct);
    }
    if (isBinaryFunctor(struct, ";")) {
      return this.scheduleDisjunction(struct);
    }
    return OptionalInt.empty();
  }

  private OptionalInt tryResolveCut(final TermStruct struct) {
    if (struct.getArity() != 0) {
      return OptionalInt.empty();
    }
    final String name = struct.getFunctor().getText();
    if (name.isEmpty() || name.charAt(0) != '!') {
      return OptionalInt.empty();
    }
    if ("!".equals(name)) {
      this.fullCut();
      return OptionalInt.of(CP_SUCCESS);
    }
    if ("!!".equals(name)) {
      this.localCut();
      return OptionalInt.of(CP_SUCCESS);
    }
    return OptionalInt.empty();
  }

  private OptionalInt scheduleConjunction(final TermStruct struct) {
    final JProlChoicePoint leftSubGoal = this.replaceLastGoalAtChain(struct.getArgumentAt(0));
    leftSubGoal.nextAndTerm = struct.getArgumentAt(1);
    leftSubGoal.nextAndTermForNextGoal = this.nextAndTerm;
    return OptionalInt.of(CP_STACK_CHANGED);
  }

  private OptionalInt scheduleDisjunction(final TermStruct struct) {
    if (this.getInternalObject() == null) {
      final JProlChoicePoint leftSubbranch = this.createChildChoicePoint(struct.getArgumentAt(0));
      leftSubbranch.nextAndTerm = this.nextAndTerm;
      this.setInternalObject(leftSubbranch);
    } else {
      this.replaceLastGoalAtChain(struct.getArgumentAt(1));
    }
    return OptionalInt.of(CP_STACK_CHANGED);
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

    switch (g1.getTermType()) {
      case ATOM:
        return this.compareGroundedAtomTo(g1, g2);
      case LIST:
      case STRUCT:
        return this.compareGroundedCompoundTo(g1, g2);
      case VAR:
        return g2.getTermType() == VAR ? g1.getText().compareTo(g2.getText()) : -1;
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
      return g2 instanceof NumericTerm ? 1 : -1;
    }
    return -1;
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

  private static final class RootVarState {

    private final Map<String, TermVar> variables;
    private final VariableStateSnapshot snapshot;

    private RootVarState(final Map<String, TermVar> variables,
                         final VariableStateSnapshot snapshot) {
      this.variables = variables;
      this.snapshot = snapshot;
    }

    private static RootVarState of(final Term grounded, final Map<String, Term> variableMap) {
      if (grounded.getTermType() == ATOM) {
        return new RootVarState(null, null);
      }
      return new RootVarState(
          grounded.findAllNamedVariables(),
          new VariableStateSnapshot(grounded, variableMap));
    }
  }

  private static final class ProveMainStep {

    private final Term resultOrNull;
    private final boolean continueMainWhile;

    private ProveMainStep(final Term resultOrNull, final boolean continueMainWhile) {
      this.resultOrNull = resultOrNull;
      this.continueMainWhile = continueMainWhile;
    }
  }

  private static final class NonClauseSolve {

    private final int cpResult;
    private final boolean continueInnerWhile;

    private NonClauseSolve(final int cpResult, final boolean continueInnerWhile) {
      this.cpResult = cpResult;
      this.continueInnerWhile = continueInnerWhile;
    }
  }

}
