package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.data.SourcePosition.UNKNOWN;
import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FX;

import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlAbortExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.kbase.IteratorType;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.lazy.LazySet;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;

/**
 * The library contains only guarded base core predicates which can make a critical operation like IO or thread.
 *
 * @since 3.0.0
 */
@JProlOperator(priority = 1150, type = FX, name = "dynamic")
public class JProlCoreGuardedLibrary extends AbstractJProlLibrary {

  public JProlCoreGuardedLibrary() {
    super("jprol-core-guarded-lib");
  }

  @JProlPredicate(
      guarded = true,
      signature = "clause/2",
      validate = ":head,?body",
      reference = "clause(Head, Body) is true if and only if\n* The predicate of Head is public (the standard does not specify how a predicate is declared public but dynamic predicates are public, and\n* There is a clause in the database which corresponds to a term H:- B which unifies with Head :- Body.")
  public static boolean predicateCLAUSE2(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term head = predicate.getArgumentAt(0).tryGround();
    final Term body = predicate.getArgumentAt(1).tryGround();

    final TermStruct struct = head.getTermType() == STRUCT ? (TermStruct) head : newStruct(head);
    if (goal.getContext().findProcessor(struct) != PredicateInvoker.NULL_PROCESSOR) {
      throw new ProlPermissionErrorException("access", "private_procedure", predicate);
    }

    Iterator<TermStruct> clIterator = goal.getInternalObject();

    if (clIterator == null) {
      clIterator = goal.getContext().getKnowledgeBase().iterate(
          IteratorType.ANY,
          head.getTermType() == STRUCT ? (TermStruct) head : newStruct(head),
          (signature, term) -> {
          }
      );
      if (!clIterator.hasNext()) {
        goal.cutVariants();
        return false;
      }

      goal.setInternalObject(clIterator);
    }

    TermStruct nxtStruct;
    while (clIterator.hasNext() && (nxtStruct = clIterator.next()) != null) {
      Term headClause;
      Term bodyClause;
      if (nxtStruct.isClause()) {
        headClause = nxtStruct.getArgumentAt(0);
        bodyClause = nxtStruct.getArgumentAt(1);
      } else {
        headClause = nxtStruct;
        bodyClause = Terms.TRUE;
      }

      if (head.isUnifiableWith(headClause) && body.isUnifiableWith(bodyClause)) {
        final boolean result = assertUnify(head, headClause) && assertUnify(body, bodyClause);
        head.arrangeVariablesWith(body);
        return result;
      }
    }
    goal.cutVariants();
    return false;
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "abort/1",
      synonyms = "abort/0",
      validate = "+term",
      reference = "Self-dispose current JProl context, it doesn't affect the root context."
  )
  public static void predicateAbort(final JProlChoicePoint goal, final TermStruct predicate) {
    if (predicate.getArity() == 0) {
      throw new ProlAbortExecutionException("Aborted", 0L);
    } else {
      final Term arg = predicate.getArgumentAt(0).tryGround();
      String abortMessage = null;
      long abortStatus = 0;
      if (arg instanceof NumericTerm) {
        abortStatus = arg.toNumber().longValue();
      } else {
        abortMessage = arg.getText();
      }
      throw new ProlAbortExecutionException(Objects.requireNonNullElse(abortMessage, "Aborted"),
          abortStatus);
    }
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "halt/1",
      synonyms = "halt/0",
      validate = "+number",
      reference = "Dispose the root JProl context."
  )
  public static void predicateHalt(final JProlChoicePoint goal, final TermStruct predicate) {
    JProlContext rootContext = goal.getContext();
    while (!rootContext.isRootContext()) {
      rootContext = rootContext.getParentContext();
    }

    if (predicate.getArity() == 0) {
      rootContext.dispose();
      throw new ProlHaltExecutionException("Halt called in code", 0L);
    } else {
      final Term arg = predicate.getArgumentAt(0).tryGround();
      long abortStatus = 0;
      if (arg instanceof NumericTerm) {
        abortStatus = arg.toNumber().longValue();
      }
      rootContext.dispose();
      throw new ProlHaltExecutionException("Halt called in code", abortStatus);
    }
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "abolish/1",
      validate = "+predicate_indicator",
      reference = "Removes all clauses of a predicate with functor Functor and arity Arity from the database. Abolishing an imported predicate only removes the import link; the predicate will keep its old definition in its definition module."
  )
  public static boolean predicateABOLISH1(final JProlChoicePoint goal, final TermStruct predicate) {
    return goal.getContext().abolish(predicate.getArgumentAt(0).tryGround());
  }

  @JProlPredicate(guarded = true,
      signature = "current_op/3",
      validate = "?integer,?operator_specifier,?atom",
      reference = "current_op(Priority, Op_specifier, TermOperator) is true if and only if TermOperator is an operator with properties given by Op_specifier and Priority"
  )
  public static boolean predicateCURRENTOP3(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final Term priority = predicate.getArgumentAt(0).tryGround();
    final Term specifier = predicate.getArgumentAt(1).tryGround();
    final Term name = predicate.getArgumentAt(2).tryGround();

    List<Iterator<TermOperator>> list = goal.getInternalObject();
    if (list == null) {
      list = new ArrayList<>();
      list.add(goal.getContext().getKnowledgeBase().makeOperatorIterator());
      final Iterator<AbstractJProlLibrary> libraries = goal.getContext().makeLibraryIterator();
      while (libraries.hasNext()) {
        list.add(libraries.next().makeOperatorIterator());
      }
      goal.setInternalObject(list);
    }

    while (!list.isEmpty()) {
      final Iterator<TermOperator> activeIterator = list.get(0);
      while (activeIterator.hasNext()) {
        final TermOperator found = activeIterator.next();
        final Term opPriority = Terms.newLong(found.getPrecedence());
        final Term opType = Terms.newAtom(found.getTypeAsString());
        final Term opName = Terms.newAtom(found.getText());

        if (priority.isUnifiableWith(opPriority) && specifier.isUnifiableWith(opType) &&
            name.isUnifiableWith(opName)) {
          return assertUnify(priority, opPriority) && assertUnify(specifier, opType) &&
              assertUnify(name, opName);
        }
      }
      list.remove(0);
    }
    goal.cutVariants();
    return false;
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "op/3",
      validate = {
          "+integer,+operator_specifier,+atom", "+integer,+operator_specifier,+list",
      },
      reference = "Predicate allows to alter operators.\nop(Priority, Op_Specifier, TermOperator) is true, with the side effect that\n1. if Priority is 0 then TermOperator is removed from operators\n2. TermOperator is added into operators, with priority (lower binds tighter) Priority and associativity determined by Op_Specifier"
  )
  public static boolean predicateOP(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg1 = predicate.getArgumentAt(0).tryGround();
    final Term arg2 = predicate.getArgumentAt(1).tryGround();
    final Term atomOrList = predicate.getArgumentAt(2).tryGround();

    final int priority = arg1.toNumber().intValue();
    final String specifier = arg2.getText();

    if (priority < 0L || priority > 1200L) {
      throw new ProlDomainErrorException("Priority must be between 0 and 1200 inclusive",
          predicate);
    }

    OpAssoc opType = OpAssoc.findForName(specifier)
        .orElseThrow(() -> new ProlDomainErrorException("Wrong operator specifier", predicate));

    final List<String> names = new ArrayList<>();
    if (atomOrList.getTermType() == LIST) {
      TermList list = (TermList) atomOrList;
      while (!list.isNullList()) {
        Term atom = list.getHead();
        if ((atom instanceof NumericTerm) || atom.getTermType() != ATOM) {
          throw new ProlDomainErrorException("Atom expected", predicate);
        }
        names.add(atom.getText());

        atom = list.getTail();
        if (atom.getTermType() != LIST) {
          throw new ProlDomainErrorException("List expected", predicate);
        }
        list = (TermList) atom;
      }
    } else {
      names.add(atomOrList.getText());
    }

    final KnowledgeBase base = goal.getContext().getKnowledgeBase();

    try {
      if (priority == 0) {
        names.forEach((name) -> base.removeOperator(name, opType));
      } else {
        names.forEach((name) -> base
            .addOperator(goal.getContext(), new TermOperator(priority, opType, name, UNKNOWN)));
      }
    } catch (SecurityException ex) {
      throw new ProlPermissionErrorException("create", "operator",
          "Attempt to override or remove a system operator", predicate);
    }
    return true;
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "asserta/1",
      validate = "+callable",
      reference = "Addition of a clause into the knowledge base before all other clauses."
  )
  public static boolean predicateASSERTA1(final JProlChoicePoint goal, final TermStruct predicate) {
    return goal.getContext().assertA(predicate.getArgumentAt(0).tryGround());
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "assertz/1",
      synonyms = "assert/1",
      validate = "+callable",
      reference = "Addition of a clause into the knowledge base after all other clauses."
  )
  public static boolean predicateASSERTZ1(final JProlChoicePoint goal, final TermStruct predicate) {
    return goal.getContext().assertZ(predicate.getArgumentAt(0).tryGround());
  }

  @JProlPredicate(
      guarded = true,
      signature = "retract/1",
      synonyms = "retracta/1",
      validate = "+callable",
      reference = "Retract the first clause which can be unified with argument. True if there is such clause in the knowledge base."
  )
  public static boolean predicateRETRACT1(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term retracting = predicate.getArgumentAt(0).tryGround();
    return goal.getContext().retractA(retracting);
  }

  @JProlPredicate(
      guarded = true,
      signature = "retractz/1",
      validate = "+callable",
      reference = "Retract the last clause which can be unified with argument. True if there is such clause in the knowledge base."
  )
  public static boolean predicateRETRACTZ(final JProlChoicePoint goal, final TermStruct predicate) {
    return goal.getContext().retractZ(predicate.getArgumentAt(0).tryGround());
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "retractall/1",
      validate = "+callable",
      reference = "Retract all clauses which can be unified with argument. Always succeeds."
  )
  public static boolean predicateRETRACTALL(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    goal.getContext().retractAll(predicate.getArgumentAt(0).tryGround());
    return true;
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "dynamic/1",
      validate = "+compound",
      reference = "Provide signature of predicate allowed dynamic knowledge base operations."
  )
  public static void predicateDYNAMIC1(final JProlChoicePoint goal,
                                      final TermStruct predicate) {
    final Term term = predicate.getArgumentAt(0).tryGround();
    final Set<String> indicators = new LazySet<>();

    final Consumer<Term> termConsumer = x -> {
      ProlAssertions.assertIndicator(x);
      final TermStruct struct = (TermStruct) x;
      final String name = struct.getArgumentAt(0).getText();
      final String arity = struct.getArgumentAt(1).getText();
      indicators.add(name + '/' + arity);
    };

    if (term.getTermType() == LIST) {
      final TermList list = (TermList) term;
      for (final Term value : list) {
        termConsumer.accept(value);
      }
    } else {
      termConsumer.accept(term);
    }

    goal.getContext().addDynamicSignatures(indicators, goal.getGoalTerm().getSourcePosition());
  }

  @JProlPredicate(
      guarded = true,
      signature = "current_prolog_flag/2",
      validate = "?atom,-term",
      reference = "Get current context prolog flag value."
  )
  public static boolean predicateCURRENTPROLOGFLAG2(final JProlChoicePoint choicePoint,
                                                   final TermStruct predicate) {
    final Term atom = predicate.getArgumentAt(0).tryGround();
    final Term term = predicate.getArgumentAt(1).tryGround();

    final boolean only = atom.isGround();

    boolean found = false;
    Iterator<JProlSystemFlag> iterator = choicePoint.getInternalObject();
    final boolean firstCall;
    if (iterator == null) {
      firstCall = true;
      iterator = JProlSystemFlag.VALUES.iterator();
      choicePoint.setInternalObject(iterator);
    } else {
      firstCall = false;
    }

    while (iterator.hasNext()) {
      final JProlSystemFlag flag = iterator.next();
      if (atom.isUnifiableWith(flag.getNameTerm())) {
        final Term flagValue = choicePoint.getContext().getSystemFlag(flag);
        if (term.isUnifiableWith(flagValue)) {
          found = assertUnify(atom, flag.getNameTerm()) && assertUnify(term, flagValue);
          break;
        }
      }

      if (only || !iterator.hasNext()) {
        choicePoint.setInternalObject(null);
        choicePoint.cutVariants();
      } else {
        choicePoint.setInternalObject(iterator);
      }
    }

    if (only && firstCall && !found) {
      throw new ProlDomainErrorException("prolog_flag", atom);
    }

    return found;
  }

  @JProlPredicate(
      guarded = true,
      determined = true,
      signature = "set_prolog_flag/2",
      validate = "+atom,+term",
      reference = "Set value of current context non-readonly flag, it will affect following calls."
  )
  public static boolean predicateSETPROLOGFLAG2(final JProlChoicePoint choicePoint,
                                               final TermStruct predicate) {
    final Term atom = predicate.getArgumentAt(0).tryGround();
    final Term term = predicate.getArgumentAt(1).tryGround();

    return JProlSystemFlag.find(atom)
        .filter(x -> !x.isReadOnly())
        .map(x -> {
          choicePoint.getContext().setSystemFlag(x, term);
          return true;
        }).orElseThrow(() -> new ProlDomainErrorException("prolog_flag", atom));
  }


}
