package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.annotations.Determined;
import com.igormaznitsa.jprol.annotations.Predicate;
import com.igormaznitsa.jprol.annotations.ProlOperator;
import com.igormaznitsa.jprol.annotations.ProlOperators;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;

import java.util.Iterator;
import java.util.stream.Stream;

import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.*;

@SuppressWarnings("EmptyMethod")
@ProlOperators(operators = {
    @ProlOperator(priority = 700, type = XFX, name = "is"),
    @ProlOperator(priority = 700, type = XFX, name = "="),
    @ProlOperator(priority = 700, type = XFX, name = "\\="),
    @ProlOperator(priority = 0, type = XFX, name = "("),
    @ProlOperator(priority = 0, type = XFX, name = ")"),
    @ProlOperator(priority = 0, type = XFX, name = "["),
    @ProlOperator(priority = 0, type = XFX, name = "]"),
    @ProlOperator(priority = 1200, type = XF, name = "."),
    @ProlOperator(priority = 1200, type = XFX, name = "|"),
    @ProlOperator(priority = 1000, type = XFY, name = ","),
    @ProlOperator(priority = 1100, type = XFY, name = ";"),
    @ProlOperator(priority = 1200, type = FX, name = "?-"),
    @ProlOperator(priority = 1200, type = FX, name = ":-"),
    @ProlOperator(priority = 1200, type = XFX, name = ":-"),
    @ProlOperator(priority = 500, type = FX, name = "not")
})
public class JProlBootstrapLibrary extends AbstractJProlLibrary {
  public JProlBootstrapLibrary() {
    super("jprol-bootstrap-lib");
  }

  @Predicate(signature = "current_prolog_flag/2", template = {"?atom,?term"}, reference = "Check prolog flag and flag values.")
  public static boolean predicateCURRENTPROLOGFLAG(final ChoicePoint goal, final TermStruct predicate) {
    final Term atom = predicate.getElement(0).findNonVarOrSame();
    final Term term = predicate.getElement(1).findNonVarOrSame();

    final boolean only = atom.isGround();

    boolean found = false;
    Iterator<JProlSystemFlag> iterator = goal.getPayload();
    final boolean firstCall;
    if (iterator == null) {
      firstCall = true;
      iterator = Stream.of(JProlSystemFlag.values()).iterator();
      goal.setPayload(iterator);
    } else {
      firstCall = false;
    }

    while (iterator.hasNext()) {
      final JProlSystemFlag flag = iterator.next();
      if (atom.dryUnifyTo(flag.getNameTerm())) {
        final Term flagValue = goal.getContext().getSystemFlag(flag);
        if (term.dryUnifyTo(flagValue)) {
          if (!(atom.unifyTo(flag.getNameTerm()) && term.unifyTo(flagValue))) {
            throw new ProlCriticalError("Unexpected situation, can't unify prolog flag");
          } else {
            found = true;
            break;
          }
        }
      }

      if (only || !iterator.hasNext()) {
        goal.setPayload(null);
        goal.cutVariants();
      } else if (iterator.hasNext()) {
        goal.setPayload(iterator);
      }
    }

    if (only && firstCall && !found) {
      throw new ProlDomainErrorException("prolog_flag", atom);
    }

    return found;
  }

  @Predicate(signature = "kcontext/1", template = {"?atom"}, reference = "Set or get current knowledge context parameter.")
  @Determined
  public static boolean predicateKCONTEXT1(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (arg.getTermType() == TermType.VAR) {
      return arg.unifyTo(goal.getContext().getKnowledgeContext().asTerm());
    } else {
      goal.getContext().setKnowledgeContext(goal.getContext().getKnowledgeContextFactory().makeKnowledgeContext(arg.getText()));
      return true;
    }
  }

  @Predicate(signature = "set_prolog_flag/2", template = {"+atom,+term"}, reference = "Set value of flag.")
  @Determined
  public static boolean predicateSETPROLOGFLAG(final ChoicePoint goal, final TermStruct predicate) {
    final Term atom = predicate.getElement(0).findNonVarOrSame();
    final Term term = predicate.getElement(1).findNonVarOrSame();
    return JProlSystemFlag.find(atom)
        .filter(x -> !x.isReadOnly())
        .map(x -> {
          goal.getContext().setSystemFlag(x, term);
          return true;
        }).orElseThrow(() -> new ProlDomainErrorException("prolog_flag", atom));
  }

  @Predicate(signature = "is/2", template = {"?evaluable,@evaluable"}, reference = "'is'(Result, Expression) is true if and only if the value of evaluating Expression as an expression is Result")
  @Determined
  public static boolean predicateIS(final ChoicePoint goal, final TermStruct predicate) {
    final Term leftPart = predicate.getElement(0);

    final NumericTerm rightPart = calculatEvaluable(goal, predicate.getElement(1));
    if (rightPart == null) {
      return false;
    }
    return leftPart.unifyTo(rightPart);
  }

  @Predicate(signature = "true/0", reference = "The perdicate is always true.")
  @Determined
  public static void predicateTRUE(final ChoicePoint goal, final TermStruct predicate) {
  }

  @Predicate(signature = "fail/0", reference = "The predicate is always false.")
  @Determined
  public static boolean predicateFAIL(final ChoicePoint goal, final TermStruct predicate) {
    return false;
  }

  @Predicate(signature = "not/1", reference = "True if goal cannot be proven")
  @Determined
  public static boolean predicateNOT(final ChoicePoint goal, final TermStruct predicate) {
    final ChoicePoint localGoal = new ChoicePoint(predicate.getElement(0), goal.getContext());
    final Term result = localGoal.next();
    return result == null;
  }

  @Predicate(signature = "=/2", reference = "Unify X and Y terms. It is true if X and Y are unifiable.")
  @Determined
  public static boolean predicateEQU(final ChoicePoint goal, final TermStruct predicate) {
    final Term left = predicate.getElement(0);
    final Term right = predicate.getElement(1);
    return left.unifyTo(right);
  }

  @Predicate(signature = "\\=/2", reference = "Unify X and Y terms. It is true if X and Y are not-unifiable.")
  @Determined
  public static boolean predicateNOTEQU(final ChoicePoint goal, final TermStruct predicate) {
    final Term left = predicate.getElement(0);
    final Term right = predicate.getElement(1);
    return !left.unifyTo(right);
  }

  @Predicate(signature = ";/2", reference = "';'(Either, Or) is true if either Either or Or is true.")
  public static void predicateOR(final ChoicePoint goal, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @Predicate(signature = ",/2", reference = "','(First, Second) is true if and only if First is true and Second is true.")
  public static void predicateAND(final ChoicePoint goal, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @Predicate(signature = "!/0", reference = "! is true. All choice ponts between the cut and the parent goal are removed. The effect is commit to use of both the current clause and the substitutions found at the point of the cut.")
  @Determined
  public static void predicateCUT(final ChoicePoint goal, final TermStruct predicate) {
    // it is a stub function for embedded inside operator
  }

  @Predicate(signature = "!!/0", reference = "!! is true. Local version of !/0. It doesn't cut the knowledge base selection, i.e. it works only inbounds of current goal.")
  @Determined
  public static void predicateCUTLOCAL(final ChoicePoint goal, final TermStruct predicate) {
    // it is a stub function for embedded inside operator
  }
}
