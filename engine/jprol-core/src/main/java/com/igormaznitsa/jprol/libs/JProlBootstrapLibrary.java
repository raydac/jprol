package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XF;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFY;

import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.Iterator;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
@JProlOperator(priority = 700, type = XFX, name = "is")
@JProlOperator(priority = 700, type = XFX, name = "=")
@JProlOperator(priority = 700, type = XFX, name = "\\=")
@JProlOperator(priority = 0, type = XFX, name = "(")
@JProlOperator(priority = 0, type = XFX, name = ")")
@JProlOperator(priority = 0, type = XFX, name = "[")
@JProlOperator(priority = 0, type = XFX, name = "]")
@JProlOperator(priority = 1200, type = XF, name = ".")
@JProlOperator(priority = 1200, type = XFX, name = "|")
@JProlOperator(priority = 1000, type = XFY, name = ",")
@JProlOperator(priority = 1100, type = XFY, name = ";")
@JProlOperator(priority = 1200, type = FX, name = "?-")
@JProlOperator(priority = 1200, type = FX, name = ":-")
@JProlOperator(priority = 1200, type = XFX, name = ":-")
@JProlOperator(priority = 500, type = FX, name = "not")
public class JProlBootstrapLibrary extends AbstractJProlLibrary {
  public JProlBootstrapLibrary() {
    super("jprol-bootstrap-lib");
  }

  @JProlPredicate(signature = "current_prolog_flag/2", args = {
      "?atom,?term"}, reference = "Check prolog flag and flag values.")
  public static boolean predicateCURRENTPROLOGFLAG(final JProlChoicePoint choicePoint,
                                                   final TermStruct predicate) {
    final Term atom = predicate.getElement(0).findNonVarOrSame();
    final Term term = predicate.getElement(1).findNonVarOrSame();

    if (choicePoint.isArgsValidate() && atom.getTermType() != TermType.VAR) {
      ProlAssertions.assertAtom(atom);
    }

    final boolean only = atom.isGround();

    boolean found = false;
    Iterator<JProlSystemFlag> iterator = choicePoint.getPayload();
    final boolean firstCall;
    if (iterator == null) {
      firstCall = true;
      iterator = JProlSystemFlag.VALUES.iterator();
      choicePoint.setPayload(iterator);
    } else {
      firstCall = false;
    }

    while (iterator.hasNext()) {
      final JProlSystemFlag flag = iterator.next();
      if (atom.dryUnifyTo(flag.getNameTerm())) {
        final Term flagValue = choicePoint.getContext().getSystemFlag(flag);
        if (term.dryUnifyTo(flagValue)) {
          found = assertUnify(atom, flag.getNameTerm()) && assertUnify(term, flagValue);
          break;
        }
      }

      if (only || !iterator.hasNext()) {
        choicePoint.setPayload(null);
        choicePoint.cutVariants();
      } else {
        choicePoint.setPayload(iterator);
      }
    }

    if (only && firstCall && !found) {
      throw new ProlDomainErrorException("prolog_flag", atom);
    }

    return found;
  }

  @JProlPredicate(
      determined = true,
      signature = "set_prolog_flag/2",
      args = {"+atom,+term"},
      reference = "Set value of flag."
  )
  public static boolean predicateSETPROLOGFLAG(final JProlChoicePoint choicePoint,
                                               final TermStruct predicate) {
    final Term atom = predicate.getElement(0).findNonVarOrSame();
    final Term term = predicate.getElement(1).findNonVarOrSame();

    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertAtom(atom);
      ProlAssertions.assertNonVar(term);
    }

    return JProlSystemFlag.find(atom)
        .filter(x -> !x.isReadOnly())
        .map(x -> {
          choicePoint.getContext().setSystemFlag(x, term);
          return true;
        }).orElseThrow(() -> new ProlDomainErrorException("prolog_flag", atom));
  }

  @JProlPredicate(determined = true, signature = "is/2", args = {
      "-number,+evaluable"}, reference = "'is'(Result, Expression) is true if and only if the value of evaluating Expression as an expression is Result")
  public static boolean predicateIS(final JProlChoicePoint choicePoint,
                                    final TermStruct predicate) {
    final Term left = predicate.getElement(0).findNonVarOrSame();
    final Term right = predicate.getElement(1).findNonVarOrSame();

    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertEvaluable(right);
    }

    final NumericTerm rightResult = calcEvaluable(choicePoint, right);
    return rightResult != null && left.unifyTo(rightResult);
  }

  @JProlPredicate(determined = true, signature = "true/0", reference = "The predicate is always true.")
  public static void predicateTRUE(final JProlChoicePoint choicePoint, final TermStruct predicate) {
  }

  @JProlPredicate(determined = true, synonyms = {
      "false/0"}, signature = "fail/0", reference = "The predicate is always false.")
  public static boolean predicateFAIL(final JProlChoicePoint choicePoint,
                                      final TermStruct predicate) {
    return false;
  }

  @JProlPredicate(determined = true, signature = "not/1", reference = "True if goal cannot be proven")
  public static boolean predicateNOT(final JProlChoicePoint choicePoint,
                                     final TermStruct predicate) {
    return
        choicePoint.makeForGoal(predicate.getElement(0).findNonVarOrSame())
            .proveWithFailForUnknown() ==
            null;
  }

  @JProlPredicate(determined = true, signature = "=/2", reference = "Unify X and Y terms. It is true if X and Y are unifiable.")
  public static boolean predicateEQU(final JProlChoicePoint choicePoint,
                                     final TermStruct predicate) {
    return predicate.getElement(0).unifyTo(predicate.getElement(1));
  }

  @JProlPredicate(determined = true, signature = "\\=/2", reference = "Unify X and Y terms. It is true if X and Y are not-unifiable.")
  public static boolean predicateNOTEQU(final JProlChoicePoint choicePoint,
                                        final TermStruct predicate) {
    return !predicate.getElement(0).unifyTo(predicate.getElement(1));
  }

  @JProlPredicate(signature = ";/2", reference = "';'(Either, Or) is true if either Either or Or is true.")
  public static void predicateOR(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @JProlPredicate(signature = ",/2", reference = "','(First, Second) is true if and only if First is true and Second is true.")
  public static void predicateAND(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @JProlPredicate(determined = true, signature = "!/0", reference = "! is true. All choice points between the cut and the parent goal are removed. The effect is commit to use of both the current clause and the substitutions found at the point of the cut.")
  public static void predicateCUT(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    // it is a stub function for embedded inside operator
  }
}
