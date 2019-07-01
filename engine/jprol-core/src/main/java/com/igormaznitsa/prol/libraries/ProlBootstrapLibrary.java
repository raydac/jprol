package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Predicate;
import com.igormaznitsa.prol.annotations.ProlOperator;
import com.igormaznitsa.prol.annotations.ProlOperators;
import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.logic.ChoicePoint;

import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.*;

@ProlOperators(Operators = {
    @ProlOperator(Priority = 700, Type = XFX, Name = "is"),
    @ProlOperator(Priority = 700, Type = XFX, Name = "="),
    @ProlOperator(Priority = 700, Type = XFX, Name = "\\="),
    @ProlOperator(Priority = 0, Type = XFX, Name = "("),
    @ProlOperator(Priority = 0, Type = XFX, Name = ")"),
    @ProlOperator(Priority = 0, Type = XFX, Name = "["),
    @ProlOperator(Priority = 0, Type = XFX, Name = "]"),
    @ProlOperator(Priority = 1200, Type = XF, Name = "."),
    @ProlOperator(Priority = 1200, Type = XFX, Name = "|"),
    @ProlOperator(Priority = 1000, Type = XFY, Name = ","),
    @ProlOperator(Priority = 1100, Type = XFY, Name = ";"),
    @ProlOperator(Priority = 1200, Type = FX, Name = "?-"),
    @ProlOperator(Priority = 1200, Type = FX, Name = ":-"),
    @ProlOperator(Priority = 1200, Type = XFX, Name = ":-"),
    @ProlOperator(Priority = 500, Type = FX, Name = "not")
})
public class ProlBootstrapLibrary extends AbstractProlLibrary {
  public ProlBootstrapLibrary() {
    super("prol-bootstrap-lib");
  }

  @Predicate(Signature = "is/2", Template = {"?evaluable,@evaluable"}, Reference = "'is'(Result, Expression) is true if and only if the value of evaluating Expression as an expression is Result")
  @Determined
  public static boolean predicateIS(final ChoicePoint goal, final TermStruct predicate) {

    final Term leftPart = predicate.getElement(0);

    final NumericTerm rightPart = calculatEvaluable(goal, predicate.getElement(1));
    if (rightPart == null) {
      return false;
    }
    return leftPart.unifyTo(rightPart);
  }

  @Predicate(Signature = "true/0", Reference = "The perdicate is always true.")
  @Determined
  public static void predicateTRUE(final ChoicePoint goal, final TermStruct predicate) {
  }

  @Predicate(Signature = "fail/0", Reference = "The predicate is always false.")
  @Determined
  public static boolean predicateFAIL(final ChoicePoint goal, final TermStruct predicate) {
    return false;
  }

  @Predicate(Signature = "not/1", Reference = "True if goal cannot be proven")
  @Determined
  public static boolean predicateNOT(final ChoicePoint goal, final TermStruct predicate) {
    final ChoicePoint localGoal = new ChoicePoint(predicate.getElement(0), goal.getContext());
    final Term result = localGoal.next();
    return result == null;
  }

  @Predicate(Signature = "=/2", Reference = "Unify X and Y terms. It is true if X and Y are unifiable.")
  @Determined
  public static boolean predicateEQU(final ChoicePoint goal, final TermStruct predicate) {
    final Term left = predicate.getElement(0);
    final Term right = predicate.getElement(1);
    return left.unifyTo(right);
  }

  @Predicate(Signature = "\\=/2", Reference = "Unify X and Y terms. It is true if X and Y are not-unifiable.")
  @Determined
  public static boolean predicateNOTEQU(final ChoicePoint goal, final TermStruct predicate) {
    final Term left = predicate.getElement(0);
    final Term right = predicate.getElement(1);
    return !left.unifyTo(right);
  }

  @Predicate(Signature = ";/2", Reference = "';'(Either, Or) is true if either Either or Or is true.")
  public static void predicateOR(final ChoicePoint goal, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @Predicate(Signature = ",/2", Reference = "','(First, Second) is true if and only if First is true and Second is true.")
  public static void predicateAND(final ChoicePoint goal, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @Predicate(Signature = "!/0", Reference = "! is true. All choice ponts between the cut and the parent goal are removed. The effect is commit to use of both the current clause and the substitutions found at the point of the cut.")
  @Determined
  public static void predicateCUT(final ChoicePoint goal, final TermStruct predicate) {
    // it is a stub function for embedded inside operator
  }

  @Predicate(Signature = "!!/0", Reference = "!! is true. Local version of !/0. It doesn't cut the knowledge base selection, i.e. it works only inbounds of current goal.")
  @Determined
  public static void predicateCUTLOCAL(final ChoicePoint goal, final TermStruct predicate) {
    // it is a stub function for embedded inside operator
  }
}
