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
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

/**
 * Bootstrap library to provide strongly required definitions for base Prolog operations.
 */
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

  @JProlPredicate(determined = true, signature = "is/2", validate = {
      "?term,+evaluable"}, reference = "'is'(Result, Expression) is true if and only if the value of evaluating Expression as an expression is Result")
  public static boolean predicateIS(final JProlChoicePoint choicePoint,
                                    final TermStruct predicate) {
    final Term left = predicate.getArgumentAt(0).tryGround();
    final Term right = predicate.getArgumentAt(1).tryGround();

    final NumericTerm rightResult = calcEvaluable(choicePoint, right);
    return rightResult != null && left.unifyWith(rightResult);
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

  @JProlPredicate(determined = true, signature = "not/1", validate = ":goal", reference = "True if goal cannot be proven")
  public static boolean predicateNOT(
      final JProlChoicePoint choicePoint,
      final TermStruct predicate
  ) {
    return
        choicePoint.getContext().makeChoicePoint(predicate.getArgumentAt(0).tryGround())
            .proveIgnoringUnknownPredicates() ==
            null;
  }

  @JProlPredicate(determined = true, signature = "=/2", validate = "?term,?term", reference = "Unify X and Y terms. It is true if X and Y are unifiable.")
  public static boolean predicateEQU(final JProlChoicePoint choicePoint,
                                     final TermStruct predicate) {
    return predicate.getArgumentAt(0).unifyWith(predicate.getArgumentAt(1));
  }

  @JProlPredicate(determined = true, signature = "\\=/2", validate = "@term,@term", reference = "Unify X and Y terms. It is true if X and Y are not-unifiable.")
  public static boolean predicateNOTEQU(final JProlChoicePoint choicePoint,
                                        final TermStruct predicate) {
    return !predicate.getArgumentAt(0).unifyWith(predicate.getArgumentAt(1));
  }

  @JProlPredicate(signature = ";/2", validate = ":goal,:goal", reference = "';'(Either, Or) is true if either Either or Or is true.")
  public static void predicateOR(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @JProlPredicate(signature = ",/2", validate = ":goal,:goal", reference = "','(First, Second) is true if and only if First is true and Second is true.")
  public static void predicateAND(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @JProlPredicate(determined = true, signature = "!/0", reference = "! is true. All choice points between the cut and the parent goal are removed. The effect is commit to use of both the current clause and the substitutions found at the point of the cut.")
  public static void predicateCUT(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    // it is a stub function for embedded inside operator
  }

  @JProlPredicate(determined = true, signature = "!!/0", reference = "A cut with local scope: commits to the current goal’s choices without cutting alternatives in enclosing goals.")
  public static void predicateCUTLOCAL(final JProlChoicePoint choicePoint,
                                       final TermStruct predicate) {
    // it is a stub function for embedded inside operator
  }
}
