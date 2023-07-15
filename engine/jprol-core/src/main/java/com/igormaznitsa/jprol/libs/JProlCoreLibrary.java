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

package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.LIST_FUNCTOR;
import static com.igormaznitsa.jprol.data.Terms.NULL_LIST;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newDouble;
import static com.igormaznitsa.jprol.data.Terms.newList;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.data.Terms.newVar;
import static com.igormaznitsa.jprol.utils.Utils.createOrAppendToList;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FY;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFY;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.YFX;

import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlAbstractCatchableException;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlCustomErrorException;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlHaltExecutionException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.exceptions.ProlRepresentationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.kbase.IteratorType;
import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlTreeBuilder;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggerType;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggeringEventObserver;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.Utils;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
@JProlOperator(priority = 1050, type = XFY, name = "->")
@JProlOperator(priority = 900, type = FY, name = "\\+")
@JProlOperator(priority = 700, type = XFX, name = ">")
@JProlOperator(priority = 700, type = XFX, name = "<")
@JProlOperator(priority = 700, type = XFX, name = "=<")
@JProlOperator(priority = 700, type = XFX, name = ">=")
@JProlOperator(priority = 700, type = XFX, name = "==")
@JProlOperator(priority = 700, type = XFX, name = "=\\=")
@JProlOperator(priority = 700, type = XFX, name = "\\==")
@JProlOperator(priority = 700, type = XFX, name = "@<")
@JProlOperator(priority = 700, type = XFX, name = "@>")
@JProlOperator(priority = 700, type = XFX, name = "@=<")
@JProlOperator(priority = 700, type = XFX, name = "@>=")
@JProlOperator(priority = 700, type = XFX, name = "=:=")
@JProlOperator(priority = 700, type = XFX, name = "=..")
@JProlOperator(priority = 500, type = YFX, name = "/\\")
@JProlOperator(priority = 500, type = YFX, name = "\\/")
@JProlOperator(priority = 500, type = YFX, name = "+")
@JProlOperator(priority = 500, type = YFX, name = "-")
@JProlOperator(priority = 500, type = FX, name = "+")
@JProlOperator(priority = 500, type = FX, name = "-")
@JProlOperator(priority = 400, type = YFX, name = "*")
@JProlOperator(priority = 400, type = YFX, name = "/")
@JProlOperator(priority = 400, type = YFX, name = "//")
@JProlOperator(priority = 400, type = YFX, name = "rem")
@JProlOperator(priority = 400, type = YFX, name = "<<")
@JProlOperator(priority = 400, type = YFX, name = ">>")
@JProlOperator(priority = 300, type = XFX, name = "mod")
@JProlOperator(priority = 200, type = FY, name = "\\")
@JProlOperator(priority = 200, type = XFX, name = "**")
@JProlOperator(priority = 200, type = XFY, name = "^")
public final class JProlCoreLibrary extends AbstractJProlLibrary {

  public JProlCoreLibrary() {
    super("jprol-core-lib");
  }

  @JProlPredicate(determined = true, signature = "=:=/2", args = {
          "@evaluable,@evaluable"}, reference = "Arithmetic equal")
  public static boolean predicateArithEqu(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1));
    return left.compare(right) == 0;
  }

  @JProlPredicate(determined = true, signature = "@</2", args = {
          "?term,?term"}, reference = "Term less than")
  public static boolean predicateTermLess(final JProlChoicePoint choicePoint,
                                          final TermStruct predicate) {
    return choicePoint.compare(predicate.getElement(0), predicate.getElement(1)) < 0;
  }

  @JProlPredicate(determined = true, signature = "@=</2", args = {
          "?term,?term"}, reference = "Term less than or equal to.")
  public static boolean predicateTermLessOrEqu(final JProlChoicePoint choicePoint,
                                               final TermStruct predicate) {
    return choicePoint.compare(predicate.getElement(0), predicate.getElement(1)) <= 0;
  }

  @JProlPredicate(determined = true, signature = "@>/2", args = {
          "?term,?term"}, reference = "Term greater than")
  public static boolean predicateTermMore(final JProlChoicePoint choicePoint,
                                          final TermStruct predicate) {
    return choicePoint.compare(predicate.getElement(0), predicate.getElement(1)) > 0;
  }

  @JProlPredicate(determined = true, signature = "@>=/2", args = {
          "?term,?term"}, reference = "Term greater than or equal to.")
  public static boolean predicateTermMoreOrEqu(final JProlChoicePoint choicePoint,
                                               final TermStruct predicate) {
    return choicePoint.compare(predicate.getElement(0), predicate.getElement(1)) >= 0;
  }

  @JProlPredicate(determined = true, signature = "==/2", args = {
          "?term,?term"}, reference = "Term identical")
  public static boolean predicateTermEqu(final JProlChoicePoint choicePoint,
                                         final TermStruct predicate) {
    return choicePoint.compare(predicate.getElement(0), predicate.getElement(1)) == 0;
  }

  @JProlPredicate(determined = true, signature = "\\==/2", args = {
          "?term,?term"}, reference = "Term not identical")
  public static boolean predicateNotTermEqu(final JProlChoicePoint choicePoint,
                                            final TermStruct predicate) {
    return choicePoint.compare(predicate.getElement(0), predicate.getElement(1)) != 0;
  }

  @JProlPredicate(determined = true, signature = ">/2", args = {
          "+evaluable,+evaluable"}, reference = "Arithmetic greater than")
  public static boolean predicateArithMore(final JProlChoicePoint goal,
                                           final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());
    return left.compare(right) > 0;
  }

  @JProlPredicate(determined = true, signature = "</2", args = {
          "+evaluable,+evaluable"}, reference = "Arithmetic less than")
  public static boolean predicateArithLess(final JProlChoicePoint goal,
                                           final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());
    return left.compare(right) < 0;
  }

  @JProlPredicate(determined = true, signature = ">=/2", args = {
          "+evaluable,+evaluable"}, reference = "Arithmetic greater than or equal to")
  public static boolean predicateArithMoreOrEqu(final JProlChoicePoint goal,
                                                final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());
    return left.compare(right) >= 0;
  }

  @JProlPredicate(determined = true, signature = "=</2", args = {
          "+evaluable,+evaluable"}, reference = "Arithmetic less than or equal to")
  public static boolean predicateArithLessOrEqu(final JProlChoicePoint goal,
                                                final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());
    return left.compare(right) <= 0;
  }

  @JProlPredicate(determined = true, signature = "=\\=/2", args = {
          "+evaluable,+evaluable"}, reference = "Arithmetic Not equal")
  public static boolean predicateArithNotEqu(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());
    return left.compare(right) != 0;
  }

  @JProlPredicate(evaluable = true, signature = "xor/2", args = {
          "+evaluable,+evaluable"}, reference = "Bitwise exclusive or.")
  public static Term predicateXOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(left);
      ProlAssertions.assertInteger(right);
    }
    return newLong(left.toNumber().longValue() ^ right.toNumber().longValue());
  }

  @JProlPredicate(evaluable = true, signature = "\\/1", args = {
          "+evaluable"}, reference = "Bitwise 'not'")
  public static Term predicateBITWISENOT(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0));
    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(arg);
    }
    return newLong(~arg.toNumber().longValue());
  }

  @JProlPredicate(evaluable = true, signature = "\\//2", args = {
          "+evaluable,+evaluable"}, reference = "Bitwise 'or'")
  public static Term predicateBITWISEOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1));

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(left);
      ProlAssertions.assertInteger(right);
    }
    return newLong(left.toNumber().longValue() | right.toNumber().longValue());
  }

  @JProlPredicate(evaluable = true, signature = "/\\/2", args = {
          "+evaluable,+evaluable"}, reference = "Bitwise 'and'")
  public static Term predicateBITWISEAND(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(left);
      ProlAssertions.assertInteger(right);
    }
    return newLong(left.toNumber().longValue() & right.toNumber().longValue());
  }

  @JProlPredicate(evaluable = true, signature = "mod/2", args = {
          "+evaluable,+evaluable"}, reference = "Modulus")
  public static Term predicateMOD(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(left);
      ProlAssertions.assertInteger(right);
    }
    final long rightNum = right.toNumber().longValue();
    if (rightNum == 0L) {
      throw new ProlEvaluationErrorException("zero divisor", predicate);
    }
    return newLong(left.toNumber().longValue() % rightNum);
  }

  @JProlPredicate(evaluable = true, signature = "rem/2", args = {
          "+evaluable,+evaluable"}, reference = "Remainder")
  public static Term predicateREM(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(left);
      ProlAssertions.assertInteger(right);
    }

    final long leftNum = left.toNumber().longValue();
    final long rightNum = right.toNumber().longValue();
    if (rightNum == 0L) {
      throw new ProlEvaluationErrorException("zero divisor", predicate);
    }
    return newLong(leftNum - (leftNum / rightNum) * rightNum);
  }

  @JProlPredicate(evaluable = true, signature = "**/2", args = {
          "+evaluable,+evaluable"}, reference = "Power")
  public static Term predicatePOWER(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    final double valueLeft = left.toNumber().doubleValue();
    final double valueRight = right.toNumber().doubleValue();

    return newDouble(Math.pow(valueLeft, valueRight));
  }

  @JProlPredicate(evaluable = true, signature = "+/2", args = {
          "+evaluable,+evaluable"}, reference = "Addition")
  public static Term predicateADDTWO(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());
    return left.add(right);
  }

  @JProlPredicate(evaluable = true, signature = "sin/1", args = {"+evaluable"}, reference = "Sine")
  public static Term predicateSIN(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    return newDouble(Math.sin(arg.toNumber().doubleValue()));
  }

  @JProlPredicate(evaluable = true, signature = "float_integer_part/1", args = {
          "+evaluable"}, reference = "Integer part")
  public static Term predicateFLOATINTEGERPART(final JProlChoicePoint goal,
                                               final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    return newLong(arg.toNumber().longValue());
  }

  @JProlPredicate(evaluable = true, signature = "float_fractional_part/1", args = {
          "+evaluable"}, reference = "Fractional part")
  public static Term predicateFLOATFRACTIONALPART(final JProlChoicePoint goal,
                                                  final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    final long valueInt = (long) value;
    return newDouble(value - (double) valueInt);
  }

  @JProlPredicate(evaluable = true, signature = "floor/1", args = {
          "+evaluable"}, reference = "Floor")
  public static Term predicateFLOOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newLong((long) Math.floor(value));
  }

  @JProlPredicate(evaluable = true, signature = "truncate/1", args = {
          "+evaluable"}, reference = "Truncate")
  public static Term predicateTRUNCATE(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newLong(value < 0 ? (long) Math.ceil(value) : (long) Math.floor(value));
  }

  @JProlPredicate(evaluable = true, signature = "round/1", args = {
          "+evaluable"}, reference = "Round")
  public static Term predicateROUND(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newLong(Math.round(value));
  }

  @JProlPredicate(evaluable = true, signature = "ceiling/1", args = {
          "+evaluable"}, reference = "Ceiling")
  public static Term predicateCEILING(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newLong((long) Math.ceil(value));
  }

  @JProlPredicate(evaluable = true, signature = "cos/1", args = {
          "+evaluable"}, reference = "Cosine")
  public static Term predicateCOS(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.cos(value));
  }

  @JProlPredicate(evaluable = true, signature = "atan/1", args = {
          "+evaluable"}, reference = "Arc tangent")
  public static Term predicateATAN(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.atan(value));
  }

  @JProlPredicate(evaluable = true, signature = "exp/1", args = {
          "+evaluable"}, reference = "Exponentiation")
  public static Term predicateEXP(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.exp(value));
  }

  @JProlPredicate(evaluable = true, signature = "log/1", args = {"+evaluable"}, reference = "Log")
  public static Term predicateLOG(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.log(value));
  }

  @JProlPredicate(evaluable = true, signature = "sqrt/1", args = {
          "+evaluable"}, reference = "Square root")
  public static Term predicateSQRT(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.sqrt(value));
  }

  @JProlPredicate(evaluable = true, signature = "abs/1", args = {
          "+evaluable"}, reference = "Absolute value")
  public static Term predicateABS(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    return arg.abs();
  }

  @JProlPredicate(evaluable = true, signature = "sign/1", args = {"+evaluable"}, reference = "SIGN")
  public static Term predicateSIGN(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    return arg.sign();
  }

  @JProlPredicate(signature = "sub_atom/5", args = {
          "+atom,?integer,?integer,?integer,?atom"}, reference = "Breaking atoms")
  public static boolean predicateSUBATOM(final JProlChoicePoint goal, final TermStruct predicate) {
    class SubAtomIterator {
      final String atom;
      final int initialLen;
      final String theSub;
      int currentBefore;
      int currentLength;
      int currentAfter;

      private SubAtomIterator(final Term atom, final Term before, final Term length,
                              final Term after, final Term sub) {
        this.atom = atom.getText();
        this.currentBefore = before.getTermType() == VAR ? 0 : before.toNumber().intValue();
        this.currentLength = length.getTermType() == VAR ? 0 : length.toNumber().intValue();
        this.currentAfter =
                after.getTermType() == VAR ? this.atom.length() : after.toNumber().intValue();
        this.theSub = sub.getTermType() == VAR ? null : sub.getText();

        if (length.getTermType() == VAR) {
          this.initialLen = -1;
        } else {
          this.initialLen = this.currentLength;
        }

        if (before.getTermType() == VAR && after.getTermType() != VAR) {
          this.currentBefore = this.atom.length() - this.currentAfter - this.currentLength;
        }

        if (before.getTermType() != VAR && after.getTermType() == VAR) {
          this.currentAfter = this.atom.length() - this.currentBefore - this.currentLength;
        }

        if (length.getTermType() == VAR) {
          this.currentLength = this.atom.length() - this.currentBefore - this.currentAfter;
        } else if (after.getTermType() == VAR) {
          this.currentAfter = this.atom.length() - this.currentBefore - this.currentLength;
        }
      }

      boolean next(final Term before, final Term length, final Term after, final Term sub) {
        if (this.currentBefore < 0 || this.currentAfter < 0 || this.currentLength < 0) {
          return false;
        }

        final String currentSub =
                this.atom.substring(this.currentBefore, this.currentBefore + this.currentLength);

        final boolean result = before.unifyTo(Terms.newLong(this.currentBefore))
                && length.unifyTo(Terms.newLong(this.currentLength))
                && after.unifyTo(Terms.newLong(this.currentAfter))
                && sub.unifyTo(Terms.newAtom(currentSub));

        if (this.theSub == null) {
          if (this.initialLen < 0) {
            this.currentLength++;
            this.currentAfter = Math.max(0, this.currentAfter - 1);
            if (this.currentBefore + this.currentLength + this.currentAfter > this.atom.length()) {
              this.currentBefore++;
              this.currentLength = 0;
              this.currentAfter = this.atom.length() - this.currentLength - this.currentBefore;
            }
          } else {
            this.currentBefore++;
            this.currentAfter = this.atom.length() - this.currentLength - this.currentBefore;
          }
        } else {
          this.currentBefore = this.atom.indexOf(this.theSub, this.currentBefore + 1);
          this.currentLength = this.theSub.length();
          this.currentAfter = this.atom.length() - this.currentBefore - this.currentLength;
        }

        return result;
      }
    }

    SubAtomIterator iterator = goal.getPayload();
    if (iterator == null) {
      final Term arg1 = predicate.getElement(0).findNonVarOrSame();
      final Term arg2 = predicate.getElement(1).findNonVarOrSame();
      final Term arg3 = predicate.getElement(2).findNonVarOrSame();
      final Term arg4 = predicate.getElement(3).findNonVarOrSame();
      final Term arg5 = predicate.getElement(4).findNonVarOrSame();

      if (goal.isArgsValidate()) {
        ProlAssertions.assertAtom(arg1);
        if (arg2.getTermType() != VAR) {
          ProlAssertions.assertInteger(arg2);
        }
        if (arg3.getTermType() != VAR) {
          ProlAssertions.assertInteger(arg3);
        }
        if (arg4.getTermType() != VAR) {
          ProlAssertions.assertInteger(arg4);
        }
        if (arg5.getTermType() != VAR) {
          ProlAssertions.assertAtom(arg5);
        }
      }

      iterator = new SubAtomIterator(arg1, arg2, arg3, arg4, arg5);
      goal.setPayload(iterator);
    }

    return iterator.next(
            predicate.getElement(1),
            predicate.getElement(2),
            predicate.getElement(3),
            predicate.getElement(4)
    );
  }

  @JProlPredicate(evaluable = true, signature = "-/2", args = {
          "+evaluable,+evaluable"}, reference = "Subtraction")
  public static Term predicateSUBTWO(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());
    return left.sub(right);
  }

  @JProlPredicate(evaluable = true, signature = "-/1", args = {
          "+evaluable"}, reference = "Negation")
  public static Term predicateNeg(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm val = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    return val.neg();
  }

  @JProlPredicate(evaluable = true, signature = "+/1", args = {
          "+evaluable"}, reference = "Not action over a number")
  public static Term predicateTheSame(final JProlChoicePoint goal, final TermStruct predicate) {
    return calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
  }

  @JProlPredicate(evaluable = true, signature = "*/2", args = {
          "+evaluable,+evaluable"}, reference = "Multiplication")
  public static Term predicateMUL(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());
    return left.mul(right);
  }

  @JProlPredicate(evaluable = true, signature = "//2", args = {
          "+evaluable,+evaluable"}, reference = "Division")
  public static Term predicateDIV(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    try {
      if ((right instanceof TermDouble && Double.compare(0.0d, right.toNumber().doubleValue()) == 0)
              || (right instanceof TermLong && right.toNumber().longValue() == 0L)) {
        throw new ArithmeticException("Zero divisor");
      }
      return left.div(right);
    } catch (ArithmeticException ex) {
      throw new ProlEvaluationErrorException(ex.getMessage(), predicate, ex);
    }
  }

  @JProlPredicate(evaluable = true, signature = "///2", args = {
          "+evaluable,+evaluable"}, reference = "Integer division.")
  public static Term predicateIDIV2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(left);
      ProlAssertions.assertInteger(right);
    }

    try {
      return left.div(right);
    } catch (ArithmeticException ex) {
      throw new ProlEvaluationErrorException(ex.getMessage(), predicate, ex);
    }
  }

  @JProlPredicate(evaluable = true, signature = "<</2", args = {
          "+evaluable,+evaluable"}, reference = "Bitwise left shift")
  public static Term predicateSHIFTL2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    final long value = left.toNumber().longValue();
    final long shift = right.toNumber().longValue();

    return newLong(value << shift);
  }

  @JProlPredicate(evaluable = true, signature = ">>/2", args = {
          "+evaluable,+evaluable"}, reference = "Bitwise right shift")
  public static Term predicateSHIFTR(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
    final NumericTerm right = calcEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

    final long value = left.toNumber().longValue();
    final long shift = right.toNumber().longValue();

    return newLong(value >> shift);
  }

  @JProlPredicate(signature = "repeat/0", reference = "repeat is true. It just places a choice point every call.")
  public static void predicateREPEAT0(final JProlChoicePoint goal, final TermStruct predicate) {
    // we just make choose point
  }

  @JProlPredicate(signature = "clause/2", args = {
          "+head,?callable"}, reference = "clause(Head, Body) is true if and only if\n* The predicate of Head is public (the standard does not specify how a predicate is declared public but dynamic predicates are public, and\n* There is a clause in the database which corresponds to a term H:- B which unifies with Head :- Body.")
  public static boolean predicateCLAUSE2(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term head = predicate.getElement(0).findNonVarOrSame();
    final Term body = predicate.getElement(1).findNonVarOrSame();

    final TermStruct struct = head.getTermType() == STRUCT ? (TermStruct) head : newStruct(head);
    if (goal.getContext().findProcessor(struct) != PredicateInvoker.NULL_PROCESSOR) {
      throw new ProlPermissionErrorException("access", "private_procedure", predicate);
    }

    Iterator<TermStruct> clIterator = goal.getPayload();

    if (clIterator == null) {
      if (goal.isArgsValidate()) {
        ProlAssertions.assertHead(head);
        if (body.getTermType() != VAR) {
          ProlAssertions.assertCallable(body);
        }
      }

      clIterator = goal.getContext().getKnowledgeBase().iterate(
              IteratorType.ANY,
              head.getTermType() == STRUCT ? (TermStruct) head : newStruct(head),
              x -> {
              }
      );
      if (!clIterator.hasNext()) {
        goal.cutVariants();
        return false;
      }

      goal.setPayload(clIterator);
    }

    TermStruct nxtStruct;
    while (clIterator.hasNext() && (nxtStruct = clIterator.next()) != null) {
      Term headClause;
      Term bodyClause;
      if (nxtStruct.isClause()) {
        headClause = nxtStruct.getElement(0);
        bodyClause = nxtStruct.getElement(1);
      } else {
        headClause = nxtStruct;
        bodyClause = Terms.TRUE;
      }

      if (head.dryUnifyTo(headClause) && body.dryUnifyTo(bodyClause)) {
        final boolean result = assertUnify(head, headClause) && assertUnify(body, bodyClause);
        head.arrangeVariablesInsideTerms(body);
        return result;
      }
    }
    goal.cutVariants();
    return false;
  }

  @JProlPredicate(signature = "current_op/3", args = "?integer,?operator_specifier,?atom", reference = "current_op(Priority, Op_specifier, TermOperator) is true if and only if TermOperator is an operator with properties given by  Op_specifier and Priority")
  public static boolean predicateCURRENTOP3(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final Term priority = predicate.getElement(0).findNonVarOrSame();
    final Term specifier = predicate.getElement(1).findNonVarOrSame();
    final Term name = predicate.getElement(2).findNonVarOrSame();

    List<Iterator<TermOperator>> list = goal.getPayload();
    if (list == null) {
      if (goal.isArgsValidate()) {
        if (priority.getTermType() != VAR) {
          ProlAssertions.assertInteger(priority);
        }
        if (specifier.getTermType() != VAR) {
          ProlAssertions.assertOperatorSpecifier(specifier);
        }
        if (name.getTermType() != VAR) {
          ProlAssertions.assertAtom(name);
        }
      }

      list = new ArrayList<>();
      list.add(goal.getContext().getKnowledgeBase().makeOperatorIterator());
      final Iterator<AbstractJProlLibrary> libraries = goal.getContext().makeLibraryIterator();
      while (libraries.hasNext()) {
        list.add(libraries.next().makeOperatorIterator());
      }
      goal.setPayload(list);
    }

    while (!list.isEmpty()) {
      final Iterator<TermOperator> activeIterator = list.get(0);
      while (activeIterator.hasNext()) {
        final TermOperator found = activeIterator.next();
        final Term opPriority = Terms.newLong(found.getPriority());
        final Term opType = Terms.newAtom(found.getTypeAsString());
        final Term opName = Terms.newAtom(found.getText());

        if (priority.dryUnifyTo(opPriority) && specifier.dryUnifyTo(opType) &&
                name.dryUnifyTo(opName)) {
          return assertUnify(priority, opPriority) && assertUnify(specifier, opType) &&
                  assertUnify(name, opName);
        }
      }
      list.remove(0);
    }
    goal.cutVariants();
    return false;
  }

  @JProlPredicate(determined = true, signature = "op/3", args = "+integer,+operator_specifier,+atom_or_atom_list", reference = "Predicate allows to alter operators.\nop(Priority, Op_Specifier, TermOperator) is true, with the side effect that\n1. if Priority is 0 then TermOperator is removed from operators\n2. TermOperator is added into operators, with priority (lower binds tighter) Priority and associativity determined by Op_Specifier")
  public static boolean predicateOP(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg1 = predicate.getElement(0).findNonVarOrSame();
    final Term arg2 = predicate.getElement(1).findNonVarOrSame();
    final Term atomOrList = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(arg1);
      ProlAssertions.assertOperatorSpecifier(arg2);
      ProlAssertions.assertAtomOrAtomList(atomOrList);
    }

    final int priority = arg1.toNumber().intValue();
    final String specifier = arg2.getText();

    if (priority < 0L || priority > 1200L) {
      throw new ProlDomainErrorException("Priority must be between 0 and 1200 inclusive",
              predicate);
    }

    OpAssoc opType = OpAssoc.findForName(specifier)
            .orElseThrow(() -> new ProlDomainErrorException("Wrong operator specifier", predicate));

    final ArrayList<String> names = new ArrayList<>();
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
                .addOperator(goal.getContext(), new TermOperator(priority, opType, name)));
      }
    } catch (SecurityException ex) {
      throw new ProlPermissionErrorException("create", "operator",
              "Attempt to override or remove a system operator", predicate);
    }
    return true;
  }

  @JProlPredicate(signature = "call/1", args = {
          "+callable"}, reference = "call(G) is true if and only if G represents a goal which is true.")
  public static boolean predicateCALL(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argument = predicate.getElement(0).findNonVarOrSame();

    JProlChoicePoint newChoicePoint = goal.getPayload();
    if (newChoicePoint == null) {
      if (goal.isArgsValidate()) {
        ProlAssertions.assertCallable(argument);
      }

      newChoicePoint = goal.makeForGoal(argument);
      goal.setPayload(newChoicePoint);
    }
    final Term nextResult = newChoicePoint.prove();

    boolean result = false;

    if (nextResult != null) {
      result = assertUnify(argument, nextResult);
      if (newChoicePoint.isCompleted()) {
        goal.cutVariants();
      }
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "once/1", args = {
          "+callable"}, reference = "once(Term) is true. once/1 is not re-executable.")
  public static boolean predicateONCE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argument = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(argument);
    }

    final JProlChoicePoint currentGoal = new JProlChoicePoint(argument, goal.getContext());
    final Term nextResult = currentGoal.prove();

    return nextResult != null;
  }

  @JProlPredicate(changesChooseChain = true, signature = "->/2", reference = "'->'(If, Then) is true if and only if If is true and Then is true for the first solution of If")
  public static boolean predicateIFTHEN(final JProlChoicePoint goal, final TermStruct predicate) {
    // if-then
    final JProlChoicePoint leftSubbranch =
            new JProlChoicePoint(predicate.getElement(0), goal.getContext());
    boolean result = false;
    if (leftSubbranch.prove() != null) {
      // replace current goal by the 'then' goal
      final JProlChoicePoint thenPart = goal.replaceLastGoalAtChain(predicate.getElement(1));
      thenPart.cutLocally(); // remove all previous choice points
      result = true;
    } else {
      goal.cutVariants();
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "var/1", reference = "var(X) is true if and only if X is a variable.")
  public static boolean predicateVAR(final JProlChoicePoint goal, final TermStruct predicate) {
    return predicate.getElement(0).findNonVarOrSame().getTermType() == VAR;
  }

  @JProlPredicate(determined = true, signature = "nonvar/1", reference = "nonvar(X) is true if and only if X is not a variable.")
  public static boolean predicateNONVAR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0);
    if (arg.getTermType() == VAR) {
      return !((TermVar) arg).isFree();
    } else {
      return true;
    }
  }

  @JProlPredicate(determined = true, signature = "atom/1", reference = "atom(X) is true if and only if X is an atom.")
  public static boolean predicateATOM(final JProlChoicePoint goal, final TermStruct predicate) {
    Term arg = predicate.getElement(0);
    if (arg.getTermType() == VAR) {
      arg = ((TermVar) arg).getValue();
      if (arg == null) {
        return false;
      }
    }

    boolean result = false;

    switch (arg.getTermType()) {
      case ATOM: {
        result = !(arg instanceof NumericTerm);
      }
      break;
      case LIST: {
        result = ((TermList) arg).isNullList();
      }
      break;
    }

    return result;
  }

  @JProlPredicate(determined = true, signature = "integer/1", reference = "integer(X) is true if and only if X is an integer.")
  public static boolean predicateINTEGER(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();

    if (arg.getTermType() == ATOM) {
      return arg instanceof TermLong;
    } else {
      return false;
    }
  }

  @JProlPredicate(determined = true, signature = "number/1", reference = "number(X) is true if and only if X is an integer or a float.")
  public static boolean predicateNUMBER(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    return (arg.getTermType() == ATOM) && (arg instanceof NumericTerm);
  }

  @JProlPredicate(determined = true, signature = "float/1", reference = "float(X) is true if and only if X is a float.")
  public static boolean predicateFLOAT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (arg.getTermType() == ATOM) {
      return arg instanceof TermDouble;
    } else {
      return false;
    }
  }

  @JProlPredicate(determined = true, signature = "compound/1", reference = "compound(X) is true if and only if X is a compound term, that is neither atomic nor a variable.")
  public static boolean predicateCOMPOUND(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term atom = predicate.getElement(0).findNonVarOrSame();
    switch (atom.getTermType()) {
      case STRUCT:
        return true;
      case LIST:
        return !((TermList) atom).isNullList();
      default:
        return false;
    }
  }

  @JProlPredicate(determined = true, signature = "atomic/1", reference = "atomic(X) is true if and only if X is atomic (that is an atom, an integer or a float).")
  public static boolean predicateATOMIC(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    boolean result = false;
    switch (arg.getTermType()) {
      case ATOM: {
        result = true;
      }
      break;
      case LIST: {
        result = ((TermList) arg).isNullList();
      }
      break;
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "arg/3", args = {
          "+integer,+compound_term,?term"}, reference = "arg(N,Term, Arg) is true if nad only if the Nth argument of Term is Arg")
  public static boolean predicateARG(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term number = predicate.getElement(0).findNonVarOrSame();
    final Term compound_term = predicate.getElement(1).findNonVarOrSame();
    final Term element = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(number);
      ProlAssertions.assertCompoundTerm(compound_term);
    }

    final long index = number.toNumber().longValue();

    if (index < 0) {
      throw new ProlDomainErrorException("Element number less than zero", number);
    }
    if (index == 0) {
      return false;
    }

    boolean result = false;
    if (compound_term.getTermType() == STRUCT) {
      final TermStruct struct = (TermStruct) compound_term;
      final long elementIndex = index - 1;
      if (elementIndex < struct.getArity()) {
        result = element.unifyTo(struct.getElement((int) elementIndex));
      }
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "functor/3", args = {"-nonvar,+atomic,+arity",
          "+nonvar,?atomic,?arity"}, reference = "functor(Term, Name, Arity) is true if and only if Term is a compound term with functor name Name and arity Arity or Term is an atomic term equal to Name and Arity is 0.")
  public static boolean predicateFUNCTOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argTerm = predicate.getElement(0).findNonVarOrSame();
    final Term argName = predicate.getElement(1).findNonVarOrSame();
    final Term argArity = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (argTerm.getTermType() == VAR) {
        ProlAssertions.assertAtomic(argName);
        ProlAssertions.assertArity(argArity);
      } else {
        if (argName.getTermType() != VAR) {
          ProlAssertions.assertAtomic(argName);
        }
        if (argArity.getTermType() != VAR) {
          ProlAssertions.assertArity(argArity);
        }
      }
    }

    switch (argTerm.getTermType()) {
      case ATOM: {
        final Term arity = newLong(0);
        return argName.unifyTo(argTerm) && argArity.unifyTo(arity);
      }
      case STRUCT: {
        final TermStruct struct = (TermStruct) argTerm;
        final Term functor = newAtom(struct.getFunctor().getText());
        final Term arity = newLong(struct.getArity());
        return argName.unifyTo(functor) && argArity.unifyTo(arity);
      }
      case LIST: {
        final TermList list = (TermList) argTerm;

        Term name;
        Term arity;

        if (list.isNullList()) {
          arity = newLong(0);
          name = NULL_LIST;
        } else {
          arity = newLong(2);
          name = LIST_FUNCTOR;
        }
        return argName.unifyTo(name) && argArity.unifyTo(arity);
      }
      case VAR: {
        final int arity = (int) argArity.toNumber().longValue();
        if (arity < 0) {
          throw new ProlRepresentationErrorException("Wrong arity value", predicate);
        }

        if (argName instanceof NumericTerm) {
          if (arity == 0) {
            return argTerm.unifyTo(argName);
          } else {
            throw new ProlTypeErrorException("atom", predicate);
          }
        }

        Term[] elements = null;

        if (arity > 0) {
          elements = new Term[arity];
          for (int li = 0; li < arity; li++) {
            elements[li] = newVar();
          }
        }

        final TermStruct newStruct = newStruct(argName, elements);

        return argTerm.unifyTo(newStruct);
      }
      default:
        throw new ProlCriticalError("Unexpected type:" + argTerm.getTermType());
    }

  }

  @JProlPredicate(determined = true, signature = "ll2r/2", args = {"+list,+list"},
      reference = "Unify each element of the left list with each element of the right list till first non-processed successful one.")
  public static boolean predicateLL2R(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertList(argLeft);
      ProlAssertions.assertList(argRight);
    }

    final TermList lleft = (TermList) argLeft;
    final TermList lright = (TermList) argRight;

    Set<Integer> freeIndex = new HashSet<>();
    int index = 0;
    for (final Term r : lright) {
      freeIndex.add(index++);
    }

    for (final Term l : lleft) {
      index = 0;
      for (final Term r : lright) {
        if (freeIndex.contains(index)) {
          if (l.unifyTo(r)) {
            freeIndex.remove(index);
            break;
          }
        }
        index++;
      }
      if (freeIndex.isEmpty()) {
        break;
      }
    }
    return freeIndex.isEmpty();
  }

  @JProlPredicate(determined = true, signature = "=../2", args = {"+nonvar,?non_empty_list",
      "-nonvar,+non_empty_list"}, reference = "Term =.. List is true if and only if\n* Term is an atomic term and List is the list whose only element is Term, or\n* Term is a compound term and List is the list whose head is the functor name of Term and whose tail is the list of the arguments of Term. ")
  public static boolean predicateUNIV(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (argLeft.getTermType() == VAR) {
        ProlAssertions.assertNonEmptyList(argRight);
      } else {
        if (argRight.getTermType() != VAR) {
          ProlAssertions.assertNonEmptyList(argRight);
        }
      }
    }

    if (argLeft.getTermType() == STRUCT) {
      if (((TermStruct) argLeft).getArity() == 0) {
        throw new ProlDomainErrorException("compound_non_zero_arity", predicate);
      }
    }

    if (argRight.getTermType() == VAR) {
      TermList list = TermList.makeListFromElementWithSplitStructure(argLeft);
      return argRight.unifyTo(list);
    } else {
      final Term atom = ((TermList) argRight).toAtom();
      if (atom.getTermType() == STRUCT) {
        final TermStruct atomAsStruct = (TermStruct) atom;
        atomAsStruct.setPredicateProcessor(goal.getContext().findProcessor(atomAsStruct));
      }
      return argLeft.unifyTo(atom);
    }
  }

  @JProlPredicate(determined = true, signature = "atom_chars/2", args = {"+atom,?character_list",
          "-atom,+character_list"}, reference = "atom_chars(Atom, List) succeeds if and only if List is a list whose elements are the one character atoms that in order make up  Atom.")
  public static boolean predicateATOMCHARS(final JProlChoicePoint goal,
                                           final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    Term right = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (ProlAssertions.isAtom(left)) {
        if (right.getTermType() != VAR) {
          ProlAssertions.assertCharacterList(right);
        }
      } else {
        ProlAssertions.assertVar(left);
        ProlAssertions.assertCharacterList(right);
        if (!right.isGround()) {
          throw new ProlInstantiationErrorException("List contains non-instantiated vars: " + right,
                  right);
        }
      }
    }

    switch (left.getTermType()) {
      case ATOM: {
        return Utils.toCharList(left).unifyTo(right);
      }
      case LIST: {
        if (((TermList) left).isNullList()) {
          return Utils.toCharList(Terms.newAtom("[]")).unifyTo(right);
        } else {
          throw new ProlTypeErrorException("atom", predicate);
        }
      }
    }

    if (right.getTermType() == LIST) {
      StringBuilder builder = new StringBuilder();

      TermList list = (TermList) right;
      while (!list.isNullList()) {
        final Term head = list.getHead();
        builder.append(head.getText());

        final Term tail = list.getTail();
        if (tail.getTermType() == LIST) {
          list = (TermList) tail;
        } else {
          return false;
        }
      }

      right = newAtom(builder.toString());
      return left.unifyTo(right);
    }

    return false;
  }

  @JProlPredicate(determined = true, signature = "char_code/2", args = {
          "+character,?character_code",
          "-character,+character_code"}, reference = "char_code(Char, Code) succeeds if and only if Code is the character code that corresponds to the character Char.")
  public static boolean predicateCHARCODE(final JProlChoicePoint goal, final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    Term right = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (left.getTermType() == VAR) {
        ProlAssertions.assertCharacterCode(right);
      } else {
        ProlAssertions.assertCharacter(left);
        if (right.getTermType() != VAR) {
          ProlAssertions.assertCharacterCode(right);
        }
      }
    }

    if (left.getTermType() == ATOM) {
      left = newLong((int) left.getText().charAt(0));
      return right.unifyTo(left);
    }

    if (right.getTermType() == ATOM) {
      right = newAtom(Character.toString((char) right.toNumber().shortValue()));
      return left.unifyTo(right);
    }

    return false;
  }

  @JProlPredicate(determined = true, signature = "number_codes/2", args = {
          "+number,?character_code_list",
          "-number,+character_code_list"}, reference = "number_codes(Number, CodeList) succeeds if and only if CodeList is a list whose elements are the codes for the one character atoms that in order make up Number.")
  public static boolean predicateNUMBERCODES(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    final Term right = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (left.getTermType() == VAR) {
        ProlAssertions.assertCharacterCodeList(right);
      } else {
        ProlAssertions.assertNumber(left);
        if (right.getTermType() != VAR) {
          ProlAssertions.assertCharacterCodeList(right);
        }
      }
    }

    if (left.getTermType() == ATOM && right.getTermType() == VAR) {
      return Utils.toCharCodeList(left).unifyTo(right);
    }

    if (right.getTermType() == LIST) {
      final StringBuilder builder = new StringBuilder();

      TermList list = (TermList) right;
      while (!list.isNullList()) {
        final TermLong head = list.getHead();
        builder.append((char) head.toNumber().shortValue());

        final Term tail = list.getTail();
        if (tail.getTermType() == LIST) {
          list = (TermList) tail;
        } else {
          return false;
        }
      }

      final String numberValue = builder.toString();

      Term number;

      try {
        if (numberValue.startsWith("0x")) {
          number = newLong(Long.parseLong(numberValue.substring(2), 16));
        } else {
          number = newLong(numberValue);
        }
      } catch (NumberFormatException ex) {
        try {
          number = newDouble(numberValue);
        } catch (NumberFormatException exx) {
          number = newAtom(numberValue);
        }
      }

      return left.unifyTo(number);
    }

    return false;
  }

  @JProlPredicate(signature = "current_predicate_all/1",
          args = {"?predicate_indicator"},
          reference = "True if PredicateIndicator is a currently defined predicate. It looks for predicates both in knowledge base and attached libraries."
  )
  public static boolean predicateCURRENTPREDICATEALL(final JProlChoicePoint choicePoint,
                                                     final TermStruct predicate) {
    final Term predicateIndicator = predicate.getElement(0).findNonVarOrSame();
    List<TermStruct> list = choicePoint.getPayload();
    if (list == null) {
      if (choicePoint.isArgsValidate() && predicateIndicator.getTermType() != VAR) {
        ProlAssertions.assertIndicator(predicateIndicator);
      }

      list = new ArrayList<>(
              choicePoint.getContext().findAllForPredicateIndicatorInLibs(predicateIndicator));

      try (final CloseableIterator<TermStruct> knowledgeBaseIterator = choicePoint.getContext().getKnowledgeBase().iterateSignatures(
              predicateIndicator.getTermType() == VAR ?
                      Terms.newStruct("/", new Term[]{Terms.newVar(), Terms.newVar()}) :
                      (TermStruct) predicateIndicator)) {
        while (knowledgeBaseIterator.hasNext()) {
          list.add(knowledgeBaseIterator.next());
        }
        list.sort(choicePoint);
        choicePoint.setPayload(list);
      } catch (IOException ex) {
        throw new ProlKnowledgeBaseException(ex);
      }
    }

    if (list.isEmpty()) {
      return false;
    } else {
      return predicateIndicator.unifyTo(list.remove(0));
    }
  }

  @JProlPredicate(signature = "current_predicate/1",
          args = {"?predicate_indicator"},
          reference = "True if PredicateIndicator is a currently defined predicate. It looks for predicates only in current knowledge base."
  )
  public static boolean predicateCURRENTPREDICATE(final JProlChoicePoint choicePoint,
                                                  final TermStruct predicate) {
    final Term predicateIndicator = predicate.getElement(0).findNonVarOrSame();
    List<TermStruct> list = choicePoint.getPayload();
    if (list == null) {
      if (choicePoint.isArgsValidate() && predicateIndicator.getTermType() != VAR) {
        ProlAssertions.assertIndicator(predicateIndicator);
      }

      list = new ArrayList<>();
      try (final CloseableIterator<TermStruct> knowledgeBaseIterator = choicePoint.getContext().getKnowledgeBase().iterateSignatures(
              predicateIndicator.getTermType() == VAR ?
                      Terms.newStruct("/", new Term[]{Terms.newVar(), Terms.newVar()}) :
                      (TermStruct) predicateIndicator)) {
        while (knowledgeBaseIterator.hasNext()) {
          list.add(knowledgeBaseIterator.next());
        }
        list.sort(choicePoint);
        choicePoint.setPayload(list);
      } catch (IOException ex) {
        throw new ProlKnowledgeBaseException(ex);
      }
    }

    if (list.isEmpty()) {
      return false;
    } else {
      return predicateIndicator.unifyTo(list.remove(0));
    }
  }

  @JProlPredicate(signature = "atom_concat/3",
          args = {"?atom,?atom,?atom"},
          reference = "Atom3 forms the concatenation of Atom1 and Atom2.")
  public static boolean predicateATOMCONCAT3(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term atom1 = predicate.getElement(0).findNonVarOrSame();
    final Term atom2 = predicate.getElement(1).findNonVarOrSame();
    final Term atom3 = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (atom1.getTermType() != VAR) {
        ProlAssertions.assertAtom(atom1);
      }
      if (atom2.getTermType() != VAR) {
        ProlAssertions.assertAtom(atom2);
      }
      if (atom3.getTermType() != VAR) {
        ProlAssertions.assertAtom(atom3);
      }
    }

    final int bounded =
            (atom1.isGround() ? 1 : 0) + (atom2.isGround() ? 1 : 0) + (atom3.isGround() ? 2 : 0);

    class AtomConcatState {
      private final StringBuilder seq1;
      private final StringBuilder seq2;

      AtomConcatState(final String text2) {
        this.seq1 = new StringBuilder(text2.length());
        this.seq2 = new StringBuilder(text2);
      }

      boolean next() {
        boolean result = false;
        if (this.seq2.length() > 0) {
          this.seq1.append(this.seq2.charAt(0));
          this.seq2.delete(0, 1);
          result = true;
        }
        return result;
      }

      Term getSeq1AsTerm() {
        return newAtom(this.seq1.toString());
      }

      Term getSeq2AsTerm() {
        return newAtom(this.seq2.toString());
      }
    }

    if (bounded < 2) {
      throw new ProlInstantiationErrorException(predicate);
    } else {
      final AtomConcatState state = goal.getPayload();
      if (state == null) {
        if (atom1.isGround() && atom2.isGround()) {
          goal.cutVariants();
          return atom3.unifyTo(newAtom(atom1.getText() + atom2.getText()));
        } else if (atom1.isGround()) {
          goal.cutVariants();
          final String text1 = atom1.getText();
          final String text3 = atom3.getText();
          if (text3.startsWith(text1)) {
            return atom2.unifyTo(newAtom(text3.substring(text1.length())));
          }
        } else if (atom2.isGround()) {
          goal.cutVariants();
          final String text2 = atom2.getText();
          final String text3 = atom3.getText();
          if (text3.endsWith(text2)) {
            return atom1.unifyTo(newAtom(text3.substring(0, text3.length() - text2.length())));
          }
        } else {
          final String wholeText = atom3.getText();
          final AtomConcatState newState = new AtomConcatState(wholeText);
          goal.setPayload(newState);
          return atom1.unifyTo(newState.getSeq1AsTerm()) && atom2.unifyTo(newState.getSeq2AsTerm());
        }
      } else {
        boolean result = state.next();
        if (result) {
          result = atom1.unifyTo(state.getSeq1AsTerm()) && atom2.unifyTo(state.getSeq2AsTerm());
        } else {
          goal.cutVariants();
        }
        return result;
      }
    }
    return false;
  }


  @JProlPredicate(
          determined = true,
          signature = "number_chars/2",
          args = {"+number,?character_list", "-number,+character_list"},
          reference = "number_chars(Number, List) succeeds if and only if List is a list whose elements are the one character atoms that in order make up Number.")
  public static boolean predicateNUMBERCHARS2(final JProlChoicePoint goal,
                                              final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    final Term right = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (left.getTermType() == VAR) {
        ProlAssertions.assertCharacterList(right);
      } else {
        ProlAssertions.assertNumber(left);
        if (right.getTermType() != VAR) {
          ProlAssertions.assertCharacterList(right);
        }
      }
    }

    final boolean result;
    if (right.getTermType() == LIST) {
      final StringBuilder builder = new StringBuilder();

      TermList list = (TermList) right;
      boolean add = false;
      while (!list.isNullList()) {
        final Term head = list.getHead();

        final char chr = head.getText().charAt(0);
        if (!add) {
          if (!Character.isWhitespace(chr)) {
            add = true;
            builder.append(chr);
          }
        } else {
          builder.append(chr);
        }

        final Term tail = list.getTail();
        if (tail.getTermType() == LIST) {
          list = (TermList) tail;
        } else {
          return false;
        }
      }
      builder.append('.');

      final String text = builder.toString();
      if (goal.isArgsValidate()) {
        for (int i = 0; i < text.length(); i++) {
          final char chr = text.charAt(i);
          if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
            throw new ProlCustomErrorException(Terms.newAtom("syntax_error"), right);
          }
        }
      }

      final Term term;
      try {
        term = new JProlTreeBuilder(goal.getContext())
                .readPhraseAndMakeTree(new StringReader(builder.append('.').toString())).term;
      } catch (PrologParserException ex) {
        throw new ProlCustomErrorException(Terms.newAtom("syntax_error"), right);
      }

      if (term instanceof NumericTerm) {
        result = left.unifyTo(term);
      } else {
        throw new ProlTypeErrorException("number", "Expected numeric term: " + term, term);
      }
    } else if (left.getTermType() == ATOM) {
      result = Utils.toCharList(left).unifyTo(right);
    } else {
      result = false;
    }

    return result;
  }

  @JProlPredicate(signature = "for/3", args = {
          "?term,+integer,+integer"}, reference = "Allows to make integer counter from a variable, (TermVar, Low, High).")
  public static boolean predicateFOR3(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();

    final class For3CounterStorage {
      final long low;
      final long high;
      long value;

      For3CounterStorage(final long low, final long high) {
        this.low = low;
        this.high = high;
        this.value = low;
      }

      boolean isEnd() {
        if (this.low <= this.high) {
          return this.value > high;
        } else {
          return this.value < high;
        }
      }

      void inc() {
        if (this.low <= this.high) {
          this.value++;
        } else {
          this.value--;
        }
      }
    }

    For3CounterStorage counter = choicePoint.getPayload();
    if (counter == null) {
      final Term lowTerm = predicate.getElement(1).findNonVarOrSame();
      final Term highTerm = predicate.getElement(2).findNonVarOrSame();

      if (choicePoint.isArgsValidate()) {
        ProlAssertions.assertInteger(lowTerm);
        ProlAssertions.assertInteger(highTerm);
      }
      final long low = lowTerm.toNumber().longValue();
      final long high = highTerm.toNumber().longValue();

      counter = new For3CounterStorage(low, high);
      choicePoint.setPayload(counter);
    } else {
      counter.inc();
    }
    final long value = counter.value;
    if (counter.isEnd()) {
      choicePoint.cutVariants();
      return false;
    } else {
      return term.unifyTo(Terms.newLong(value));
    }
  }

  @JProlPredicate(determined = true, signature = "rnd/2", args = {"+integer,?integer",
          "+list,?term"}, reference = "Generate pseudo random in 0(included)...limit(excluded) or select random element from the list.")
  public static boolean predicateRND(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term first = predicate.getElement(0).findNonVarOrSame();
    final Term second = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertNonVar(first);
      if (first.getTermType() != LIST) {
        if (second.getTermType() != VAR) {
          ProlAssertions.assertInteger(second);
        }
      }
    }

    final Term result;
    if (first.getTermType() == LIST) {
      final TermList list = (TermList) first;
      if (list.isNullList()) {
        result = Terms.NULL_LIST;
      } else {
        final Term[] array = list.toArray();
        result = array[ThreadLocalRandom.current().nextInt(array.length)];
      }
    } else {
      result = Terms.newLong(ThreadLocalRandom.current().nextLong(first.toNumber().longValue()));
    }
    return second.unifyTo(result);
  }

  @JProlPredicate(determined = true, signature = "atom_length/2", args = {
          "+atom,?integer"}, reference = "atom_length(Atom, Length) is true if and only if the integer Length equals the number of characters in the name of the atom Atom.")
  public static boolean predicateATOMLENGTH(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    final Term right = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(left);
      if (right.getTermType() != VAR) {
        ProlAssertions.assertInteger(right);
      }
    }

    left = newLong(left.getTextLength());
    return left.unifyTo(right);
  }

  @JProlPredicate(determined = true, signature = "atom_codes/2", args = {
          "+atom,?character_code_list",
          "?atom,+character_code_list"}, reference = "atom_codes(Atom, List) succeeds if and only if List is a list whose elements are the character codes that in order correspond to the characters that make up  Atom.")
  public static boolean predicateATOMCHARCODES(final JProlChoicePoint goal,
                                               final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    Term right = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (ProlAssertions.isAtom(left)) {
        if (right.getTermType() != VAR) {
          ProlAssertions.assertCharacterCodeList(right);
        }
      } else {
        if (left.getTermType() != VAR) {
          ProlAssertions.assertAtom(left);
        }
        ProlAssertions.assertCharacterCodeList(right);
      }
    }

    switch (left.getTermType()) {
      case ATOM: {
        return Utils.toCharCodeList(left).unifyTo(right);
      }
      case LIST: {
        if (((TermList) left).isNullList()) {
          return Utils.toCharCodeList(newAtom("[]")).unifyTo(right);
        } else {
          throw new ProlTypeErrorException("atom", predicate);
        }
      }
    }

    if (left.getTermType() == ATOM) {
      return Utils.toCharCodeList(left).unifyTo(right);
    }

    if (right.getTermType() == LIST) {
      final StringBuilder builder = new StringBuilder();

      TermList list = (TermList) right;
      while (!list.isNullList()) {
        final Term head = list.getHead();

        if (!(head instanceof TermLong)) {
          throw new ProlRepresentationErrorException("character_code", predicate);
        }

        builder.append((char) head.toNumber().shortValue());

        final Term tail = list.getTail();
        if (tail.getTermType() == LIST) {
          list = (TermList) tail;
        } else {
          return false;
        }
      }

      right = newAtom(builder.toString());
      return left.unifyTo(right);
    }

    return false;
  }

  @JProlPredicate(determined = true, signature = "dispose/1", synonyms = {"dispose/0"}, args = {
          "+integer"}, reference = " These predicate terminate JProl context even if started through async.")
  public static void predicateDispose(final JProlChoicePoint goal, final TermStruct predicate) {
    if (predicate.getArity() == 0) {
      goal.getContext().dispose();
      throw new ProlHaltExecutionException();
    } else {
      final Term arg = predicate.getElement(0).findNonVarOrSame();
      if (goal.isArgsValidate()) {
        ProlAssertions.assertInteger(arg);
      }
      final long status = arg.toNumber().longValue();
      goal.getContext().dispose();
      throw new ProlHaltExecutionException(status);
    }
  }

  @JProlPredicate(determined = true, signature = "abolish/1", args = {
          "+predicate_indicator"}, reference = "abolish(Pred/2) is true. It has for side effect the removal of all clauses of the predicate indicated by Pred. After abolish/1 the predicate is not found by current_predicate.")
  public static boolean predicateABOLISH1(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertIndicator(arg);
    }
    final String signature = Utils.extractPredicateSignatureFromStructure(arg);

    final KnowledgeBase base = goal.getContext().getKnowledgeBase();
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure",
          "Predicate signature '" + signature + "'is presented in library", newAtom(signature));
    }

    base.abolish(goal.getContext(), signature);
    return true;
  }

  @JProlPredicate(determined = true, signature = "sort/2", args = {
          "+list,?list"}, reference = "True if Sorted can be unified with a list holding the elements of List, sorted to the standard order of terms")
  public static boolean predicateSORT2(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    final Term termList = predicate.getElement(0).findNonVarOrSame();
    final Term termSorted = predicate.getElement(1).findNonVarOrSame();

    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertList(termList);
      if (termSorted.getTermType() != VAR) {
        ProlAssertions.assertList(termSorted);
      }
    }

    if (termSorted.getTermType() == VAR) {
      final Term[] terms = ((TermList) termList).toArray();
      Arrays.sort(terms, choicePoint);
      final TermList sortedList;
      if (terms.length > 1) {
        for (int i = terms.length - 1; i > 0; i--) {
          final Term term = terms[i];
          final Term termPrev = terms[i - 1];
          if (choicePoint.compare(term, termPrev) == 0) {
            terms[i] = null;
          }
        }
        sortedList =
            TermList.asList(
                Arrays.stream(terms).filter(Objects::nonNull).collect(Collectors.toList()));
      } else {
        sortedList = TermList.asList(Arrays.asList(terms));
      }
      return termSorted.unifyTo(sortedList);
    } else {
      return termList.unifyTo(termSorted);
    }
  }

  @JProlPredicate(determined = true, signature = "findall/3", args = {
          "?term,+callable,?list"}, reference = "Creates a list of the instantiations Template gets successively on backtracking over Goal and unifies the result with Bag.")
  public static boolean predicateFINDALL3(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term template = predicate.getElement(0).findNonVarOrSame();
    final Term scopeGoal = predicate.getElement(1).findNonVarOrSame();
    final Term instances = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(scopeGoal);
      if (instances.getTermType() != VAR) {
        ProlAssertions.assertList(instances);
      }
    }

    final JProlChoicePoint find_goal = new JProlChoicePoint(scopeGoal.makeClone(), goal.getContext());

    TermList result = null;
    TermList currentList = null;

    while (true) {
      final Term nextTemplate = find_goal.prove();

      if (nextTemplate == null) {
        break;
      }

      final Term templateCopy = template.makeClone();
      final Term scopeGoalCopy = scopeGoal.makeClone();
      templateCopy.arrangeVariablesInsideTerms(scopeGoalCopy);

      assertUnify(scopeGoalCopy, nextTemplate);
      // good, add to the list
      if (result == null) {
        // first
        result = newList(templateCopy.findNonVarOrSame().makeClone());
        currentList = result;
      } else {
        // not first
        currentList =
                createOrAppendToList(currentList, templateCopy.findNonVarOrSame().makeClone());
      }
    }

    if (result == null) {
      result = NULL_LIST;
    }

    return instances.unifyTo(result);
  }

  @JProlPredicate(signature = "bagof/3", args = {
          "?term,+callable,?list"}, reference = "Unify Bag with the alternatives of Template. If Goal has free variables besides the one sharing with Template, bagof/3 will backtrack over the alternatives of these free variables, unifying Bag with the corresponding alternatives of Template. The construct +TermVar^Goal tells bagof/3 not to bind TermVar in Goal. bagof/3 fails if Goal has no solutions.")
  public static boolean predicateBAGOF(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term template = predicate.getElement(0).findNonVarOrSame();
    final Term scopeGoal = predicate.getElement(1).findNonVarOrSame();
    final Term instances = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(scopeGoal);
      if (instances.getTermType() != VAR) {
        ProlAssertions.assertList(instances);
      }
    }

    final class BofKey {

      private final Map<String, Term> vars;
      private final int hash;

      BofKey(final JProlChoicePoint goal, final Set<String> excludedVariables) {
        final Map<String, Term> varSnapshot = goal.findAllGroundedVars();
        excludedVariables.forEach(varSnapshot::remove);
        final List<String> orderedNames = new ArrayList<>(varSnapshot.keySet());
        Collections.sort(orderedNames);
        this.hash = orderedNames.stream().map(n -> varSnapshot.get(n).getText())
                .collect(Collectors.joining(":")).hashCode();
        this.vars = varSnapshot;
      }

      public void restoreVarValues(final JProlChoicePoint goal) {
        this.vars.keySet().forEach(name -> goal.findVar(name).ifPresent(v -> v.unifyTo(this.vars.get(name))));
      }

      @Override
      public int hashCode() {
        return this.hash;
      }

      @Override
      public boolean equals(final Object that) {
        if (that == this) {
          return true;
        }
        boolean result = false;

        if (that instanceof BofKey && ((BofKey) that).vars.size() == this.vars.size()) {
          final BofKey thatKey = (BofKey) that;
          result = this.vars.entrySet().stream()
                  .allMatch(e -> thatKey.vars.containsKey(e.getKey()) &&
                          thatKey.vars.get(e.getKey()).dryUnifyTo(e.getValue()));
        }
        return result;
      }

    }

    Map<BofKey, TermList> preparedMap = goal.getPayload();

    if (preparedMap == null) {
      preparedMap = new LinkedHashMap<>();

      final Set<String> excludedVars = new HashSet<>(template.allNamedVarsAsMap().keySet());

      Term processingGoal = scopeGoal;
      while (processingGoal.getTermType() == STRUCT
              && ((TermStruct) processingGoal).getArity() == 2
              && "^".equals(((TermStruct) processingGoal).getFunctor().getText())) {

        final TermStruct theStruct = (TermStruct) processingGoal;
        final Term left = theStruct.getElement(0);

        if (left.getTermType() == VAR) {
          excludedVars.add(left.getText());
        } else {
          throw new ProlTypeErrorException("var", "Expected VAR as left side argument", left);
        }

        processingGoal = theStruct.getElement(1);
      }

      final JProlChoicePoint find_goal =
              new JProlChoicePoint(processingGoal.makeClone(), goal.getContext());

      while (true) {
        final Term nextTemplate = find_goal.proveWithFailForUnknown();

        if (nextTemplate == null) {
          break;
        }

        final Term templateCopy = template.makeClone();
        final Term scopeGoalCopy = processingGoal.makeClone();
        templateCopy.arrangeVariablesInsideTerms(scopeGoalCopy);

        assertUnify(scopeGoalCopy, nextTemplate);
        final BofKey key = new BofKey(find_goal, excludedVars);
        final TermList resultList;
        if (preparedMap.containsKey(key)) {
          resultList = preparedMap.get(key);
          createOrAppendToList(resultList, templateCopy.findNonVarOrSame().makeClone());
        } else {
          resultList = newList(templateCopy.findNonVarOrSame().makeClone());
          preparedMap.put(key, resultList);
        }
      }

      goal.setPayload(preparedMap);
    }

    if (preparedMap.isEmpty()) {
      return false;
    } else {
      final BofKey firstKey = preparedMap.keySet().stream().findFirst().get();
      final TermList list = preparedMap.remove(firstKey);
      if (instances.unifyTo(list)) {
        firstKey.restoreVarValues(goal);
        return true;
      } else {
        return false;
      }
    }
  }

  @JProlPredicate(signature = "setof/3", args = {
          "?term,+callable,?list"}, reference = "Equivalent to bagof/3, but sorts the result using sort/2 to get a sorted list of alternatives without duplicates.")
  public static boolean predicateSETOF3(final JProlChoicePoint choicePoint, final TermStruct predicate) {
    final Term template = predicate.getElement(0).findNonVarOrSame();
    final Term scopeGoal = predicate.getElement(1).findNonVarOrSame();
    final Term instances = predicate.getElement(2).findNonVarOrSame();

    if (choicePoint.isArgsValidate()) {
      ProlAssertions.assertCallable(scopeGoal);
      if (instances.getTermType() != VAR) {
        ProlAssertions.assertList(instances);
      }
    }

    final class SofKey {

      private final Map<String, Term> vars;
      private final int hash;

      SofKey(final JProlChoicePoint goal, final Set<String> excludedVariables) {
        final Map<String, Term> varSnapshot = goal.findAllGroundedVars();
        excludedVariables.forEach(varSnapshot::remove);
        final List<String> orderedNames = new ArrayList<>(varSnapshot.keySet());
        Collections.sort(orderedNames);
        this.hash = orderedNames.stream().map(n -> varSnapshot.get(n).getText())
                .collect(Collectors.joining(":")).hashCode();
        this.vars = varSnapshot;
      }

      public void restoreVarValues(final JProlChoicePoint goal) {
        this.vars.keySet().forEach(name -> goal.findVar(name).ifPresent(v -> v.unifyTo(this.vars.get(name))));
      }

      @Override
      public int hashCode() {
        return this.hash;
      }

      @Override
      public boolean equals(final Object that) {
        if (that == this) {
          return true;
        }
        boolean result = false;

        if (that instanceof SofKey && ((SofKey) that).vars.size() == this.vars.size()) {
          final SofKey thatKey = (SofKey) that;
          result = this.vars.entrySet().stream()
                  .allMatch(e -> thatKey.vars.containsKey(e.getKey()) &&
                          thatKey.vars.get(e.getKey()).dryUnifyTo(e.getValue()));
        }
        return result;
      }

    }

    Map<SofKey, TermList> preparedMap = choicePoint.getPayload();

    if (preparedMap == null) {
      preparedMap = new LinkedHashMap<>();

      final Set<String> excludedVars = new HashSet<>(template.allNamedVarsAsMap().keySet());

      Term processingGoal = scopeGoal;
      while (processingGoal.getTermType() == STRUCT
              && ((TermStruct) processingGoal).getArity() == 2
              && "^".equals(((TermStruct) processingGoal).getFunctor().getText())) {

        final TermStruct theStruct = (TermStruct) processingGoal;
        final Term left = theStruct.getElement(0);

        if (left.getTermType() == VAR) {
          excludedVars.add(left.getText());
        } else {
          throw new ProlTypeErrorException("var", "Expected VAR as left side argument", left);
        }

        processingGoal = theStruct.getElement(1);
      }

      final JProlChoicePoint find_goal =
              new JProlChoicePoint(processingGoal.makeClone(), choicePoint.getContext());

      while (true) {
        final Term nextTemplate = find_goal.proveWithFailForUnknown();

        if (nextTemplate == null) {
          break;
        }

        final Term templateCopy = template.makeClone();
        final Term scopeGoalCopy = processingGoal.makeClone();
        templateCopy.arrangeVariablesInsideTerms(scopeGoalCopy);

        assertUnify(scopeGoalCopy, nextTemplate);
        final SofKey key = new SofKey(find_goal, excludedVars);
        final TermList resultList;
        if (preparedMap.containsKey(key)) {
          resultList = preparedMap.get(key);
          Utils.createOrAppendToList(resultList, templateCopy.findNonVarOrSame().makeClone());
        } else {
          resultList = newList(templateCopy.findNonVarOrSame().makeClone());
          preparedMap.put(key, resultList);
        }
      }

      final Map<SofKey, TermList> sortedMap = new LinkedHashMap<>();
      preparedMap.forEach((key, value) -> {
        final Term[] tmpArray = value.toArray();
        Arrays.sort(tmpArray, choicePoint);
        final TermList sortedList = TermList.asList(
            Arrays.stream(tmpArray)
                .distinct().collect(Collectors.toList()));

        sortedMap.put(key, sortedList);
      });

      preparedMap = sortedMap;

      choicePoint.setPayload(preparedMap);
    }

    if (preparedMap.isEmpty()) {
      return false;
    } else {
      final SofKey firstKey = preparedMap.keySet().stream().findFirst().get();
      final TermList list = preparedMap.remove(firstKey);
      if (instances.unifyTo(list)) {
        firstKey.restoreVarValues(choicePoint);
        return true;
      } else {
        return false;
      }
    }
  }

  @JProlPredicate(determined = true, signature = "asserta/1", args = {
          "+callable"}, reference = "Addition of a clause into the knowlwde base before all other clauses.")
  public static boolean predicateASSERTA1(final JProlChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();

    Term termToAdd = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(termToAdd);
    }

    if (termToAdd.getTermType() != STRUCT) {
      termToAdd = newStruct(termToAdd);
    }

    final String signature = ((TermStruct) termToAdd).isClause() ?
            ((TermStruct) termToAdd).getElement(0).getSignature() : termToAdd.getSignature();

    // check that it doesn't overload any static system predicate
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure",
          "Predicate signature '" + signature + "'is presented in library", newAtom(signature));
    }

    base.assertA(goal.getContext(), (TermStruct) termToAdd.makeCloneAndVarBound());
    return true;
  }

  @JProlPredicate(determined = true, signature = "assertz/1", synonyms = {"assert/1"}, args = {
          "+callable"}, reference = "Addition of a clause into the knowlwde base after all other clauses.")
  public static boolean predicateASSERTZ1(final JProlChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();
    Term termToRemove = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(termToRemove);
    }

    if (termToRemove.getTermType() != STRUCT) {
      termToRemove = newStruct(termToRemove);
    }

    final String signature = ((TermStruct) termToRemove).isClause() ?
            ((TermStruct) termToRemove).getElement(0).getSignature() : termToRemove.getSignature();

    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure",
          "Predicate signature '" + signature + "'is presented in library", newAtom(signature));
    }

    base.assertZ(goal.getContext(), (TermStruct) termToRemove.makeCloneAndVarBound());

    return true;
  }

  @JProlPredicate(determined = true, signature = "retract/1", synonyms = {"retracta/1"}, args = {
          "+callable"}, reference = "Retract the first clause which can be unified with argument. True if there is such clause in the knowledge base.")
  public static boolean predicateRETRACT1(final JProlChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();
    Term atom = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(atom);
    }
    if (atom.getTermType() != STRUCT) {
      atom = newStruct(atom);
    }

    final String signature =
            ((TermStruct) atom).isClause() ? ((TermStruct) atom).getElement(0).getSignature() :
                    atom.getSignature();

    // check that it doesn't overload any static system predicate
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure",
          "Predicate signature '" + signature + "'is presented in library", newAtom(signature));
    }

    return base.retractA(goal.getContext(), (TermStruct) atom.makeCloneAndVarBound());
  }

  @JProlPredicate(determined = true, signature = "retractz/1", args = {
          "+callable"}, reference = "Retract the last clause which can be unified with argument. True if there is such clause in the knowledge base.")
  public static boolean predicateRETRACTZ(final JProlChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();
    Term atom = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(atom);
    }

    if (atom.getTermType() != STRUCT) {
      atom = newStruct(atom);
    }

    final String signature =
            ((TermStruct) atom).isClause() ? ((TermStruct) atom).getElement(0).getSignature() :
                    atom.getSignature();

    // check that it doesn't overload any static system predicate
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure",
          "Predicate signature '" + signature + "'is presented in library", newAtom(signature));
    }

    return base.retractZ(goal.getContext(), (TermStruct) atom);
  }

  @JProlPredicate(determined = true, signature = "retractall/1", args = {
          "+callable"}, reference = "Retract all clauses which can be unified with argument. True if there is as minimum one clause in the knowledge base.")
  public static boolean predicateRETRACTALL(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();
    Term atom = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(atom);
    }

    if (atom.getTermType() != STRUCT) {
      atom = newStruct(atom);
    }

    final String signature =
            ((TermStruct) atom).isClause() ? ((TermStruct) atom).getElement(0).getSignature() :
                    atom.getSignature();

    // check that it doesn't overload any static system predicate
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure",
          "Predicate signature '" + signature + "'is presented in library", newAtom(signature));
    }

    return base.retractAll(goal.getContext(), (TermStruct) atom);
  }

  @JProlPredicate(signature = "catch/3", args = "+callable,?term,+callable", reference = "A goal catch(Goal, Catcher, Handler) is true if\n1. call(Goal) is true, or\n2. An exception is raised which throws a Ball that is caught by Catcher and Handler then succeeds ")
  public static boolean predicateCATCH(final JProlChoicePoint goal, final TermStruct predicate) {
    JProlChoicePoint catchGoal = goal.getPayload();

    final Term catching = predicate.getElement(0).findNonVarOrSame();
    final Term catcher = predicate.getElement(1).findNonVarOrSame();
    final Term solver = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(catching);
      ProlAssertions.assertCallable(solver);
    }

    if (catchGoal == null) {
      catchGoal = goal.makeForGoal(catching);
      goal.setPayload(catchGoal);
    }

    if (catchGoal.getGoalTerm() == solver) {
      final Term result = catchGoal.prove();

      if (result == null) {
        goal.cutVariants();
        return false;
      } else {
        if (catchGoal.isCompleted()) {
          goal.cutVariants();
        }
        return true;
      }
    } else {

      try {
        final Term result = catchGoal.prove();
        if (result == null) {
          goal.cutVariants();
          return false;
        } else {
          if (catchGoal.isCompleted()) {
            goal.cutVariants();
          }
          return true;
        }
      } catch (final ProlAbstractCatchableException ex) {
        if (catcher.unifyTo(ex.getAsStruct())) {
          catchGoal = new JProlChoicePoint(solver, goal.getContext());
          goal.setPayload(catchGoal);
          final Term result = catchGoal.prove();
          if (result == null) {
            goal.cutVariants();
            return false;
          } else {
            if (catchGoal.isCompleted()) {
              goal.cutVariants();
            }
            goal.setPayload(catchGoal);
            return true;
          }
        } else {
          return false;
        }
      }
    }
  }

  @JProlPredicate(determined = true, signature = "throw/1", args = "+callable", reference = "Throw an exception which can be caught by catch/3")
  public static void predicateTHROW(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(term);
    }

    final String exceptionSignature = term.getSignature();

    if ("instantiation_error/0".equals(exceptionSignature)) {
      throw new ProlInstantiationErrorException(predicate);
    }

    if ("type_error/2".equals(exceptionSignature)) {
      throw new ProlTypeErrorException(predicate.getElement(0).forWrite(), predicate.getElement(1));
    }

    if ("domain_error/2".equals(exceptionSignature)) {
      throw new ProlDomainErrorException(predicate.getElement(0).forWrite(),
              predicate.getElement(1));
    }

    if ("permission_error/3".equals(exceptionSignature)) {
      throw new ProlPermissionErrorException(predicate.getElement(0).forWrite(),
              predicate.getElement(1).forWrite(), predicate.getElement(2));
    }

    if ("representation_error/1".equals(exceptionSignature)) {
      throw new ProlRepresentationErrorException(predicate.getElement(0).forWrite(), predicate);
    }

    if ("evaluation_error/1".equals(exceptionSignature)) {
      throw new ProlEvaluationErrorException(predicate.getElement(0).forWrite(), predicate);
    }

    // all other errors make as custom
    //-------------------------------------
    Term arg = predicate.getElement(0);
    if (arg.getTermType() != STRUCT) {
      arg = newStruct(arg);
    }
    throw new ProlCustomErrorException(arg, predicate);
  }

  @JProlPredicate(determined = true, signature = "pause/1", args = {
          "+number"}, reference = "Make pause for defined milliseconds.")
  public static void predicatePAUSE(final JProlChoicePoint goal, final TermStruct predicate)
          throws InterruptedException {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertNumber(term);
    }
    final long milliseconds = term.toNumber().longValue();
    if (milliseconds > 0) {
      Thread.sleep(milliseconds);
    }
  }

  @JProlPredicate(determined = true, signature = "regtrigger/3", args = {
          "+predicate_indicator,+atom,+callable"}, reference = "regtrigger(somepredicate/3,onassert,triggerhandler) is always true. The predicate allows to register a trigger handler for distinguished predicate signature. The handled trigger event can be selected from the list [onassert, onretract, onassertretract].")
  public static boolean predicateREGTRIGGER3(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term arg1 = predicate.getElement(0).findNonVarOrSame();
    final Term arg2 = predicate.getElement(1).findNonVarOrSame();
    final Term callableTerm = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertIndicator(arg1);
      ProlAssertions.assertAtom(arg2);
      ProlAssertions.assertCallable(callableTerm);
    }

    final String signature = Utils.extractPredicateSignatureFromStructure(arg1);
    final String triggeringEvent = arg2.getText();
    final JProlContext context = goal.getContext();

    final JProlTriggeringEventObserver deferredTriggeringGoal =
            new JProlTriggeringEventObserver(callableTerm);

    if (triggeringEvent != null) {
      switch (triggeringEvent) {
        case "onassert":
          deferredTriggeringGoal.addSignature(signature, JProlTriggerType.TRIGGER_ASSERT);
          break;
        case "onretract":
          deferredTriggeringGoal.addSignature(signature, JProlTriggerType.TRIGGER_RETRACT);
          break;
        case "onassertretract":
          deferredTriggeringGoal.addSignature(signature, JProlTriggerType.TRIGGER_ASSERT_RETRACT);
          break;
        default:
          throw new ProlCriticalError(
                  "Unsupported trigger event detected [" + triggeringEvent + ']');
      }
    }

    context.registerTrigger(deferredTriggeringGoal);

    return true;
  }

  @JProlPredicate(determined = true, signature = "copy_term/2", args = {
          "?term,?term"}, reference = "copy_term(X,Y) is true if and only if Y unifies with a term T which is a renamed copy of X.")
  public boolean predicateCOPYTERM2(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term in = predicate.getElement(0).findNonVarOrSame().makeClone();
    final Term out = predicate.getElement(1).findNonVarOrSame();
    return in.unifyTo(out);
  }

  @JProlPredicate(determined = true, signature = "\\+/1", args = "+callable", reference = "\\+(Term) is true if and only if call(Term) is false.")
  public boolean predicateCannotBeProven1(final JProlChoicePoint goal,
                                          final TermStruct predicate) {
    final Term argument = predicate.getElement(0).findNonVarOrSame();
    if (goal.isArgsValidate()) {
      ProlAssertions.assertCallable(argument);
    }
    final JProlChoicePoint subGoal = new JProlChoicePoint(argument, goal.getContext());
    return subGoal.proveWithFailForUnknown() == null;
  }
}
