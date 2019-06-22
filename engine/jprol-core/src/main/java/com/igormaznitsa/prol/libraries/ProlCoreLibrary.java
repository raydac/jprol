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

package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.annotations.*;
import com.igormaznitsa.prol.containers.ClauseIterator;
import com.igormaznitsa.prol.containers.ClauseIteratorType;
import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.*;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerGoal;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.utils.Utils;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

import static com.igormaznitsa.prol.data.TermType.*;
import static com.igormaznitsa.prol.data.Terms.*;
import static com.igormaznitsa.prol.utils.Utils.createOrAppendToList;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.*;

@ProlOperators(Operators = {
    //------------------------
    @ProlOperator(Priority = 0, Type = XFX, Name = "("),
    @ProlOperator(Priority = 0, Type = XFX, Name = ")"),
    @ProlOperator(Priority = 0, Type = XFX, Name = "["),
    @ProlOperator(Priority = 0, Type = XFX, Name = "]"),
    @ProlOperator(Priority = 1200, Type = XF, Name = "."),
    @ProlOperator(Priority = 1200, Type = XFX, Name = "|"),
    //------------------------
    @ProlOperator(Priority = 700, Type = XFX, Name = "is"),
    @ProlOperator(Priority = 700, Type = XFX, Name = "="),
    @ProlOperator(Priority = 700, Type = XFX, Name = "\\="),
    @ProlOperator(Priority = 1000, Type = XFY, Name = ","),
    @ProlOperator(Priority = 1050, Type = XFY, Name = "->"),
    @ProlOperator(Priority = 1100, Type = XFY, Name = ";"),
    @ProlOperator(Priority = 1200, Type = FX, Name = "?-"),
    @ProlOperator(Priority = 1200, Type = FX, Name = ":-"),
    @ProlOperator(Priority = 1200, Type = XFX, Name = ":-"),
    @ProlOperator(Priority = 900, Type = FY, Name = "\\+"),
    @ProlOperator(Priority = 700, Type = XFX, Name = ">"),
    @ProlOperator(Priority = 700, Type = XFX, Name = "<"),
    @ProlOperator(Priority = 700, Type = XFX, Name = "=<"),
    @ProlOperator(Priority = 700, Type = XFX, Name = ">="),
    @ProlOperator(Priority = 700, Type = XFX, Name = "=="),
    @ProlOperator(Priority = 700, Type = XFX, Name = "=\\="),
    @ProlOperator(Priority = 700, Type = XFX, Name = "\\=="),
    @ProlOperator(Priority = 700, Type = XFX, Name = "@<"),
    @ProlOperator(Priority = 700, Type = XFX, Name = "@>"),
    @ProlOperator(Priority = 700, Type = XFX, Name = "@=<"),
    @ProlOperator(Priority = 700, Type = XFX, Name = "@>="),
    @ProlOperator(Priority = 700, Type = XFX, Name = "=:="),
    @ProlOperator(Priority = 700, Type = XFX, Name = "=.."),
    @ProlOperator(Priority = 500, Type = YFX, Name = "/\\"),
    @ProlOperator(Priority = 500, Type = YFX, Name = "\\/"),
    @ProlOperator(Priority = 500, Type = YFX, Name = "+"),
    @ProlOperator(Priority = 500, Type = YFX, Name = "-"),
    @ProlOperator(Priority = 500, Type = FX, Name = "not"),
    @ProlOperator(Priority = 500, Type = FX, Name = "+"),
    @ProlOperator(Priority = 500, Type = FX, Name = "-"),
    @ProlOperator(Priority = 400, Type = YFX, Name = "*"),
    @ProlOperator(Priority = 400, Type = YFX, Name = "/"),
    @ProlOperator(Priority = 400, Type = YFX, Name = "//"),
    @ProlOperator(Priority = 400, Type = YFX, Name = "rem"),
    @ProlOperator(Priority = 400, Type = YFX, Name = "<<"),
    @ProlOperator(Priority = 400, Type = YFX, Name = ">>"),
    @ProlOperator(Priority = 300, Type = XFX, Name = "mod"),
    @ProlOperator(Priority = 200, Type = FY, Name = "\\"),
    @ProlOperator(Priority = 200, Type = XFX, Name = "**"),
    @ProlOperator(Priority = 200, Type = XFY, Name = "^")
})
public final class ProlCoreLibrary extends AbstractProlLibrary {

  private static final Random RANDOMIZEGEN = new Random(System.nanoTime());
  private static final Term TRUE = newAtom("true");

  public ProlCoreLibrary() {
    super("prol-core-lib");
  }

  @Predicate(Signature = "=:=/2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic Equal")
  @Determined
  public static boolean predicateArithEqu(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.compare(right) == 0;
  }

  @Predicate(Signature = "@</2", Template = {"?term,?term"}, Reference = "Term less than")
  @Determined
  public static boolean predicateTermLess(final ChoicePoint goal, final TermStruct predicate) {
    return predicate.getElement(0).compareTermTo(predicate.getElement(1)) < 0;
  }

  @Predicate(Signature = "@=</2", Template = {"?term,?term"}, Reference = "Term less than or equal to.")
  @Determined
  public static boolean predicateTermLessOrEqu(final ChoicePoint goal, final TermStruct predicate) {
    return predicate.getElement(0).compareTermTo(predicate.getElement(1)) <= 0;
  }

  @Predicate(Signature = "@>/2", Template = {"?term,?term"}, Reference = "Term greater than")
  @Determined
  public static boolean predicateTermMore(final ChoicePoint goal, final TermStruct predicate) {
    return predicate.getElement(0).compareTermTo(predicate.getElement(1)) > 0;
  }

  @Predicate(Signature = "@>=/2", Template = {"?term,?term"}, Reference = "Term greater than or equal to.")
  @Determined
  public static boolean predicateTermMoreOrEqu(final ChoicePoint goal, final TermStruct predicate) {
    return predicate.getElement(0).compareTermTo(predicate.getElement(1)) >= 0;
  }

  @Predicate(Signature = "==/2", Template = {"?term,?term"}, Reference = "Term identical")
  @Determined
  public static boolean predicateTermEqu(final ChoicePoint goal, final TermStruct predicate) {
    return predicate.getElement(0).compareTermTo(predicate.getElement(1)) == 0;
  }

  @Predicate(Signature = "\\==/2", Template = {"?term,?term"}, Reference = "Term not identical")
  @Determined
  public static boolean predicateNotTermEqu(final ChoicePoint goal, final TermStruct predicate) {
    return predicate.getElement(0).compareTermTo(predicate.getElement(1)) != 0;
  }

  @Predicate(Signature = ">/2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic greater than")
  @Determined
  public static boolean predicateArithMore(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.compare(right) > 0;
  }

  @Predicate(Signature = "</2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic less than")
  @Determined
  public static boolean predicateArithLess(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.compare(right) < 0;
  }

  @Predicate(Signature = ">=/2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic greater than or equal to")
  @Determined
  public static boolean predicateArithMoreOrEqu(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.compare(right) >= 0;
  }

  @Predicate(Signature = "=</2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic less than or equal to")
  @Determined
  public static boolean predicateArithLessOrEqu(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.compare(right) <= 0;
  }

  @Predicate(Signature = "=\\=/2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic Not equal")
  @Determined
  public static boolean predicateArithNotEqu(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.compare(right) != 0;
  }

  @Predicate(Signature = "xor/2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise `exclusive or'")
  @Evaluable
  public static Term predicateXOR(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    if (left instanceof TermLong && right instanceof TermLong) {
      final long lft = left.toNumber().longValue();
      final long rght = right.toNumber().longValue();

      return newLong(lft ^ rght);
    } else {
      throw new ProlInstantiationErrorException("Both arguments must be integer", predicate);
    }
  }

  @Predicate(Signature = "\\/1", Template = {"+evaluable"}, Reference = "Bitwise 'not'")
  @Evaluable
  public static Term predicateBITWISENOT(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));

    if (left instanceof TermLong) {
      final long lft = left.toNumber().longValue();

      return newLong(~lft);
    } else {
      throw new ProlInstantiationErrorException("Argument must be integer", predicate);
    }
  }

  @Predicate(Signature = "\\//2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise 'or'")
  @Evaluable
  public static Term predicateBITWISEOR(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    if (left instanceof TermLong && right instanceof TermLong) {
      final long lft = left.toNumber().longValue();
      final long rght = right.toNumber().longValue();

      return newLong(lft | rght);
    } else {
      throw new ProlInstantiationErrorException("Both arguments must be integer", predicate);
    }
  }

  @Predicate(Signature = "/\\/2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise 'and'")
  @Evaluable
  public static Term predicateBITWISEAND(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    if (left instanceof TermLong && right instanceof TermLong) {
      final long lft = left.toNumber().longValue();
      final long rght = right.toNumber().longValue();

      return newLong(lft & rght);
    } else {
      throw new ProlInstantiationErrorException("Both arguments must be integer", predicate);
    }
  }

  @Predicate(Signature = "mod/2", Template = {"+evaluable,+evaluable"}, Reference = "Modulus")
  @Evaluable
  public static Term predicateMOD(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    final long leftval = left.toNumber().longValue();
    final long rightval = right.toNumber().longValue();

    return newLong(leftval % rightval);
  }

  @Predicate(Signature = "rem/2", Template = {"+evaluable,+evaluable"}, Reference = "Remainder")
  @Evaluable
  public static Term predicateREM(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    final long leftval = left.toNumber().longValue();
    final long rightval = right.toNumber().longValue();

    return newLong(leftval - (leftval / rightval) * rightval);
  }

  @Predicate(Signature = "**/2", Template = {"+evaluable,+evaluable"}, Reference = "Power")
  @Evaluable
  public static Term predicatePOWER(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    final double leftval = left.toNumber().doubleValue();
    final double rightval = right.toNumber().doubleValue();

    return newDouble(Math.pow(leftval, rightval));
  }

  @Predicate(Signature = "+/2", Template = {"+evaluable,+evaluable"}, Reference = "Addition")
  @Evaluable
  public static Term predicateADDTWO(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.add(right);
  }

  @Predicate(Signature = "sin/1", Template = {"+evaluable"}, Reference = "Sine")
  @Evaluable
  public static Term predicateSIN(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.sin(value));
  }

  @Predicate(Signature = "float_integer_part/1", Template = {"+evaluable"}, Reference = "Integer part")
  @Evaluable
  public static Term predicateFLOATINTEGERPART(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final long value = arg.toNumber().longValue();
    return newLong(value);
  }

  @Predicate(Signature = "float_fractional_part/1", Template = {"+evaluable"}, Reference = "Fractional part")
  @Evaluable
  public static Term predicateFLOATFRACTIONALPART(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    final long valueInt = (long) value;
    return newDouble(value - (double) valueInt);
  }

  @Predicate(Signature = "floor/1", Template = {"+evaluable"}, Reference = "Floor")
  @Evaluable
  public static Term predicateFLOOR(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newLong((long) Math.floor(value));
  }

  @Predicate(Signature = "truncate/1", Template = {"+evaluable"}, Reference = "Truncate")
  @Evaluable
  public static Term predicateTRUNCATE(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newLong(value < 0 ? (long) Math.ceil(value) : (long) Math.floor(value));
  }

  @Predicate(Signature = "round/1", Template = {"+evaluable"}, Reference = "Round")
  @Evaluable
  public static Term predicateROUND(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newLong(Math.round(value));
  }

  @Predicate(Signature = "ceiling/1", Template = {"+evaluable"}, Reference = "Ceiling")
  @Evaluable
  public static Term predicateCEILING(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newLong((long) Math.ceil(value));
  }

  @Predicate(Signature = "cos/1", Template = {"+evaluable"}, Reference = "Cosine")
  @Evaluable
  public static Term predicateCOS(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.cos(value));
  }

  @Predicate(Signature = "atan/1", Template = {"+evaluable"}, Reference = "Arc tangent")
  @Evaluable
  public static Term predicateATAN(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.atan(value));
  }

  @Predicate(Signature = "exp/1", Template = {"+evaluable"}, Reference = "Exponentiation")
  @Evaluable
  public static Term predicateEXP(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.exp(value));
  }

  @Predicate(Signature = "log/1", Template = {"+evaluable"}, Reference = "Log")
  @Evaluable
  public static Term predicateLOG(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.log(value));
  }

  @Predicate(Signature = "sqrt/1", Template = {"+evaluable"}, Reference = "Square root")
  @Evaluable
  public static Term predicateSQRT(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    final double value = arg.toNumber().doubleValue();
    return newDouble(Math.sqrt(value));
  }

  @Predicate(Signature = "abs/1", Template = {"+evaluable"}, Reference = "Absolute value")
  @Evaluable
  public static Term predicateABS(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    return arg.abs();
  }

  @Predicate(Signature = "sign/1", Template = {"+evaluable"}, Reference = "SIGN")
  @Evaluable
  public static Term predicateSIGN(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
    return arg.sign();
  }

  @Predicate(Signature = "-/2", Template = {"+evaluable,+evaluable"}, Reference = "Subtraction")
  @Evaluable
  public static Term predicateSUBTWO(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.sub(right);
  }

  @Predicate(Signature = "-/1", Template = {"+evaluable"}, Reference = "Negation")
  @Evaluable
  public static Term predicateNeg(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm val = calculatEvaluable(goal, predicate.getElement(0));
    return val.neg();
  }

  @Predicate(Signature = "+/1", Template = {"+evaluable"}, Reference = "Not action over a number")
  @Evaluable
  public static Term predicateTheSame(final ChoicePoint goal, final TermStruct predicate) {
    return calculatEvaluable(goal, predicate.getElement(0));
  }

  @Predicate(Signature = "*/2", Template = {"+evaluable,+evaluable"}, Reference = "Multiplication")
  @Evaluable
  public static Term predicateMUL(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.mul(right);
  }

  @Predicate(Signature = "//2", Template = {"+evaluable,+evaluable"}, Reference = "Dividsion")
  @Evaluable
  public static Term predicateDIV(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    return left.div(right);
  }

  @Predicate(Signature = "///2", Template = {"+evaluable,+evaluable"}, Reference = "Integer division")
  @Evaluable
  public static Term predicateINTDIV(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    Term result = left.div(right);
    if (result instanceof TermDouble) {
      result = newLong(result.toNumber().longValue());
    }

    return result;
  }

  @Predicate(Signature = "<</2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise left shift")
  @Evaluable
  public static Term predicateSHIFTLEFT(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    final long value = left.toNumber().longValue();
    final long shift = right.toNumber().longValue();

    return newLong(value << shift);
  }

  @Predicate(Signature = ">>/2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise right shift")
  @Evaluable
  public static Term predicateSHIFTRIGHT(final ChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
    final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

    final long value = left.toNumber().longValue();
    final long shift = right.toNumber().longValue();

    return newLong(value >> shift);
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

  @Predicate(Signature = "repeat/0", Reference = "repeat is true. It just places a choice point every call.")
  public static void predicateREPEAT(final ChoicePoint goal, final TermStruct predicate) {
    // we just make a choose point
  }

  @Predicate(Signature = "clause/2", Template = {"+head,?callable_term"}, Reference = "clause(Head, Body) is true if and only if\n* The predicate of Head is public (the standard does not specify how a predicate is declared public but dynamic predicates are public, and\n* There is a clause in the database which corresponds to a term H:- B which unifies with Head :- Body.")
  public static boolean predicateCLAUSE(final ChoicePoint goal, final TermStruct predicate) {
    final Term head = predicate.getElement(0).findNonVarOrSame();
    final Term body = predicate.getElement(1).findNonVarOrSame();

    final TermStruct struct = head.getTermType() == STRUCT ? (TermStruct) head : newStruct(head);
    if (goal.getContext().findProcessor(struct) != PredicateProcessor.NULL_PROCESSOR) {
      throw new ProlPermissionErrorException("access", "private_procedure", predicate);
    }

    ClauseIterator clIterator = goal.getPayload();

    if (clIterator == null) {
      clIterator = goal.getContext().getKnowledgeBase().getClauseIterator(ClauseIteratorType.ANY, head.getTermType() == STRUCT ? (TermStruct) head : newStruct(head));
      if (clIterator == null || !clIterator.hasNext()) {
        goal.resetVariants();
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
        bodyClause = TRUE;
      }

      if (headClause.dryUnifyTo(head) && bodyClause.dryUnifyTo(body)) {
        if (!(headClause.unifyTo(head) && bodyClause.unifyTo(body))) {
          throw new ProlCriticalError("Impossible state at clause/2 #982342");
        }
        return true;
      }
    }
    goal.resetVariants();
    return false;
  }

  //@Predicate(Signature = "current_op/3", Template = "?integer,?operator_specifier,?atom", Reference = "current_op(Priority, Op_specifier, TermOperator) is true if and only if TermOperator is an operator with properties given by  Op_specifier and Priority")
  //@SuppressWarnings("unchecked")
  //public static boolean predicateCURRENTOP(final ChoicePoint goal, final TermStruct predicate) {
  //  final Term priority = predicate.getElement(0).findNonVarOrSame();
  //  final Term specifier = predicate.getElement(1).findNonVarOrSame();
  //  final Term name = predicate.getElement(2).findNonVarOrSame();
  //
  //  Object[] auxObject = goal.getPayload();
  //  if (auxObject == null) {
  //    // the first call
  //    final Iterator<TermOperatorContainer> operator_iterator = goal.getContext().getKnowledgeBase().getOperatorIterator();
  //    auxObject = new Object[] {operator_iterator, null, null};
  //    goal.setPayload(auxObject);
  //  }
  //
  //  final Iterator<TermOperatorContainer> operator_iterator = (Iterator<TermOperatorContainer>) auxObject[0];
  //  TermOperatorContainer last_container = (TermOperatorContainer) auxObject[1];
  //  TermOperator last_operator = (TermOperator) auxObject[2];
  //
  //  final String opNameVal = name.getTermType() == ATOM ? name.getText() : null; // null = any
  //  final int typeVal = specifier.getTermType() == ATOM ? TermOperator.getTypeFromString(specifier.getText()) : -1; // -1 = any
  //  long priorityVal = 0; // 0 - any
  //  if (priority.getTermType() == ATOM) {
  //    priorityVal = priority.toNumber().longValue();
  //    if (priorityVal < 1L || priorityVal > 1200L) {
  //      throw new ProlDomainErrorException("Unsupported operator priority", predicate);
  //    }
  //  }
  //
  //  while (!Thread.currentThread().isInterrupted()) {
  //    if (last_container == null) {
  //      // find container
  //      while (operator_iterator.hasNext()) {
  //        last_container = operator_iterator.next();
  //
  //        if (opNameVal != null) {
  //          if (last_container.getText().equals(opNameVal)) {
  //            break;
  //          }
  //        } else {
  //          break;
  //        }
  //      }
  //
  //      if (last_container == null) {
  //        // there are not more variants
  //        goal.resetVariants();
  //        goal.setPayload(null);
  //        return false;
  //      }
  //    }
  //
  //    // find operator
  //    if (typeVal < 0) {
  //      // find all
  //      final OpAssoc typestart = last_operator == null ? 0 : last_operator.getOperatorType() + 1;
  //
  //      for (int li = typestart; li < 7; li++) {
  //        last_operator = last_container.getForTypePrecisely(li);
  //        if (last_operator != null) {
  //          break;
  //        }
  //      }
  //    } else {
  //      final TermOperator op = last_container.getForTypePrecisely(typeVal);
  //      if (op == last_operator) {
  //        last_operator = null;
  //      }
  //    }
  //
  //    if (last_operator != null) {
  //      if (priorityVal > 0) {
  //        if (last_operator.getPriority() != priorityVal) {
  //          continue;
  //        }
  //      }
  //    } else {
  //      last_container = null;
  //      continue;
  //    }
  //
  //    // we have found an operator
  //    auxObject[1] = last_container;
  //    auxObject[2] = last_operator;
  //
  //    final Term priorityOfFound = newLong(last_operator.getPriority());
  //    final Term specifierOfFound = newAtom(last_operator.getTypeAsString());
  //    final Term nameOfFound = newAtom(last_operator.getText());
  //
  //    if (!(predicate.getElement(0).unifyTo(priorityOfFound) && predicate.getElement(1).unifyTo(specifierOfFound) && predicate.getElement(2).unifyTo(nameOfFound))) {
  //      goal.resetVariants();
  //      goal.setPayload(null);
  //      return false;
  //    } else {
  //      return true;
  //    }
  //  }
  //
  //  return false;
  //}

  @Predicate(Signature = "op/3", Template = "+integer,+operator_specifier,@atom_or_atom_list", Reference = "These predicates allow the operator table to be altered or inspected.\nop(Priority, Op_Specifier, TermOperator) is true, with the side effect that\n1. if Priority is 0 then TermOperator is removed from the operator table, else\n2. TermOperator is added to the TermOperator table, with priority (lower binds tighter) Priority and associativity determined by Op_Specifier")
  @Determined
  public static boolean predicateOP(final ChoicePoint goal, final TermStruct predicate) {
    final int priority = predicate.getElement(0).findNonVarOrSame().toNumber().intValue();
    final String specifier = predicate.getElement(1).findNonVarOrSame().getText();
    final Term atomOrList = predicate.getElement(2).findNonVarOrSame();

    if (priority < 0L || priority > 1200L) {
      throw new ProlDomainErrorException("Priority must be between 0 and 1200 inclusive", predicate);
    }

    OpAssoc opType = OpAssoc.findForName(specifier)
        .orElseThrow(() -> new ProlDomainErrorException("Wrong operator specifier", predicate));

    final ArrayList<String> names = new ArrayList<>();
    if (atomOrList.getTermType() == LIST) {
      TermList list = (TermList) atomOrList;
      while (!list.isNullList()) {
        Term atom = list.getHead();
        if ((atom instanceof NumericTerm) || atom.getTermType() != ATOM) {
          throw new ProlDomainErrorException("Impossible operator name", predicate);
        }
        names.add(atom.getText());

        atom = list.getTail();
        if (atom.getTermType() != LIST) {
          throw new ProlDomainErrorException("Unsuppoerted atom list format", predicate);
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
        names.forEach((name) -> base.addOperator(goal.getContext(), new TermOperator(priority, opType, name)));
      }
    } catch (SecurityException ex) {
      throw new ProlPermissionErrorException("create", "operator", "Attemption to override or remove a system operator", predicate);
    }
    return true;
  }

  @Predicate(Signature = "call/1", Template = {"+callable_term"}, Reference = "call(G) is true if and only if G represents a goal which is true.")
  public static boolean predicateCALL(final ChoicePoint goal, final TermStruct predicate) {
    ChoicePoint currentgoal = goal.getPayload();
    final Term argument = predicate.getElement(0).findNonVarOrSame();

    if (currentgoal == null) {
      // first call of the goal
      currentgoal = new ChoicePoint(argument, goal.getContext());
      goal.setPayload(currentgoal);
    }

    final Term nextResult = currentgoal.next();

    boolean result = false;

    if (nextResult != null) {
      if (!argument.unifyTo(nextResult)) {
        throw new ProlCriticalError("Can't make equ for result of CALL");
      }
      if (currentgoal.hasVariants()) {
        goal.resetVariants();
      }

      result = true;
    }

    return result;
  }

  @Predicate(Signature = "once/1", Template = {"+callable_term"}, Reference = "once(Term) is true. once/1 is not re-executable.")
  @Determined
  public static boolean predicateONCE(final ChoicePoint goal, final TermStruct predicate) {
    final Term argument = predicate.getElement(0).findNonVarOrSame();
    final ChoicePoint currentgoal = new ChoicePoint(argument, goal.getContext());

    final Term nextResult = currentgoal.next();

    return nextResult != null;
  }

  @Predicate(Signature = ";/2", Reference = "';'(Either, Or) is true if either Either or Or is true.")
  public static void predicateOR(final ChoicePoint goal, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @Predicate(Signature = ",/2", Reference = "','(First, Second) is true if and only if First is true and Second is true.")
  public static void predicateAND(final ChoicePoint goal, final TermStruct predicate) {
    // stub, see Goal#resolve
  }

  @Predicate(Signature = "->/2", Reference = "'->'(If, Then) is true if and only if If is true and Then is true for the first solution of If")
  @ChangesChoosePointChain
  public static boolean predicateIFTHEN(final ChoicePoint goal, final TermStruct predicate) {
    // if-then
    final ChoicePoint leftSubbranch = new ChoicePoint(predicate.getElement(0), goal.getContext());
    boolean result = false;
    if (leftSubbranch.next() != null) {
      // replace current goal by the 'then' goal
      final ChoicePoint thenPart = goal.replaceLastGoalAtChain(predicate.getElement(1));
      thenPart.cutLocally(); // remove all previous choice points
      result = true;
    } else {
      goal.resetVariants();
    }
    return result;
  }

  @Predicate(Signature = "var/1", Reference = "var(X) is true if and only if X is a variable.")
  @Determined
  public static boolean predicateVAR(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0);
    if (arg.getTermType() == VAR) {
      return ((TermVar) arg).isFree();
    } else {
      return false;
    }
  }

  @Predicate(Signature = "nonvar/1", Reference = "nonvar(X) is true if and only if X is not a variable.")
  @Determined
  public static boolean predicateNONVAR(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0);
    if (arg.getTermType() == VAR) {
      return !((TermVar) arg).isFree();
    } else {
      return true;
    }
  }

  @Predicate(Signature = "atom/1", Reference = "atom(X) is true if and only if X is an atom.")
  @Determined
  public static boolean predicateATOM(final ChoicePoint goal, final TermStruct predicate) {
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

  @Predicate(Signature = "integer/1", Reference = "integer(X) is true if and only if X is an integer.")
  @Determined
  public static boolean predicateINTEGER(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();

    if (arg.getTermType() == ATOM) {
      return arg instanceof TermLong;
    } else {
      return false;
    }
  }

  @Predicate(Signature = "number/1", Reference = "number(X) is true if and only if X is an integer or a float.")
  @Determined
  public static boolean predicateNUMBER(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    return (arg.getTermType() == ATOM) && (arg instanceof NumericTerm);
  }

  @Predicate(Signature = "float/1", Reference = "float(X) is true if and only if X is a float.")
  @Determined
  public static boolean predicateFLOAT(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    if (arg.getTermType() == ATOM) {
      return arg instanceof TermDouble;
    } else {
      return false;
    }
  }

  @Predicate(Signature = "compound/1", Reference = "compound(X) is true if and only if X is a compound term, that is neither atomic nor a variable.")
  @Determined
  public static boolean predicateCOMPOUND(final ChoicePoint goal, final TermStruct predicate) {
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

  @Predicate(Signature = "atomic/1", Reference = "atomic(X) is true if and only if X is atomic (that is an atom, an integer or a float).")
  @Determined
  public static boolean predicateATOMIC(final ChoicePoint goal, final TermStruct predicate) {
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

  @Predicate(Signature = "arg/3", Template = {"+integer,+compound_term,?term"}, Reference = "arg(N,Term, Arg) is true if nad only if the Nth argument of Term is Arg")
  @Determined
  public static boolean predicateARG(final ChoicePoint goal, final TermStruct predicate) {
    final TermLong number = predicate.getElement(0).findNonVarOrSame();
    final Term compound_term = predicate.getElement(1).findNonVarOrSame();
    final Term element = predicate.getElement(2).findNonVarOrSame();

    final long index = number.toNumber().longValue();

    if (index < 0) {
      throw new ProlDomainErrorException("Element number less than zero", number);
    }
    if (index == 0) {
      return false;
    }

    if (compound_term.getTermType() == STRUCT) {
      final TermStruct struct = (TermStruct) compound_term;
      final long elementIndex = index - 1;
      if (elementIndex >= struct.getArity()) {
        return false;
      }
      return element.unifyTo(struct.getElement((int) elementIndex));
    } else {
      return false;
    }
  }

  @Predicate(Signature = "functor/3", Template = {"-nonvar,+atomic,+integer", "+nonvar,?atomic,?integer"}, Reference = "functor(Term, Name, Arity) is true if and only if Term is a compound term with functor name Name and arity Arity or Term is an atomic term equal to Name and Arity is 0.")
  @Determined
  public static boolean predicateFUNCTOR(final ChoicePoint goal, final TermStruct predicate) {
    final Term argTerm = predicate.getElement(0).findNonVarOrSame();
    final Term argName = predicate.getElement(1).findNonVarOrSame();
    final Term argArity = predicate.getElement(2).findNonVarOrSame();

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
        throw new ProlCriticalError("Unsupported type found!");
    }

  }

  @Predicate(Signature = "=../2", Template = {"+nonvar,?non_empty_list", "-nonvar,+non_empty_list"}, Reference = "Term =.. List is true if and only if\n* Term is an atomic term and List is the list whose only element is Term, or\n* Term is a compound term and List is the list whose head is the functor name of Term and whose tail is the list of the arguments of Term. ")
  @Determined
  public static boolean predicateUNIV(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (argRight.getTermType() != VAR) {
      final Term atom = ((TermList) argRight).toAtom();
      if (atom.getTermType() == STRUCT) {
        ((TermStruct) atom).setPredicateProcessor(goal.getContext().findProcessor((TermStruct) atom));
      }
      return argLeft.unifyTo(atom);
    } else {
      TermList lst = TermList.asTermList(argLeft);
      return argRight.unifyTo(lst);
    }
  }

  private static NumericTerm calculatEvaluable(final ChoicePoint goal, Term term) {
    try {
      if (term.getTermType() == VAR) {
        final TermVar varoriginal = (TermVar) term;
        term = ((TermVar) term).getValue();
        if (term == null) {
          throw new ProlInstantiationErrorException("An empty variable [" + varoriginal + "] found at [" + goal + ']', varoriginal);
        }
      }

      switch (term.getTermType()) {
        case ATOM: {
          if (term instanceof NumericTerm) {
            return (NumericTerm) term;
          } else {
            throw new ProlTypeErrorException("number", "Not a numeric atom +[" + term + "] found at goal [" + goal + ']', term);
          }
        }
        case STRUCT: {
          final PredicateProcessor processor = ((TermStruct) term).getPredicateProcessor();
          if (processor.isEvaluable()) {
            return (NumericTerm) processor.executeEvaluable(goal, (TermStruct) term);
          } else {
            throw new ProlTypeErrorException("evaluable", "Not an arithmetic operator found [" + goal.toString() + ']', term);
          }
        }
        default:
          throw new ProlTypeErrorException("evaluable", "Unsupported atom at an arithmetic expression [" + goal.toString() + ']', term);
      }
    } catch (ArithmeticException ex) {
      throw new ProlEvaluationErrorException(ex.getMessage(), "Arithmetic exception", term, ex);
    }
  }

  @Predicate(Signature = "atom_chars/2", Template = {"+atom,?list", "-atom,+character_list"}, Reference = "atom_chars(Atom, List) succeeds if and only if List is a list whose elements are the one character atoms that in order make up  Atom.")
  @Determined
  public static boolean predicateATOMCHARS(final ChoicePoint goal, final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    Term right = predicate.getElement(1).findNonVarOrSame();

    switch (left.getTermType()) {
      case ATOM: {
        left = left.toCharList();
        return left.unifyTo(right);
      }
      case LIST: {
        if (((TermList) left).isNullList()) {
          left = newAtom("[]").toCharList();
          return left.unifyTo(right);
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

  @Predicate(Signature = "char_code/2", Template = {"+character,?character_code", "-character,+character_code"}, Reference = "char_code(Char, Code) succeeds if and only if Code is the character code that corresponds to the character Char.")
  @Determined
  public static boolean predicateCHARCODE(final ChoicePoint goal, final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    Term right = predicate.getElement(1).findNonVarOrSame();

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

  @Predicate(Signature = "number_codes/2", Template = {"+number,?character_code_list", "-number,+character_code_list"}, Reference = "number_codes(Number, CodeList) succeeds if and only if CodeList is a list whose elements are the codes for the one character atoms that in order make up Number.")
  @Determined
  public static boolean predicateNUMBERCODES(final ChoicePoint goal, final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    final Term right = predicate.getElement(1).findNonVarOrSame();

    if (left.getTermType() == ATOM && right.getTermType() == VAR) {
      left = left.toCharCodeList();
      return left.unifyTo(right);
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

  @Predicate(Signature = "current_predicate_all/1",
      Template = {"?predicate_indicator"},
      Reference = "True if PredicateIndicator is a currently defined predicate. It looks for predicates both in knowledge base and attached libraries."
  )
  public static boolean predicateCURRENTPREDICATEALL(final ChoicePoint goal, final TermStruct predicate) {
    final Term predicateIndicator = predicate.getElement(0).findNonVarOrSame();
    List<TermStruct> list = goal.getPayload();
    if (list == null) {
      list = new ArrayList<>(goal.getContext().findAllForPredicateIndicatorInLibs(predicateIndicator));
      list.addAll(goal.getContext()
          .getKnowledgeBase()
          .findAllForPredicateIndicator(
              predicateIndicator.getTermType() == VAR ? null : (TermStruct) predicateIndicator
          ));
      list.sort(TermStruct::compareTermTo);
      goal.setPayload(list);
    }

    if (list.isEmpty()) {
      return false;
    } else {
      return predicateIndicator.unifyTo(list.remove(0));
    }
  }

  @Predicate(Signature = "current_predicate/1",
      Template = {"?predicate_indicator"},
      Reference = "True if PredicateIndicator is a currently defined predicate. It looks for predicates only in current knowledge base."
  )
  public static boolean predicateCURRENTPREDICATE(final ChoicePoint goal, final TermStruct predicate) {
    final Term predicateIndicator = predicate.getElement(0).findNonVarOrSame();
    List<TermStruct> list = goal.getPayload();
    if (list == null) {
      list = goal.getContext()
          .getKnowledgeBase()
          .findAllForPredicateIndicator(
              predicateIndicator.getTermType() == VAR ? null : (TermStruct) predicateIndicator
          );
      list.sort(TermStruct::compareTermTo);
      goal.setPayload(list);
    }

    if (list.isEmpty()) {
      return false;
    } else {
      return predicateIndicator.unifyTo(list.remove(0));
    }
  }

  @Predicate(Signature = "atom_concat/3",
      Template = {"?atom,?atom,?atom"},
      Reference = "Atom3 forms the concatenation of Atom1 and Atom2.")
  public static boolean predicateATOMCONCAT(final ChoicePoint goal, final TermStruct predicate) {
    final Term atom1 = predicate.getElement(0).findNonVarOrSame();
    final Term atom2 = predicate.getElement(1).findNonVarOrSame();
    final Term atom3 = predicate.getElement(2).findNonVarOrSame();

    final int bounded = (atom1.isGround() ? 1 : 0) + (atom2.isGround() ? 1 : 0) + (atom3.isGround() ? 2 : 0);

    class AtomConcatState {
      final StringBuilder seq1;
      final StringBuilder seq2;

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
          goal.resetVariants();
          return atom3.unifyTo(newAtom(atom1.getText() + atom2.getText()));
        } else if (atom1.isGround()) {
          goal.resetVariants();
          final String text1 = atom1.getText();
          final String text3 = atom3.getText();
          if (text3.startsWith(text1)) {
            return atom2.unifyTo(newAtom(text3.substring(text1.length())));
          }
        } else if (atom2.isGround()) {
          goal.resetVariants();
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
          goal.resetVariants();
        }
        return result;
      }
    }
    return false;
  }


  @Predicate(Signature = "number_chars/2",
      Template = {"+number,?character_list", "-number,+character_list"},
      Reference = "number_chars(Number, List) succeeds if and only if List is a list whose elements are the one character atoms that in order make up Number.")
  @Determined
  public static boolean predicateNUMBERCHARS(final ChoicePoint goal, final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    final Term right = predicate.getElement(1).findNonVarOrSame();

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

      Term number;

      final String numberValue = builder.toString();

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
          throw new ProlCustomErrorException(newStruct(newAtom("syntax_error"), new Term[] {newAtom(numberValue)}), predicate);
        }
      }

      return left.unifyTo(number);
    }

    if (left.getTermType() == ATOM) {
      left = left.toCharList();
      return left.unifyTo(right);
    }

    return false;
  }

  @Predicate(Signature = "for/3", Template = {"?term,+integer,+integer"}, Reference = "Allows to make an integer counter from a variable, (TermVar, Start, End).")
  public static boolean predicateFOR(final ChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0);
    if (term.getTermType() != VAR) {
      goal.resetVariants();
      return false;
    }

    final TermVar var = predicate.getElement(0);
    final long start = predicate.getElement(1).findNonVarOrSame().toNumber().longValue();
    final long limit = predicate.getElement(2).findNonVarOrSame().toNumber().longValue();

    AtomicLong currentIndex = goal.getPayload();

    boolean result = true;
    if (currentIndex == null) {
      currentIndex = new AtomicLong(start);
      var.changeVarChainValue(newLong(start));
      goal.setPayload(currentIndex);
    } else {
      if (currentIndex.incrementAndGet() > limit) {
        goal.resetVariants();
        result = false;
      }
    }

    return result;
  }

  @Predicate(Signature = "rnd/2", Template = {"+integer,?integer", "+list,?term"}, Reference = "Allows to generate a pseudo randomize integer (limit,value) between 0 (inclusive) and the limit (exclusive) or select random element from the list.")
  @Determined
  public static boolean predicateRND(final ChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();

    if (term.getTermType() == LIST) {

      final TermList list = (TermList) term;

      final Term result;
      if (list.isNullList()) {
        result = Terms.NULL_LIST;
      } else {
        // calculate length of the list
        final Term[] array = list.toArray();
        result = array[RANDOMIZEGEN.nextInt(array.length)];
      }
      return predicate.getElement(1).unifyTo(result);

    } else {
      final long limit = predicate.getElement(0).findNonVarOrSame().toNumber().longValue();

      final TermLong genVal = newLong(Math.round(RANDOMIZEGEN.nextDouble() * limit));
      return predicate.getElement(1).unifyTo(genVal);
    }
  }

  @Predicate(Signature = "atom_length/2", Template = {"+atom,?integer"}, Reference = "atom_length(Atom, Length) is true if and only if the integer Length equals the number of characters in the name of the atom Atom.")
  @Determined
  public static boolean predicateATOMLENGTH(final ChoicePoint goal, final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    final Term right = predicate.getElement(1).findNonVarOrSame();

    left = newLong(left.getTextLength());
    return left.unifyTo(right);
  }

  @Predicate(Signature = "atom_codes/2", Template = {"+atom,?character_code_list", "?atom,+list"}, Reference = "atom_codes(Atom, List) succeeds if and only if List is a list whose elements are the character codes that in order correspond to the characters that make up  Atom.")
  @Determined
  public static boolean predicateATOMCHARCODES(final ChoicePoint goal, final TermStruct predicate) {
    Term left = predicate.getElement(0).findNonVarOrSame();
    Term right = predicate.getElement(1).findNonVarOrSame();

    switch (left.getTermType()) {
      case ATOM: {
        left = left.toCharCodeList();
        return left.unifyTo(right);
      }
      case LIST: {
        if (((TermList) left).isNullList()) {
          left = newAtom("[]").toCharCodeList();
          return left.unifyTo(right);
        } else {
          throw new ProlTypeErrorException("atom", predicate);
        }
      }
    }

    if (left.getTermType() == ATOM) {
      left = left.toCharCodeList();

      return left.unifyTo(right);
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

  @Predicate(Signature = "dispose/1", Template = {"+integer"}, Reference = " These predicate terminate a Prolog engine and you can send the status of a cause.")
  @PredicateSynonyms(Signatures = {"dispose/0"})
  @Determined
  public static void predicateHALT(final ChoicePoint goal, final TermStruct predicate) {
    if (predicate.getArity() == 0) {
      goal.getContext().dispose();
      throw new ProlHaltExecutionException();
    } else {
      final long status = predicate.getElement(0).findNonVarOrSame().toNumber().longValue();
      goal.getContext().dispose();
      throw new ProlHaltExecutionException(status);
    }
  }

  @Predicate(Signature = "abolish/1", Template = {"@predicate_indicator"}, Reference = "abolish(Pred/2) is true. It has for side effect the removal of all clauses of the predicate indicated by Pred. After abolish/1 the predicate is not found by current_predicate.")
  @Determined
  public static boolean predicateABOLISH(final ChoicePoint goal, final TermStruct predicate) {
    final String signature = Utils.extractPredicateSignatureFromStructure(predicate.getElement(0));
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();

    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure", newAtom(signature));
    }

    base.abolish(goal.getContext(), signature);
    return true;
  }

  @Predicate(Signature = "sort/2", Template = {"+list,?list"}, Reference = "True if Sorted can be unified with a list holding the elements of List, sorted to the standard order of terms")
  @Determined
  public static boolean predicateSORT(final ChoicePoint goal, final TermStruct predicate) {
    final Term nonsorted = predicate.getElement(0).findNonVarOrSame();
    final Term sorted = predicate.getElement(1).findNonVarOrSame();

    final Term[] bufferarray = ((TermList) nonsorted).toArray();
    Arrays.sort(bufferarray, Utils.TERM_COMPARATOR);
    final TermList sortedList = TermList.asTermList(bufferarray);

    return sorted.unifyTo(sortedList);
  }

  @Predicate(Signature = "findall/3", Template = {"?term,+callable_term,?list"}, Reference = "Creates  a list of the instantiations Template gets  successively on backtracking  over Goal and unifies the  result with Bag.")
  @Determined
  public static boolean predicateFINDALL(final ChoicePoint goal, final TermStruct predicate) {
    final Term template = predicate.getElement(0).findNonVarOrSame();
    final Term pgoal = predicate.getElement(1).findNonVarOrSame();
    final Term instances = predicate.getElement(2).findNonVarOrSame();

    final ChoicePoint find_goal = new ChoicePoint(pgoal.makeClone(), goal.getContext());

    TermList result = null;
    TermList currentList = null;

    while (!Thread.currentThread().isInterrupted()) {
      final Term nextTemplate = find_goal.next();

      if (nextTemplate == null) {
        break;
      }

      final Term templateCopy = template.makeClone();
      final Term pgoalCopy = pgoal.makeClone();
      templateCopy.arrangeVariablesInsideTerms(pgoalCopy);

      if (pgoalCopy.unifyTo(nextTemplate)) {
        // good, add to the list
        if (result == null) {
          // first
          result = newList(templateCopy.findNonVarOrSame().makeClone());
          currentList = result;
        } else {
          // not first
          currentList = createOrAppendToList(currentList, templateCopy.findNonVarOrSame().makeClone());
        }
      } else {
        throw new ProlCriticalError("Impossible situation at findall/3!");
      }
    }

    if (result == null) {
      result = NULL_LIST;
    }

    return instances.unifyTo(result);
  }

  @Predicate(Signature = "bagof/3", Template = {"?term,+callable_term,?list"}, Reference = "Unify Bag with the alternatives of Template. If Goal has free variables besides the one sharing with Template, bagof/3 will backtrack over the alternatives of these free variables, unifying Bag with the corresponding alternatives of Template. The construct +TermVar^Goal tells bagof/3 not to bind TermVar in Goal. bagof/3 fails if Goal has no solutions.")
  public static boolean predicateBAGOF(final ChoicePoint goal, final TermStruct predicate) {

    final class BofKey {

      private final Map<String, Term> vars;
      private final int hash;

      BofKey(final ChoicePoint goal, final Set<String> excludedVariables) {
        final Map<String, Term> varSnapshot = goal.findAllGroundedVars();
        excludedVariables.forEach(varSnapshot::remove);
        final List<String> orderedNames = new ArrayList<>(varSnapshot.keySet());
        Collections.sort(orderedNames);
        this.hash = orderedNames.stream().map(n -> varSnapshot.get(n).getText()).collect(Collectors.joining(":")).hashCode();
        this.vars = varSnapshot;
      }

      public void restoreVarValues(final ChoicePoint goal) {
        this.vars.keySet().forEach(name -> {
          final TermVar thatvar = goal.getVarForName(name);
          if (thatvar != null) {
            thatvar.unifyTo(this.vars.get(name));
          }
        });
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
              .allMatch(e -> thatKey.vars.containsKey(e.getKey()) && thatKey.vars.get(e.getKey()).dryUnifyTo(e.getValue()));
        }
        return result;
      }

    }

    final Term template = predicate.getElement(0).findNonVarOrSame();
    final Term pgoal = predicate.getElement(1).findNonVarOrSame();
    final Term instances = predicate.getElement(2).findNonVarOrSame();

    Map<BofKey, TermList> preparedMap = goal.getPayload();

    if (preparedMap == null) {
      preparedMap = new LinkedHashMap<>();

      final Set<String> excludedVars = new HashSet<>(template.allNamedVarsAsMap().keySet());

      Term processingGoal = pgoal;
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

      final ChoicePoint find_goal = new ChoicePoint(processingGoal.makeClone(), goal.getContext());

      while (!Thread.currentThread().isInterrupted()) {
        final Term nextTemplate = find_goal.next();

        if (nextTemplate == null) {
          break;
        }

        final Term templateCopy = template.makeClone();
        final Term pgoalCopy = processingGoal.makeClone();
        templateCopy.arrangeVariablesInsideTerms(pgoalCopy);

        if (pgoalCopy.unifyTo(nextTemplate)) {
          final BofKey thekey = new BofKey(find_goal, excludedVars);
          final TermList resultList;
          if (preparedMap.containsKey(thekey)) {
            resultList = preparedMap.get(thekey);
            createOrAppendToList(resultList, templateCopy.findNonVarOrSame().makeClone());
          } else {
            resultList = newList(templateCopy.findNonVarOrSame().makeClone());
            preparedMap.put(thekey, resultList);
          }
        } else {
          throw new ProlCriticalError("Impossible situation at findall/3!");
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

  @Predicate(Signature = "setof/3", Template = {"?term,+callable_term,?list"}, Reference = "Equivalent to bagof/3, but sorts the result using sort/2 to get a sorted list of alternatives without duplicates.")
  public static boolean predicateSETOF(final ChoicePoint goal, final TermStruct predicate) {

    final class SofKey {

      private final Map<String, Term> vars;
      private final int hash;

      SofKey(final ChoicePoint goal, final Set<String> excludedVariables) {
        final Map<String, Term> varSnapshot = goal.findAllGroundedVars();
        excludedVariables.forEach(varSnapshot::remove);
        final List<String> orderedNames = new ArrayList<>(varSnapshot.keySet());
        Collections.sort(orderedNames);
        this.hash = orderedNames.stream().map(n -> varSnapshot.get(n).getText()).collect(Collectors.joining(":")).hashCode();
        this.vars = varSnapshot;
      }

      public void restoreVarValues(final ChoicePoint goal) {
        this.vars.keySet().forEach(name -> {
          final TermVar thatvar = goal.getVarForName(name);
          if (thatvar != null) {
            thatvar.unifyTo(this.vars.get(name));
          }
        });
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
              .allMatch(e -> thatKey.vars.containsKey(e.getKey()) && thatKey.vars.get(e.getKey()).dryUnifyTo(e.getValue()));
        }
        return result;
      }

    }

    final Term template = predicate.getElement(0).findNonVarOrSame();
    final Term pgoal = predicate.getElement(1).findNonVarOrSame();
    final Term instances = predicate.getElement(2).findNonVarOrSame();

    Map<SofKey, TermList> preparedMap = goal.getPayload();

    if (preparedMap == null) {
      preparedMap = new LinkedHashMap<>();

      final Set<String> excludedVars = new HashSet<>(template.allNamedVarsAsMap().keySet());

      Term processingGoal = pgoal;
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

      final ChoicePoint find_goal = new ChoicePoint(processingGoal.makeClone(), goal.getContext());

      while (!Thread.currentThread().isInterrupted()) {
        final Term nextTemplate = find_goal.next();

        if (nextTemplate == null) {
          break;
        }

        final Term templateCopy = template.makeClone();
        final Term pgoalCopy = processingGoal.makeClone();
        templateCopy.arrangeVariablesInsideTerms(pgoalCopy);

        if (pgoalCopy.unifyTo(nextTemplate)) {
          final SofKey thekey = new SofKey(find_goal, excludedVars);
          final TermList resultList;
          if (preparedMap.containsKey(thekey)) {
            resultList = preparedMap.get(thekey);
            Utils.createOrAppendToList(resultList, templateCopy.findNonVarOrSame().makeClone());
          } else {
            resultList = newList(templateCopy.findNonVarOrSame().makeClone());
            preparedMap.put(thekey, resultList);
          }
        } else {
          throw new ProlCriticalError("Impossible situation at findall/3!");
        }
      }

      final Map<SofKey, TermList> sortedMap = new LinkedHashMap<>();
      preparedMap.forEach((key, value) -> {
        final Term[] tmpArray = value.toArray();
        Arrays.sort(tmpArray, Utils.TERM_COMPARATOR);
        final TermList sortedList = TermList.asTermList(
            Arrays.stream(tmpArray)
                .distinct().toArray(Term[]::new)
        );

        sortedMap.put(key, sortedList);
      });

      preparedMap = sortedMap;

      goal.setPayload(preparedMap);
    }

    if (preparedMap.isEmpty()) {
      return false;
    } else {
      final SofKey firstKey = preparedMap.keySet().stream().findFirst().get();
      final TermList list = preparedMap.remove(firstKey);
      if (instances.unifyTo(list)) {
        firstKey.restoreVarValues(goal);
        return true;
      } else {
        return false;
      }
    }
  }

  @Predicate(Signature = "asserta/1", Template = {"@clause"}, Reference = "Addition of a clause into the knowlwde base before all other clauses.")
  @Determined
  public static boolean predicateASSERTA(final ChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();

    Term atom = predicate.getElement(0).findNonVarOrSame();

    if (atom.getTermType() != STRUCT) {
      atom = newStruct(atom);
    }

    final String signature = ((TermStruct) atom).isClause() ? ((TermStruct) atom).getElement(0).getSignature() : atom.getSignature();

    // check that we doesn't overload any static system predicate
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure", newAtom(signature));
    }

    base.assertA(goal.getContext(), (TermStruct) atom.makeCloneAndVarBound());
    return true;
  }

  @Predicate(Signature = "assertz/1", Template = {"@clause"}, Reference = "Addition of a clause into the knowlwde base after all other clauses.")
  @PredicateSynonyms(Signatures = "assert/1")
  @Determined
  public static boolean predicateASSERTZ(final ChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();
    Term atom = predicate.getElement(0).findNonVarOrSame();

    if (atom.getTermType() != STRUCT) {
      atom = newStruct(atom);
    }

    final String signature = ((TermStruct) atom).isClause() ? ((TermStruct) atom).getElement(0).getSignature() : atom.getSignature();

    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure", newAtom(signature));
    }

    base.assertZ(goal.getContext(), (TermStruct) atom.makeCloneAndVarBound());

    return true;
  }

  @Predicate(Signature = "retract/1", Template = {"@clause"}, Reference = "Retract the first clause which can be unified with argument. True if there is such clause in the knowledge base.")
  @PredicateSynonyms(Signatures = "retracta/1")
  @Determined
  public static boolean predicateRETRACT(final ChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();

    Term atom = predicate.getElement(0).findNonVarOrSame();

    if (atom.getTermType() != STRUCT) {
      atom = newStruct(atom);
    }

    final String signature = ((TermStruct) atom).isClause() ? ((TermStruct) atom).getElement(0).getSignature() : atom.getSignature();

    // check that we doesn't overload any static system predicate
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure", newAtom(signature));
    }

    return base.retractA(goal.getContext(), (TermStruct) atom.makeCloneAndVarBound());
  }

  @Predicate(Signature = "retractz/1", Template = {"@clause"}, Reference = "Retract the last clause which can be unified with argument. True if there is such clause in the knowledge base.")
  @Determined
  public static boolean predicateRETRACTZ(final ChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();

    Term atom = predicate.getElement(0).findNonVarOrSame();

    if (atom.getTermType() != STRUCT) {
      atom = newStruct(atom);
    }

    final String signature = ((TermStruct) atom).isClause() ? ((TermStruct) atom).getElement(0).getSignature() : atom.getSignature();

    // check that we doesn't overload any static system predicate
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure", newAtom(signature));
    }

    return base.retractZ(goal.getContext(), (TermStruct) atom);
  }

  @Predicate(Signature = "retractall/1", Template = {"@clause"}, Reference = "Retract all clauses which can be unified with argument. True if there is as minimum one clause in the knowledge base.")
  @Determined
  public static boolean predicateRETRACTALL(final ChoicePoint goal, final TermStruct predicate) {
    final KnowledgeBase base = goal.getContext().getKnowledgeBase();
    Term atom = predicate.getElement(0).findNonVarOrSame();

    if (atom.getTermType() != STRUCT) {
      atom = newStruct(atom);
    }

    final String signature = ((TermStruct) atom).isClause() ? ((TermStruct) atom).getElement(0).getSignature() : atom.getSignature();

    // check that we doesn't overload any static system predicate
    if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
      throw new ProlPermissionErrorException("modify", "static_procedure", newAtom(signature));
    }

    return base.retractAll(goal.getContext(), (TermStruct) atom);
  }

  @Predicate(Signature = "catch/3", Template = "+callable_term,?term,+callable_term", Reference = "A goal catch(Goal, Catcher, Handler) is true if\n1. call(Goal) is true, or\n2. An exception is raised which throws a Ball that is caught by Catcher and Handler then succeeds ")
  public static boolean predicateCATCH(final ChoicePoint goal, final TermStruct predicate) {
    ChoicePoint catchGoal = goal.getPayload();

    final Term catching = predicate.getElement(0).findNonVarOrSame();
    final Term catcher = predicate.getElement(1).findNonVarOrSame();
    final Term solver = predicate.getElement(2).findNonVarOrSame();

    if (catchGoal == null) {
      catchGoal = new ChoicePoint(catching, goal.getContext());
      goal.setPayload(catchGoal);
    }

    if (catchGoal.getGoalTerm() == solver) {
      final Term result = catchGoal.next();

      if (result == null) {
        goal.resetVariants();
        return false;
      } else {
        if (catchGoal.hasVariants()) {
          goal.resetVariants();
        }
        return true;
      }
    } else {

      try {
        final Term result = catchGoal.next();
        if (result == null) {
          goal.resetVariants();
          return false;
        } else {
          if (catchGoal.hasVariants()) {
            goal.resetVariants();
          }
          return true;
        }
      } catch (ProlAbstractCatcheableException ex) {
        // exception was thrown
        if (catcher.unifyTo(ex.getAsStruct())) {
          catchGoal = new ChoicePoint(solver, goal.getContext());
          goal.setPayload(catchGoal);
          final Term result = catchGoal.next();
          if (result == null) {
            goal.resetVariants();
            return false;
          } else {
            if (catchGoal.hasVariants()) {
              goal.resetVariants();
            }
            goal.setPayload(catchGoal);
            return true;
          }
        } else {
          throw ex;
        }
      }
    }
  }

  @Predicate(Signature = "throw/1", Template = "+callable_term", Reference = "Throw an exception which can be catched by catch/3")
  @Determined
  public static void predicateTHROW(final ChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    final String exceptionSignature = term.getSignature();

    if ("instantiation_error/0".equals(exceptionSignature)) {
      throw new ProlInstantiationErrorException(predicate);
    }

    if ("type_error/2".equals(exceptionSignature)) {
      throw new ProlTypeErrorException(predicate.getElement(0).forWrite(), predicate.getElement(1));
    }

    if ("domain_error/2".equals(exceptionSignature)) {
      throw new ProlDomainErrorException(predicate.getElement(0).forWrite(), predicate.getElement(1));
    }

    if ("permission_error/3".equals(exceptionSignature)) {
      throw new ProlPermissionErrorException(predicate.getElement(0).forWrite(), predicate.getElement(1).forWrite(), predicate.getElement(2));
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

  @Predicate(Signature = "pause/1", Template = {"+number"}, Reference = "Make a pause for defined millisecond number.")
  @Determined
  public static void predicatePAUSE(final ChoicePoint goal, final TermStruct predicate) throws InterruptedException {
    final NumericTerm term = predicate.getElement(0);

    final long milliseconds = term.toNumber().longValue();
    if (milliseconds > 0) {
      Thread.sleep(milliseconds);
    }
  }

  // inside function used by facts/1 and rules/1 predicates
  private static TermStruct processIterator(final ChoicePoint goal, final ClauseIterator iterator) {
    TermStruct result = null;

    if (iterator.hasNext()) {
      result = iterator.next();
    } else {
      goal.resetVariants();
    }

    return result;
  }

  @Predicate(Signature = "facts/1", Template = {"+callable_term"}, Reference = "Finds only facts at the knowledge base.")
  public static boolean predicateFACTS(final ChoicePoint goal, final TermStruct predicate) {
    ClauseIterator factIterator = goal.getPayload();
    final Term origterm = predicate.getElement(0).findNonVarOrSame();

    if (factIterator == null) {
      Term term = origterm;
      if (term.getTermType() == ATOM) {
        term = newStruct(term);
      }

      final KnowledgeBase base = goal.getContext().getKnowledgeBase();
      factIterator = base.getClauseIterator(ClauseIteratorType.FACTS, (TermStruct) term);

      if (factIterator == null) {
        goal.resetVariants();
        return false;
      } else {
        goal.setPayload(factIterator);
      }
    }

    boolean result = false;

    final TermStruct nextFact = processIterator(goal, factIterator);
    if (nextFact == null) {
      goal.setPayload(null);
      goal.resetVariants();
    } else if (origterm.getTermType() == ATOM) {
      result = true;
    } else {
      if (!origterm.unifyTo(nextFact)) {
        throw new ProlCriticalError("Critical error, not unifyTo!");
      }
      result = true;
    }

    return result;
  }

  @Predicate(Signature = "lock/1", Template = {"+atom"}, Reference = "Block current thread until it will be possible to lock an atom, don't forget unlock.")
  @Determined
  public static void predicateLOCK(final ChoicePoint goal, final TermStruct predicate) {
    final String atomName = predicate.getElement(0).getText();
    goal.getContext().lockLockerForName(atomName);
  }

  @Predicate(Signature = "unlock/1", Template = {"+atom"}, Reference = "Unlock a locker for its name and allow to continue work of waiting threads. If any other thread is the owner for the locker then permission_error/3 will be thrown.")
  @Determined
  public static void predicateUNLOCK(final ChoicePoint goal, final TermStruct predicate) {
    final String atomName = predicate.getElement(0).getText();

    try {
      goal.getContext().unlockLockerForName(atomName);
    } catch (IllegalArgumentException ex) {
      final ProlExistenceErrorException exx = new ProlExistenceErrorException("locker", "unlock", predicate, ex);
      throw exx;
    } catch (IllegalMonitorStateException ex) {
      final ProlPermissionErrorException exx = new ProlPermissionErrorException("locker", "unlock", predicate, ex);
      throw exx;
    }
  }

  @Predicate(Signature = "trylock/1", Template = {"+atom"}, Reference = "Try make lock for a named locker, if it is being locked already then fail else success.")
  @Determined
  public static boolean predicateTRYLOCK(final ChoicePoint goal, final TermStruct predicate) {
    final String atomName = predicate.getElement(0).getText();
    return goal.getContext().trylockLockerForName(atomName);
  }

  @Predicate(Signature = "async/1", Template = {"+callable_term"}, Reference = "Allows to next a goal asynchronously, it will be started as a daemon so it will be stopped when the main goal will be solved or failed. If there will be uncatched exception it will be just out at the log.")
  @Determined
  public static void predicateASYNC(final ChoicePoint goal, final TermStruct predicate) {
    final Term goalToSolve = predicate.getElement(0).findNonVarOrSame();
    final ProlContext context = goal.getContext();

    // we have to check the goal because it must not have any noninstantiated variable!!!!
    final Map<String, TermVar> vars = goalToSolve.allNamedVarsAsMap();
    for (TermVar var : vars.values()) {
      if (!var.isGround()) {
        throw new ProlInstantiationErrorException("Variable \'" + var.getText() + "\' is not instantiated, you must have all variables instantiated for async/1 .", predicate);
      }
    }

    context.submitAsync(goalToSolve);
  }

  @Predicate(Signature = "waitasync/0", Reference = "Blocking waiting until all daemon threads (started with either fork/1 or async/1) of the context will be done.")
  @Determined
  public static void predicateWAITASYNC(final ChoicePoint goal, final TermStruct predicate) {
    final ExecutorService service = goal.getContext().getContextExecutorService();

    goal.getContext().waitAllAsyncDone();

    if (Thread.currentThread().isInterrupted()) {
      service.shutdown();
      throw new ProlForkExecutionException("Execution interrupted", predicate, null);
    }
  }

  private static List<Future<Term>> startListAsFork(final ChoicePoint goal, final TermStruct predicate, final TermList termlist) {
    List<AuxForkTask> goalList = goal.getPayload();

    if (goalList == null) {
      Set<Integer> varFlagTable = null; // the lazy initing map allows us to find out that there are non instantiated shared variables

      // the first call
      goalList = new ArrayList<>();
      TermList tlist = termlist;
      final ProlContext context = goal.getContext();
      while (!tlist.isNullList()) {
        final Term term = tlist.getHead();

        // ---- we have to check that user doesn't try share noninstantiated variables between parallel goals
        if (term.getTermType() != ATOM) {
          // find vars
          final Map<String, TermVar> varTable = term.allNamedVarsAsMap();
          if (!varTable.isEmpty()) {
            if (varFlagTable == null) {
              varFlagTable = new HashSet<>();
            }
            for (final Map.Entry<String, TermVar> pair : varTable.entrySet()) {
              final TermVar variable = pair.getValue();
              if (!variable.isAnonymous() && !variable.isGround()) {
                // we check only undefined vars
                final Integer varUID = variable.getVarUid();
                if (varFlagTable.contains(varUID)) {
                  // we have such var in one from other terms, that is impossible for concurrent execution, all vars must be instantiated
                  throw new ProlInstantiationErrorException("Variable \'" + variable.getText() + "\' is being shared between one or more parallel solving goals but not instantiated.", predicate);
                } else {
                  varFlagTable.add(varUID); // add the variable UID into the set
                }
              }
            }
          }
        }
        //----------------------------------------------------

        goalList.add(new AuxForkTask(term, context));

        final Term tail = tlist.getTail();
        if (tail.getTermType() == LIST) {
          tlist = (TermList) tail;
        } else {
          break;
        }
      }
      goal.setPayload(goalList);
    }

    List<Future<Term>> resultList = new ArrayList<>();

    final ExecutorService executor = goal.getContext().getContextExecutorService();

    for (final AuxForkTask task : goalList) {
      resultList.add(executor.submit(task));
    }

    return resultList;
  }

  @Predicate(Signature = "fork/1", Template = {"+list"}, Reference = "Allows to prove a few goals (non linked between each other) in separated threads simultaneously, it is blocking the calling thread until all threads (started by the predicate) are completed. The fork implements AND operation (i.e. all goals have to be true else the predicate will fail).You must not have the same noninstantiated variables in terms that will be executed in different threads. The fork_error/1 will be thrown if any thread will throw an exception.")
  public static boolean predicateFORK(final ChoicePoint goal, final TermStruct predicate) throws InterruptedException {
    TermList termlist = predicate.getElement(0).findNonVarOrSame();

    // invoke all taska and wait for them all
    final List<Future<Term>> taskList = startListAsFork(goal, predicate, termlist);

    boolean result = true;
    int taskindex = 0;

    List<Throwable> forkExceptions = null; // lazy initialization

    while (!termlist.isNullList()) {
      final Term head = termlist.getHead();

      try {
        final Future<Term> task = taskList.get(taskindex);
        final Term resultOfTask = task.get();
        if (resultOfTask == null) {
          // we have not result of the task
          result = false;
          break;
        } else {
          if (!head.unifyTo(resultOfTask)) {
            final ProlCriticalError err = new ProlCriticalError("Impossible situation, the proven fork task goal is not equal the etalon task goal.");
            throw err;
          }
        }
      } catch (ExecutionException ex) {
        if (forkExceptions == null) {
          forkExceptions = new ArrayList<>();
        }

        forkExceptions.add(ex);
      }

      final Term tail = termlist.getTail();
      if (tail.getTermType() == LIST) {
        termlist = (TermList) tail;
      } else {
        break;
      }

      taskindex++;
    }

    if (forkExceptions != null) {

      final ProlForkExecutionException ex = new ProlForkExecutionException(predicate, forkExceptions.toArray(new Throwable[0]));
      throw ex;
    }

    if (!result) {
      goal.resetVariants();
    }

    return result;
  }

  @Predicate(Signature = "ifork/1", Template = {"+list"}, Reference = "It works like fork/1 but it will interrupt all noncompleted threads of the fork if any proven result is fail.")
  public static boolean predicateIFORK(final ChoicePoint goal, final TermStruct predicate) throws InterruptedException {
    final TermList termlist = predicate.getElement(0).findNonVarOrSame();

    // invoke all taska and wait for them all
    final List<Future<Term>> taskList = startListAsFork(goal, predicate, termlist);

    boolean result = true;

    Exception forkException = null;
    Term termThrowsException = null;

    // parse the list for terms
    final Term[] parsedgoal = termlist.toArray();

    final Map<Integer, Future<Term>> workingThreads = new HashMap<>();
    int index = 0;
    for (final Future<Term> task : taskList) {
      workingThreads.put(index++, task);
    }

    while (!workingThreads.isEmpty()) {
      if (Thread.currentThread().isInterrupted()) {
        // stop all fork tasks
        for (final Future<Term> task : workingThreads.values()) {
          task.cancel(true);
        }
        workingThreads.clear();
        throw new InterruptedException();
      }

      boolean stopAllWorkingThreads = false;
      Integer completedIndex = null;

      for (final Map.Entry<Integer, Future<Term>> checkingtask : workingThreads.entrySet()) {
        final Future<Term> term = checkingtask.getValue();
        if (term.isDone()) {
          completedIndex = checkingtask.getKey();

          if (term.isCancelled()) {
            stopAllWorkingThreads = true;
            break;
          }
          try {
            final Term resultterm = term.get();
            if (resultterm == null) {
              // fail
              result = false;
              stopAllWorkingThreads = true;
              break;
            } else {
              // check the result
              final int threadindex = completedIndex;
              final Term originalGoal = parsedgoal[threadindex];
              if (!originalGoal.unifyTo(resultterm)) {
                final ProlCriticalError err = new ProlCriticalError("Impossible situation, the proven fork task goal is not equal the etalon task goal. [index=" + threadindex + ']');
                throw err;
              }
            }
          } catch (final Exception ex) {
            final int threadindex = completedIndex;
            termThrowsException = parsedgoal[threadindex];
            forkException = ex;
            stopAllWorkingThreads = true;
            result = false;
          }
          break;
        }
      }

      // remove the completed task
      if (completedIndex != null) {
        workingThreads.remove(completedIndex);
      }

      if (stopAllWorkingThreads) {
        workingThreads.values().forEach((task) -> task.cancel(true));
      }
    }

    if (forkException != null) {

      final ProlForkExecutionException ex = new ProlForkExecutionException(termThrowsException, new Throwable[] {forkException});
      throw ex;
    }

    if (!result) {
      goal.resetVariants();
    }

    return result;
  }

  @Predicate(Signature = "regtrigger/3", Template = {"+predicate_indicator,+triggerevent,+callable_term"}, Reference = "regtrigger(somepredicate/3,onassert,triggerhandler) is always true. The predicate allows to register a trigger handler for distinguished predicate signature. The handled trigger event can be selected from the list [onassert, onretract, onassertretract].")
  @Determined
  public static boolean predicateREGTRIGGER(final ChoicePoint goal, final TermStruct predicate) {
    final String signature = Utils.extractPredicateSignatureFromStructure(predicate.getElement(0));
    final String triggerevent = predicate.getElement(1).findNonVarOrSame().getText();
    final Term callableTerm = predicate.getElement(2).findNonVarOrSame();
    final ProlContext context = goal.getContext();

    final ProlTriggerGoal triggergoal = new ProlTriggerGoal(callableTerm, context);

    if (null == triggerevent) {
      throw new ProlCriticalError("Unsupported trigger event detected [" + triggerevent + ']');
    } else {
      switch (triggerevent) {
        case "onassert":
          triggergoal.addSignature(signature, ProlTriggerType.TRIGGER_ASSERT);
          break;
        case "onretract":
          triggergoal.addSignature(signature, ProlTriggerType.TRIGGER_RETRACT);
          break;
        case "onassertretract":
          triggergoal.addSignature(signature, ProlTriggerType.TRIGGER_ASSERT_RETRACT);
          break;
        default:
          throw new ProlCriticalError("Unsupported trigger event detected [" + triggerevent + ']');
      }
    }

    context.registerTrigger(triggergoal);

    return true;
  }

  @Predicate(Signature = "copy_term/2", Template = {"?term,?term"}, Reference = "copy_term(X,Y) is true if and only if Y unifies with a term T which is a renamed copy of X.")
  @Determined
  public final boolean predicateCOPYTERM(final ChoicePoint goal, final TermStruct predicate) {
    final Term left = predicate.getElement(0).findNonVarOrSame().makeClone();
    final Term right = predicate.getElement(1).findNonVarOrSame();
    return right.unifyTo(left);
  }

  @Predicate(Signature = "\\+/1", Template = "+callable_term", Reference = "\\+(Term) is true if and only if call(Term) is false.")
  @Determined
  public final boolean predicateCannotBeProven(final ChoicePoint goal, final TermStruct predicate) {
    final Term argument = predicate.getElement(0);
    final ChoicePoint subgoal = new ChoicePoint(argument, goal.getContext());
    return subgoal.next() == null;
  }

  @Predicate(Signature = "time/4", Template = {"?integer,?integer,?integer,?integer"}, Reference = "Get current time Hours,Minutes,Seconds,Milliseconds.")
  @Determined
  public final boolean predicateTIME(final ChoicePoint goal, final TermStruct predicate) {
    final Calendar date = Calendar.getInstance();
    final TermLong hours = newLong(date.get(Calendar.HOUR_OF_DAY));
    final TermLong minutes = newLong(date.get(Calendar.MINUTE));
    final TermLong seconds = newLong(date.get(Calendar.SECOND));
    final TermLong milliseconds = newLong(date.get(Calendar.MILLISECOND));

    return predicate.getElement(0).unifyTo(hours) && predicate.getElement(1).unifyTo(minutes) && predicate.getElement(2).unifyTo(seconds) && predicate.getElement(3).unifyTo(milliseconds);
  }

  @Predicate(Signature = "date/3", Template = {"?integer,?integer,?integer"}, Reference = "Get current date Year, Month, Day. The January is 1st month")
  @Determined
  public final boolean predicateDATE(final ChoicePoint goal, final TermStruct predicate) {
    final Calendar date = Calendar.getInstance();
    final TermLong year = newLong(date.get(Calendar.YEAR));
    final TermLong month = newLong(date.get(Calendar.MONTH) + 1);
    final TermLong day = newLong(date.get(Calendar.DAY_OF_MONTH));

    return predicate.getElement(0).unifyTo(year) && predicate.getElement(1).unifyTo(month) && predicate.getElement(2).unifyTo(day);
  }


  private static final class AuxForkTask implements Callable<Term> {

    private final Term term;
    private final ChoicePoint goal;

    AuxForkTask(final Term termToSolve, final ProlContext context) {
      this.term = termToSolve.makeClone().findNonVarOrDefault(null);

      if (termToSolve.getTermType() == VAR) {
        this.goal = null;
      } else {
        this.goal = new ChoicePoint(this.term, context, null);
      }
    }

    ChoicePoint getGoal() {
      return this.goal;
    }

    Term getTerm() {
      return this.term;
    }

    @Override
    public Term call() {
      return this.goal == null ? null : this.goal.next();
    }

    @Override
    public String toString() {
      return this.term.toString();
    }
  }
}
