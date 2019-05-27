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
import com.igormaznitsa.prol.containers.*;
import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.*;
import com.igormaznitsa.prol.io.ProlStream;
import com.igormaznitsa.prol.io.ProlStreamManager;
import com.igormaznitsa.prol.io.ProlTextReader;
import com.igormaznitsa.prol.io.ProlTextWriter;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerGoal;
import com.igormaznitsa.prol.logic.triggers.ProlTriggerType;
import com.igormaznitsa.prol.parser.ProlConsult;
import com.igormaznitsa.prol.utils.Utils;

import java.awt.*;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Reader;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The Prol Core Library This class extends a prol abstract library and contain
 * a lot of useful definitions
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
@ProlOperators(Operators = {
        //------------------------
        @ProlOperator(Priority = 0, Type = Operator.OPTYPE_XFX, Name = "("),
        @ProlOperator(Priority = 0, Type = Operator.OPTYPE_XFX, Name = ")"),
        @ProlOperator(Priority = 0, Type = Operator.OPTYPE_XFX, Name = "["),
        @ProlOperator(Priority = 0, Type = Operator.OPTYPE_XFX, Name = "]"),
        @ProlOperator(Priority = 1200, Type = Operator.OPTYPE_XF, Name = "."),
        @ProlOperator(Priority = 1200, Type = Operator.OPTYPE_XFX, Name = "|"),
        //------------------------
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "is"),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "="),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "\\="),
        @ProlOperator(Priority = 1000, Type = Operator.OPTYPE_XFY, Name = ","),
        @ProlOperator(Priority = 1050, Type = Operator.OPTYPE_XFY, Name = "->"),
        @ProlOperator(Priority = 1100, Type = Operator.OPTYPE_XFY, Name = ";"),
        @ProlOperator(Priority = 1200, Type = Operator.OPTYPE_FX, Name = "?-"),
        @ProlOperator(Priority = 1200, Type = Operator.OPTYPE_FX, Name = ":-"),
        @ProlOperator(Priority = 1200, Type = Operator.OPTYPE_XFX, Name = ":-"),
        @ProlOperator(Priority = 900, Type = Operator.OPTYPE_FY, Name = "\\+"),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = ">"),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "<"),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "=<"),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = ">="),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "=="),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "=\\="),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "\\=="),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "@<"),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "@>"),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "@=<"),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "@>="),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "=:="),
        @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "=.."),
        @ProlOperator(Priority = 500, Type = Operator.OPTYPE_YFX, Name = "/\\"),
        @ProlOperator(Priority = 500, Type = Operator.OPTYPE_YFX, Name = "\\/"),
        @ProlOperator(Priority = 500, Type = Operator.OPTYPE_YFX, Name = "+"),
        @ProlOperator(Priority = 500, Type = Operator.OPTYPE_YFX, Name = "-"),
        @ProlOperator(Priority = 500, Type = Operator.OPTYPE_FX, Name = "not"),
        @ProlOperator(Priority = 500, Type = Operator.OPTYPE_FX, Name = "+"),
        @ProlOperator(Priority = 500, Type = Operator.OPTYPE_FX, Name = "-"),
        @ProlOperator(Priority = 400, Type = Operator.OPTYPE_YFX, Name = "*"),
        @ProlOperator(Priority = 400, Type = Operator.OPTYPE_YFX, Name = "/"),
        @ProlOperator(Priority = 400, Type = Operator.OPTYPE_YFX, Name = "//"),
        @ProlOperator(Priority = 400, Type = Operator.OPTYPE_YFX, Name = "rem"),
        @ProlOperator(Priority = 400, Type = Operator.OPTYPE_YFX, Name = "<<"),
        @ProlOperator(Priority = 400, Type = Operator.OPTYPE_YFX, Name = ">>"),
        @ProlOperator(Priority = 300, Type = Operator.OPTYPE_XFX, Name = "mod"),
        @ProlOperator(Priority = 200, Type = Operator.OPTYPE_FY, Name = "\\"),
        @ProlOperator(Priority = 200, Type = Operator.OPTYPE_XFX, Name = "**"),
        @ProlOperator(Priority = 200, Type = Operator.OPTYPE_XFY, Name = "^")
})
public final class ProlCoreLibrary extends ProlAbstractLibrary {

    /**
     * The constant contains the NEXT_LINE atom to avoid recreation of such atom
     * and use the only instance
     */
    public final static Term NEXT_LINE = new Term("\n");
    /**
     * The constant contains the SPACE atom to avoid recreation of such atom and
     * use the only instance
     */
    public final static Term SPACE = new Term(" ");
    /**
     * The Inside logger, the canonical class name is used as the logger
     * identifier (ProlCoreLibrary.class.getCanonicalName())
     */
    protected static final Logger LOG = Logger.getLogger(ProlCoreLibrary.class.getCanonicalName());
    /**
     * The randomize generator to make random numbers
     */
    private static Random RANDOMIZEGEN = new Random(System.nanoTime());
    /**
     * An Inside constant for use at some predicates
     */
    private static Term TRUE = new Term("true");

    public ProlCoreLibrary() {
        super("ProlCoreLib");
    }

    @Predicate(Signature = "=:=/2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic Equal")
    @Determined
    public static boolean predicateArithEqu(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return left.compare(right) == 0;
    }

    @Predicate(Signature = "@</2", Template = {"?term,?term"}, Reference = "Term less than")
    @Determined
    public static boolean predicateTermLess(final Goal goal, final TermStruct predicate) {
        return predicate.getElement(0).termComparsion(predicate.getElement(1)) < 0;
    }

    @Predicate(Signature = "@=</2", Template = {"?term,?term"}, Reference = "Term less than or equal to.")
    @Determined
    public static boolean predicateTermLessOrEqu(final Goal goal, final TermStruct predicate) {
        return predicate.getElement(0).termComparsion(predicate.getElement(1)) <= 0;
    }

    @Predicate(Signature = "@>/2", Template = {"?term,?term"}, Reference = "Term greater than")
    @Determined
    public static boolean predicateTermMore(final Goal goal, final TermStruct predicate) {
        return predicate.getElement(0).termComparsion(predicate.getElement(1)) > 0;
    }

    @Predicate(Signature = "@>=/2", Template = {"?term,?term"}, Reference = "Term greater than or equal to.")
    @Determined
    public static boolean predicateTermMoreOrEqu(final Goal goal, final TermStruct predicate) {
        return predicate.getElement(0).termComparsion(predicate.getElement(1)) >= 0;
    }

    @Predicate(Signature = "==/2", Template = {"?term,?term"}, Reference = "Term identical")
    @Determined
    public static boolean predicateTermEqu(final Goal goal, final TermStruct predicate) {
        return predicate.getElement(0).termComparsion(predicate.getElement(1)) == 0;
    }

    @Predicate(Signature = "\\==/2", Template = {"?term,?term"}, Reference = "Term not identical")
    @Determined
    public static boolean predicateNotTermEqu(final Goal goal, final TermStruct predicate) {
        return predicate.getElement(0).termComparsion(predicate.getElement(1)) != 0;
    }

    @Predicate(Signature = ">/2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic greater than")
    @Determined
    public static boolean predicateArithMore(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return left.compare(right) > 0;
    }

    @Predicate(Signature = "</2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic less than")
    @Determined
    public static boolean predicateArithLess(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return left.compare(right) < 0;
    }

    @Predicate(Signature = ">=/2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic greater than or equal to")
    @Determined
    public static boolean predicateArithMoreOrEqu(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return left.compare(right) >= 0;
    }

    @Predicate(Signature = "=</2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic less than or equal to")
    @Determined
    public static boolean predicateArithLessOrEqu(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return left.compare(right) <= 0;
    }

    @Predicate(Signature = "=\\=/2", Template = {"@evaluable,@evaluable"}, Reference = "Arithmetic Not equal")
    @Determined
    public static boolean predicateArithNotEqu(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return left.compare(right) != 0;
    }

    @Predicate(Signature = "xor/2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise `exclusive or'")
    @Evaluable
    public static Term predicateXOR(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        if (left instanceof TermInteger && right instanceof TermInteger) {
            final int lft = left.getNumericValue().intValue();
            final int rght = right.getNumericValue().intValue();

            return new TermInteger(lft ^ rght);
        } else {
            throw new ProlInstantiationErrorException("Both arguments must be integer", predicate);
        }
    }

    @Predicate(Signature = "\\/1", Template = {"+evaluable"}, Reference = "Bitwise 'not'")
    @Evaluable
    public static Term predicateBITWISENOT(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));

        if (left instanceof TermInteger) {
            final int lft = left.getNumericValue().intValue();

            return new TermInteger(~lft);
        } else {
            throw new ProlInstantiationErrorException("Argument must be integer", predicate);
        }
    }

    @Predicate(Signature = "\\//2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise 'or'")
    @Evaluable
    public static Term predicateBITWISEOR(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        if (left instanceof TermInteger && right instanceof TermInteger) {
            final int lft = left.getNumericValue().intValue();
            final int rght = right.getNumericValue().intValue();

            return new TermInteger(lft | rght);
        } else {
            throw new ProlInstantiationErrorException("Both arguments must be integer", predicate);
        }
    }

    @Predicate(Signature = "/\\/2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise 'and'")
    @Evaluable
    public static Term predicateBITWISEAND(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        if (left instanceof TermInteger && right instanceof TermInteger) {
            final int lft = left.getNumericValue().intValue();
            final int rght = right.getNumericValue().intValue();

            return new TermInteger(lft & rght);
        } else {
            throw new ProlInstantiationErrorException("Both arguments must be integer", predicate);
        }
    }

    @Predicate(Signature = "mod/2", Template = {"+evaluable,+evaluable"}, Reference = "Modulus")
    @Evaluable
    public static Term predicateMOD(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        final int leftval = left.getNumericValue().intValue();
        final int rightval = right.getNumericValue().intValue();

        return new TermInteger(leftval % rightval);
    }

    @Predicate(Signature = "rem/2", Template = {"+evaluable,+evaluable"}, Reference = "Remainder")
    @Evaluable
    public static Term predicateREM(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        final int leftval = left.getNumericValue().intValue();
        final int rightval = right.getNumericValue().intValue();

        return new TermInteger(leftval - (leftval / rightval) * rightval);
    }

    @Predicate(Signature = "**/2", Template = {"+evaluable,+evaluable"}, Reference = "Power")
    @Evaluable
    public static Term predicatePOWER(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        final float leftval = left.getNumericValue().floatValue();
        final float rightval = right.getNumericValue().floatValue();

        return new TermFloat((float) Math.pow(leftval, rightval));
    }

    @Predicate(Signature = "+/2", Template = {"+evaluable,+evaluable"}, Reference = "Addition")
    @Evaluable
    public static Term predicateADDTWO(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return (Term) (left.add(right));
    }

    @Predicate(Signature = "sin/1", Template = {"+evaluable"}, Reference = "Sine")
    @Evaluable
    public static Term predicateSIN(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermFloat((float) Math.sin(value));
    }

    @Predicate(Signature = "float_integer_part/1", Template = {"+evaluable"}, Reference = "Integer part")
    @Evaluable
    public static Term predicateFLOATINTEGERPART(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final int value = arg.getNumericValue().intValue();
        return new TermInteger(value);
    }

    @Predicate(Signature = "float_fractional_part/1", Template = {"+evaluable"}, Reference = "Fractional part")
    @Evaluable
    public static Term predicateFLOATFRACTIONALPART(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        final int valueInt = (int) value;
        return new TermFloat(value - (float) valueInt);
    }

    @Predicate(Signature = "floor/1", Template = {"+evaluable"}, Reference = "Floor")
    @Evaluable
    public static Term predicateFLOOR(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermInteger((int) Math.floor(value));
    }

    @Predicate(Signature = "truncate/1", Template = {"+evaluable"}, Reference = "Truncate")
    @Evaluable
    public static Term predicateTRUNCATE(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermInteger(value < 0 ? (int) Math.ceil(value) : (int) Math.floor(value));
    }

    @Predicate(Signature = "round/1", Template = {"+evaluable"}, Reference = "Round")
    @Evaluable
    public static Term predicateROUND(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermInteger(Math.round(value));
    }

    @Predicate(Signature = "ceiling/1", Template = {"+evaluable"}, Reference = "Ceiling")
    @Evaluable
    public static Term predicateCEILING(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermInteger((int) Math.ceil(value));
    }

    @Predicate(Signature = "cos/1", Template = {"+evaluable"}, Reference = "Cosine")
    @Evaluable
    public static Term predicateCOS(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermFloat((float) Math.cos(value));
    }

    @Predicate(Signature = "atan/1", Template = {"+evaluable"}, Reference = "Arc tangent")
    @Evaluable
    public static Term predicateATAN(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermFloat((float) Math.atan(value));
    }

    @Predicate(Signature = "exp/1", Template = {"+evaluable"}, Reference = "Exponentiation")
    @Evaluable
    public static Term predicateEXP(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermFloat((float) Math.exp(value));
    }

    @Predicate(Signature = "log/1", Template = {"+evaluable"}, Reference = "Log")
    @Evaluable
    public static Term predicateLOG(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermFloat((float) Math.log(value));
    }

    @Predicate(Signature = "sqrt/1", Template = {"+evaluable"}, Reference = "Square root")
    @Evaluable
    public static Term predicateSQRT(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        final float value = arg.getNumericValue().floatValue();
        return new TermFloat((float) Math.sqrt(value));
    }

    @Predicate(Signature = "abs/1", Template = {"+evaluable"}, Reference = "Absolute value")
    @Evaluable
    public static Term predicateABS(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        return (Term) arg.abs();
    }

    @Predicate(Signature = "sign/1", Template = {"+evaluable"}, Reference = "SIGN")
    @Evaluable
    public static Term predicateSIGN(final Goal goal, final TermStruct predicate) {
        final NumericTerm arg = calculatEvaluable(goal, predicate.getElement(0));
        return (Term) arg.sign();
    }

    @Predicate(Signature = "-/2", Template = {"+evaluable,+evaluable"}, Reference = "Subtraction")
    @Evaluable
    public static Term predicateSUBTWO(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return (Term) (left.sub(right));
    }

    @Predicate(Signature = "-/1", Template = {"+evaluable"}, Reference = "Negation")
    @Evaluable
    public static Term predicateNeg(final Goal goal, final TermStruct predicate) {
        final NumericTerm val = calculatEvaluable(goal, predicate.getElement(0));
        return (Term) (val.neg());
    }

    @Predicate(Signature = "+/1", Template = {"+evaluable"}, Reference = "Not action over a number")
    @Evaluable
    public static Term predicateTheSame(final Goal goal, final TermStruct predicate) {
        return (Term) calculatEvaluable(goal, predicate.getElement(0));
    }

    @Predicate(Signature = "*/2", Template = {"+evaluable,+evaluable"}, Reference = "Multiplication")
    @Evaluable
    public static Term predicateMUL(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return (Term) (left.mul(right));
    }

    @Predicate(Signature = "//2", Template = {"+evaluable,+evaluable"}, Reference = "Dividsion")
    @Evaluable
    public static Term predicateDIV(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        return (Term) (left.div(right));
    }

    @Predicate(Signature = "///2", Template = {"+evaluable,+evaluable"}, Reference = "Integer division")
    @Evaluable
    public static Term predicateINTDIV(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        Term result = (Term) left.div(right);
        if (result instanceof TermFloat) {
            result = new TermInteger(((NumericTerm) result).getNumericValue().intValue());
        }

        return result;
    }

    @Predicate(Signature = "<</2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise left shift")
    @Evaluable
    public static Term predicateSHIFTLEFT(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        final int value = left.getNumericValue().intValue();
        final int shift = right.getNumericValue().intValue();

        return new TermInteger(value << shift);
    }

    @Predicate(Signature = ">>/2", Template = {"+evaluable,+evaluable"}, Reference = "Bitwise right shift")
    @Evaluable
    public static Term predicateSHIFTRIGHT(final Goal goal, final TermStruct predicate) {
        final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0));
        final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1));

        final int value = left.getNumericValue().intValue();
        final int shift = right.getNumericValue().intValue();

        return new TermInteger(value >> shift);
    }

    @Predicate(Signature = "is/2", Template = {"?evaluable,@evaluable"}, Reference = "'is'(Result, Expression) is true if and only if the value of evaluating Expression as an expression is Result")
    @Determined
    public static boolean predicateIS(final Goal goal, final TermStruct predicate) {

        final Term leftPart = predicate.getElement(0);

        final NumericTerm rightPart = calculatEvaluable(goal, predicate.getElement(1));
        if (rightPart == null) {
            return false;
        }
        return leftPart.Equ((Term) rightPart);
    }

    @Predicate(Signature = "true/0", Reference = "The perdicate is always true.")
    @Determined
    public static void predicateTRUE(final Goal goal, final TermStruct predicate) {
    }

    @Predicate(Signature = "fail/0", Reference = "The predicate is always false.")
    @Determined
    public static boolean predicateFAIL(final Goal goal, final TermStruct predicate) {
        return false;
    }

    @Predicate(Signature = "not/1", Reference = "True if goal cannot be proven")
    @Determined
    public static boolean predicateNOT(final Goal goal, final TermStruct predicate) throws InterruptedException {
        final Goal localGoal = new Goal(predicate.getElement(0), goal.getContext(), goal.getTracer());
        final Term result = localGoal.solve();
        return result == null;
    }

    @Predicate(Signature = "=/2", Reference = "Unify X and Y terms. It is true if X and Y are unifiable.")
    @Determined
    public static boolean predicateEQU(final Goal goal, final TermStruct predicate) {
        final Term left = predicate.getElement(0);
        final Term right = predicate.getElement(1);
        return left.Equ(right);
    }

    @Predicate(Signature = "\\=/2", Reference = "Unify X and Y terms. It is true if X and Y are not-unifiable.")
    @Determined
    public static boolean predicateNOTEQU(final Goal goal, final TermStruct predicate) {
        final Term left = predicate.getElement(0);
        final Term right = predicate.getElement(1);
        return !left.Equ(right);
    }

    @Predicate(Signature = "!/0", Reference = "! is true. All choice ponts between the cut and the parent goal are removed. The effect is commit to use of both the current clause and the substitutions found at the point of the cut.")
    @Determined
    public static void predicateCUT(final Goal goal, final TermStruct predicate) {
        // it is a stub function for embedded inside operator
    }

    @Predicate(Signature = "!!/0", Reference = "!! is true. Local version of !/0. It doesn't cut the knowledge base selection, i.e. it works only inbounds of current goal.")
    @Determined
    public static void predicateCUTLOCAL(final Goal goal, final TermStruct predicate) {
        // it is a stub function for embedded inside operator
    }

    @Predicate(Signature = "repeat/0", Reference = "repeat is true. It just places a choice point every call.")
    public static void predicateREPEAT(final Goal goal, final TermStruct predicate) {
        // we just make a choose point
    }

    @Predicate(Signature = "clause/2", Template = {"+head,?callable_term"}, Reference = "clause(Head, Body) is true if and only if\n* The predicate of Head is public (the standard does not specify how a predicate is declared public but dynamic predicates are public, and\n* There is a clause in the database which corresponds to a term H:- B which unifies with Head :- Body.")
    public static boolean predicateCLAUSE(final Goal goal, final TermStruct predicate) {
        final Term head = Utils.getTermFromElement(predicate.getElement(0));
        final Term body = Utils.getTermFromElement(predicate.getElement(1));

        final TermStruct struct = head.getTermType() == Term.TYPE_STRUCT ? (TermStruct) head : new TermStruct(head);
        if (goal.getContext().findProcessor(struct) != PredicateProcessor.NULL_PROCESSOR) {
            throw new ProlPermissionErrorException("access", "private_procedure", predicate);
        }

        ClauseIterator clIterator = (ClauseIterator) goal.getAuxObject();

        if (clIterator == null) {
            clIterator = goal.getContext().getKnowledgeBase().getClauseIterator(head.getTermType() == Term.TYPE_STRUCT ? (TermStruct) head : new TermStruct(head));
            if (clIterator == null || !clIterator.hasNext()) {
                goal.noMoreVariants();
                return false;
            }

            goal.setAuxObject(clIterator);
        }

        if (clIterator.hasNext()) {
            //TODO to check

            while (clIterator.hasNext()) {
                final TermStruct nxtStruct = clIterator.next();
                if (nxtStruct != null) {
                    Term headClause;
                    Term bodyClause;
                    if (nxtStruct.isFunctorLikeRuleDefinition()) {
                        headClause = nxtStruct.getElement(0);
                        bodyClause = nxtStruct.getElement(1);
                    } else {
                        headClause = nxtStruct;
                        bodyClause = TRUE;
                    }

                    if (headClause.equWithoutSet(head) && bodyClause.equWithoutSet(body)) {
                        if (!(headClause.Equ(head) && bodyClause.Equ(body))) {
                            throw new ProlCriticalError("Impossible state at clause/2 #982342");
                        }
                        return true;
                    }
                }
            }
        }
        goal.noMoreVariants();
        return false;
    }

    @Predicate(Signature = "current_op/3", Template = "?integer,?operator_specifier,?atom", Reference = "current_op(Priority, Op_specifier, Operator) is true if and only if Operator is an operator with properties given by  Op_specifier and Priority")
    @SuppressWarnings("unchecked")
    public static boolean predicateCURRENTOP(final Goal goal, final TermStruct predicate) {
        final Term priority = Utils.getTermFromElement(predicate.getElement(0));
        final Term specifier = Utils.getTermFromElement(predicate.getElement(1));
        final Term name = Utils.getTermFromElement(predicate.getElement(2));

        Object[] auxObject = (Object[]) goal.getAuxObject();
        if (auxObject == null) {
            // the first call
            final Iterator<OperatorContainer> operator_iterator = goal.getContext().getKnowledgeBase().getOperatorIterator();
            auxObject = new Object[]{operator_iterator, null, null};
            goal.setAuxObject(auxObject);
        }

        final Iterator<OperatorContainer> operator_iterator = (Iterator<OperatorContainer>) auxObject[0];
        OperatorContainer last_container = (OperatorContainer) auxObject[1];
        Operator last_operator = (Operator) auxObject[2];

        final String opNameVal = name.getTermType() == Term.TYPE_ATOM ? name.getText() : null; // null = any
        final int typeVal = specifier.getTermType() == Term.TYPE_ATOM ? Operator.getTypeFromString(specifier.getText()) : -1; // -1 = any
        int priorityVal = 0; // 0 - any
        if (priority.getTermType() == Term.TYPE_ATOM) {
            priorityVal = ((TermInteger) priority).getNumericValue().intValue();
            if (priorityVal < 1 || priorityVal > 1200) {
                throw new ProlDomainErrorException("Unsupported operator priority", predicate);
            }
        }

        while (!Thread.currentThread().isInterrupted()) {
            if (last_container == null) {
                // find container
                while (operator_iterator.hasNext()) {
                    last_container = operator_iterator.next();

                    if (opNameVal != null) {
                        if (last_container.getText().equals(opNameVal)) {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                if (last_container == null) {
                    // there are not more variants
                    goal.noMoreVariants();
                    goal.setAuxObject(null);
                    return false;
                }
            }

            // find operator
            if (typeVal < 0) {
                // find all
                final int typestart = last_operator == null ? 0 : last_operator.getOperatorType() + 1;

                for (int li = typestart; li < 7; li++) {
                    last_operator = last_container.getForTypePrecisely(li);
                    if (last_operator != null) {
                        break;
                    }
                }
            } else {
                final Operator op = last_container.getForTypePrecisely(typeVal);
                if (op == last_operator) {
                    last_operator = null;
                }
            }

            if (last_operator != null) {
                if (priorityVal > 0) {
                    if (last_operator.getPriority() != priorityVal) {
                        continue;
                    }
                }
            } else {
                last_container = null;
                continue;
            }

            // we have found an operator
            auxObject[1] = last_container;
            auxObject[2] = last_operator;

            final Term priorityOfFound = new TermInteger(last_operator.getPriority());
            final Term specifierOfFound = new Term(last_operator.getTypeAsString());
            final Term nameOfFound = new Term(last_operator.getText());

            if (!(predicate.getElement(0).Equ(priorityOfFound) && predicate.getElement(1).Equ(specifierOfFound) && predicate.getElement(2).Equ(nameOfFound))) {
                goal.noMoreVariants();
                goal.setAuxObject(null);
                return false;
            } else {
                return true;
            }
        }
        
        return false;
    }

    @Predicate(Signature = "op/3", Template = "+integer,+operator_specifier,@atom_or_atom_list", Reference = "These predicates allow the operator table to be altered or inspected.\nop(Priority, Op_Specifier, Operator) is true, with the side effect that\n1. if Priority is 0 then Operator is removed from the operator table, else\n2. Operator is added to the Operator table, with priority (lower binds tighter) Priority and associativity determined by Op_Specifier")
    @Determined
    public static boolean predicateOP(final Goal goal, final TermStruct predicate) {
        final int priority = ((TermInteger) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
        final String specifier = Utils.getTermFromElement(predicate.getElement(1)).getText();
        final Term atomOrList = Utils.getTermFromElement(predicate.getElement(2));

        if (priority < 0 || priority > 1200) {
            throw new ProlDomainErrorException("Priority must be between 0 and 1200 inclusive", predicate);
        }
        final int opType = Operator.getTypeFromString(specifier);
        if (opType < 0) {
            throw new ProlDomainErrorException("Wrong operator specifier", predicate);
        }

        final ArrayList<String> names = new ArrayList<>();
        if (atomOrList.getTermType() == Term.TYPE_LIST) {
            TermList list = (TermList) atomOrList;
            while (!list.isNullList()) {
                Term atom = list.getHead();
                if ((atom instanceof NumericTerm) || atom.getTermType() != Term.TYPE_ATOM) {
                    throw new ProlDomainErrorException("Impossible operator name", predicate);
                }
                names.add(atom.getText());

                atom = list.getTail();
                if (atom.getTermType() != Term.TYPE_LIST) {
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
                for (final String name : names) {
                    base.removeOperator(name, opType);
                }
            } else {
                for (final String name : names) {
                    base.addOperator(new Operator(priority, opType, name));
                }
            }
        } catch (SecurityException ex) {
            throw new ProlPermissionErrorException("create", "operator", "Attemption to override or remove a system operator", predicate);
        }
        return true;
    }

    @Predicate(Signature = "call/1", Template = {"+callable_term"}, Reference = "call(G) is true if and only if G represents a goal which is true.")
    public static boolean predicateCALL(final Goal goal, final TermStruct predicate) throws InterruptedException {
        Goal currentgoal = (Goal) goal.getAuxObject();
        final Term argument = Utils.getTermFromElement(predicate.getElement(0));

        if (currentgoal == null) {
            // first call of the goal
            currentgoal = new Goal(argument, goal.getContext(), goal.getTracer());
            goal.setAuxObject(currentgoal);
        }

        final Term nextResult = currentgoal.solve();

        boolean result = false;

        if (nextResult != null) {
            if (!argument.Equ(nextResult)) {
                throw new ProlCriticalError("Can't make equ for result of CALL");
            }
            if (currentgoal.isCompleted()) {
                goal.noMoreVariants();
            }

            result = true;
        }

        return result;
    }

    @Predicate(Signature = "once/1", Template = {"+callable_term"}, Reference = "once(Term) is true. once/1 is not re-executable.")
    @Determined
    public static boolean predicateONCE(final Goal goal, final TermStruct predicate) throws InterruptedException {
        final Term argument = Utils.getTermFromElement(predicate.getElement(0));
        final Goal currentgoal = new Goal(argument, goal.getContext(), goal.getTracer());

        final Term nextResult = currentgoal.solve();

        return nextResult != null;
    }

    @Predicate(Signature = ";/2", Reference = "';'(Either, Or) is true iff and either Either or Or is true.")
    public final static void predicateOR(final Goal goal, final TermStruct predicate) throws InterruptedException {
        // it is a stub function for embedded inside operator
    }

    @Predicate(Signature = "->/2", Reference = "'->'(If, Then) is true if and only if If is true and Then is true for the first solution of If")
    @ItChangesGoalChain
    public final static boolean predicateIFTHEN(final Goal goal, final TermStruct predicate) throws InterruptedException {
        // if-then
        final Goal leftSubbranch = new Goal(predicate.getElement(0), goal.getContext(), goal.getTracer());
        boolean result = false;
        if (leftSubbranch.solve() != null) {
            // replace current goal by the 'then' goal
            final Goal thenPart = goal.replaceLastGoalAtChain(predicate.getElement(1));
            thenPart.cutLocal(); // remove all previous choice points
            result = true;
        } else {
            goal.noMoreVariants();
        }
        return result;
    }

    @Predicate(Signature = ",/2", Reference = "','(First, Second) is true if and only if First is true and Second is true.")
    public static void predicateAND(final Goal goal, final TermStruct predicate) throws InterruptedException {
        // it is a stub function for embedded inside operator
    }

    @Predicate(Signature = "var/1", Reference = "var(X) is true if and only if X is a variable.")
    @Determined
    public static boolean predicateVAR(final Goal goal, final TermStruct predicate) {
        final Term arg = predicate.getElement(0);
        if (arg.getTermType() == Term.TYPE_VAR) {
            return ((Var) arg).isUndefined();
        } else {
            return false;
        }
    }

    @Predicate(Signature = "nonvar/1", Reference = "nonvar(X) is true if and only if X is not a variable.")
    @Determined
    public static boolean predicateNONVAR(final Goal goal, final TermStruct predicate) {
        final Term arg = predicate.getElement(0);
        if (arg.getTermType() == Term.TYPE_VAR) {
            return !((Var) arg).isUndefined();
        } else {
            return true;
        }
    }

    @Predicate(Signature = "atom/1", Reference = "atom(X) is true if and only if X is an atom.")
    @Determined
    public static boolean predicateATOM(final Goal goal, final TermStruct predicate) {
        Term arg = predicate.getElement(0);
        if (arg.getTermType() == Term.TYPE_VAR) {
            arg = ((Var) arg).getValue();
            if (arg == null) {
                return false;
            }
        }

        boolean result = false;

        switch (arg.getTermType()) {
            case Term.TYPE_ATOM: {
                result = !(arg instanceof NumericTerm);
            }
            break;
            case Term.TYPE_LIST: {
                result = ((TermList) arg).isNullList();
            }
            break;
        }

        return result;
    }

    @Predicate(Signature = "integer/1", Reference = "integer(X) is true if and only if X is an integer.")
    @Determined
    public static boolean predicateINTEGER(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));

        if (arg.getTermType() == Term.TYPE_ATOM) {
            return arg instanceof TermInteger;
        } else {
            return false;
        }
    }

    @Predicate(Signature = "number/1", Reference = "number(X) is true if and only if X is an integer or a float.")
    @Determined
    public static boolean predicateNUMBER(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        return (arg.getTermType() == Term.TYPE_ATOM) && (arg instanceof NumericTerm);
    }

    @Predicate(Signature = "float/1", Reference = "float(X) is true if and only if X is a float.")
    @Determined
    public static boolean predicateFLOAT(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        if (arg.getTermType() == Term.TYPE_ATOM) {
            return arg instanceof TermFloat;
        } else {
            return false;
        }
    }

    @Predicate(Signature = "compound/1", Reference = "compound(X) is true if and only if X is a compound term, that is neither atomic nor a variable.")
    @Determined
    public static boolean predicateCOMPOUND(final Goal goal, final TermStruct predicate) {
        final Term atom = Utils.getTermFromElement(predicate.getElement(0));
        switch (atom.getTermType()) {
            case Term.TYPE_STRUCT:
                return true;
            case Term.TYPE_LIST:
                return !((TermList) atom).isNullList();
            default:
                return false;
        }
    }

    @Predicate(Signature = "atomic/1", Reference = "atomic(X) is true if and only if X is atomic (that is an atom, an integer or a float).")
    @Determined
    public static boolean predicateATOMIC(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        boolean result = false;
        switch (arg.getTermType()) {
            case Term.TYPE_ATOM: {
                result = true;
            }
            break;
            case Term.TYPE_LIST: {
                result = ((TermList) arg).isNullList();
            }
            break;
        }
        return result;
    }

    @Predicate(Signature = "arg/3", Template = {"+integer,+compound_term,?term"}, Reference = "arg(N,Term, Arg) is true if nad only if the Nth argument of Term is Arg")
    @Determined
    public static boolean predicateARG(final Goal goal, final TermStruct predicate) {
        final TermInteger number = (TermInteger) Utils.getTermFromElement(predicate.getElement(0));
        final Term compound_term = Utils.getTermFromElement(predicate.getElement(1));
        final Term element = Utils.getTermFromElement(predicate.getElement(2));

        final int index = number.getNumericValue().intValue();

        if (index < 0) {
            throw new ProlDomainErrorException("Element number less than zero", number);
        }
        if (index == 0) {
            return false;
        }

        if (compound_term.getTermType() == Term.TYPE_STRUCT) {
            final TermStruct struct = (TermStruct) compound_term;
            final int elementIndex = index - 1;
            if (elementIndex >= struct.getArity()) {
                return false;
            }
            return element.Equ(struct.getElement(elementIndex));
        } else {
            return false;
        }
    }

    @Predicate(Signature = "functor/3", Template = {"-nonvar,+atomic,+integer", "+nonvar,?atomic,?integer"}, Reference = "functor(Term, Name, Arity) is true if and only if:\n* Term is a compound term with functor name Name and arity Arity or\n* Term is an atomic term equal to Name and Arity is 0.")
    @Determined
    public static boolean predicateFUNCTOR(final Goal goal, final TermStruct predicate) {
        final Term argTerm = Utils.getTermFromElement(predicate.getElement(0));
        final Term argName = Utils.getTermFromElement(predicate.getElement(1));
        final Term argArity = Utils.getTermFromElement(predicate.getElement(2));

        switch (argTerm.getTermType()) {
            case Term.TYPE_ATOM: {
                final Term arity = new TermInteger(0);
                return argName.Equ(argTerm) && argArity.Equ(arity);
            }
            case Term.TYPE_STRUCT: {
                final TermStruct struct = (TermStruct) argTerm;
                final Term functor = new Term(struct.getFunctor().getText());
                final Term arity = new TermInteger(struct.getArity());
                return argName.Equ(functor) && argArity.Equ(arity);
            }
            case Term.TYPE_LIST: {
                final TermList list = (TermList) argTerm;

                Term name;
                Term arity;

                if (list.isNullList()) {
                    arity = new TermInteger(0);
                    name = TermList.NULLLIST;
                } else {
                    arity = new TermInteger(2);
                    name = TermList.LIST_FUNCTOR_AS_TERM;
                }
                return argName.Equ(name) && argArity.Equ(arity);
            }
            case Term.TYPE_VAR: {
                final int arity = ((TermInteger) argArity).getNumericValue().intValue();
                if (arity < 0) {
                    throw new ProlRepresentationErrorException("Wrong arity value", predicate);
                }

                if (argName instanceof NumericTerm) {
                    if (arity == 0) {
                        return argTerm.Equ(argName);
                    } else {
                        throw new ProlTypeErrorException("atom", predicate);
                    }
                }

                Term[] elements = null;

                if (arity > 0) {
                    elements = new Term[arity];
                    for (int li = 0; li < arity; li++) {
                        elements[li] = new Var();
                    }
                }

                final TermStruct newStruct = new TermStruct(argName, elements);

                return argTerm.Equ(newStruct);
            }
            default:
                throw new ProlCriticalError("Unsupported type found!");
        }

    }

    @Predicate(Signature = "=../2", Template = {"+nonvar,?non_empty_list", "-nonvar,+non_empty_list"}, Reference = "Term =.. List is true if and only if\n* Term is an atomic term and List is the list whose only element is Term, or\n* Term is a compound term and List is the list whose head is the functor name of Term and whose tail is the list of the arguments of Term. ")
    @Determined
    public static boolean predicateUNIV(final Goal goal, final TermStruct predicate) {
        final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
        final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

        if (argRight.getTermType() != Term.TYPE_VAR) {
            Term atom = Utils.getListAsAtom(goal.getContext(), (TermList) argRight);
            return argLeft.Equ(atom);
        } else {
            TermList lst = Utils.unrollTermIntoList(argLeft);
            return argRight.Equ(lst);
        }
    }

    private static boolean consultFromResource(String resource, ProlContext context, ProlStreamManager streamManager) {
        Reader reader = null;
        try {
            reader = streamManager.getReaderForResource(resource);
            final ProlConsult consulter = new ProlConsult(reader, context);
            consulter.consult();
            return true;
        } catch (IOException ex) {
            LOG.log(Level.WARNING, "consultFromResource()", ex);
            return false;
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception ex) {
                    LOG.log(Level.WARNING, "consultFromResource()", ex);
                }
            }
        }
    }

    @Predicate(Signature = "consult/1", Template = {"+atom", "+list"}, Reference = "Take an atom as the file name of the resource to be used for consultation, or a list contains a resource name chain. The resource will be getted through the current ProlStreamManager.")
    @Determined
    public static boolean predicateCONSULT(final Goal goal, final TermStruct predicate) {
        final Term term = Utils.getTermFromElement(predicate.getElement(0));

        final ProlContext ctxt = goal.getContext();
        final ProlStreamManager streamManager = ctxt.getStreamManager();

        switch (term.getTermType()) {
            case Term.TYPE_ATOM: {
                String name = term.getText();
                return consultFromResource(name, ctxt, streamManager);
            }
            case Term.TYPE_LIST: {
                TermList list = (TermList) term;

                while (!Thread.currentThread().isInterrupted()) {
                    if (list.isNullList()) {
                        return true;
                    }
                    final Term headterm = Utils.getTermFromElement(list.getHead());
                    final Term tailTerm = Utils.getTermFromElement(list.getTail());
                    if (tailTerm.getTermType() == Term.TYPE_LIST) {
                        list = (TermList) tailTerm;
                    } else {
                        return false;
                    }

                    final String name = headterm.getText();
                    if (!consultFromResource(name, ctxt, streamManager)) {
                        return false;
                    }
                }
            }
            default:
                return false;
        }
    }

    private static NumericTerm calculatEvaluable(final Goal goal, Term term) {
        try {
            if (term.getTermType() == Term.TYPE_VAR) {
                final Var varoriginal = (Var) term;
                term = ((Var) term).getValue();
                if (term == null) {
                    throw new ProlInstantiationErrorException("An empty variable [" + varoriginal + "] found at [" + goal + ']', varoriginal);
                }
            }

            switch (term.getTermType()) {
                case Term.TYPE_ATOM: {
                    if (term instanceof NumericTerm) {
                        return (NumericTerm) term;
                    } else {
                        throw new ProlTypeErrorException("number", "Not a numeric atom +[" + term + "] found at goal [" + goal + ']', term);
                    }
                }
                case Term.TYPE_STRUCT: {
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
    public static boolean predicateATOMCHARS(final Goal goal, final TermStruct predicate) {
        Term left = Utils.getTermFromElement(predicate.getElement(0));
        Term right = Utils.getTermFromElement(predicate.getElement(1));

        switch (left.getTermType()) {
            case Term.TYPE_ATOM: {
                left = left.asCharList();
                return left.Equ(right);
            }
            case Term.TYPE_LIST: {
                if (((TermList) left).isNullList()) {
                    left = new Term("[]").asCharList();
                    return left.Equ(right);
                } else {
                    throw new ProlTypeErrorException("atom", predicate);
                }
            }
        }

        if (right.getTermType() == Term.TYPE_LIST) {
            StringBuilder builder = new StringBuilder();

            TermList list = (TermList) right;
            while (list != TermList.NULLLIST) {
                final Term head = list.getHead();
                builder.append(head.getText());

                final Term tail = list.getTail();
                if (tail.getTermType() == Term.TYPE_LIST) {
                    list = (TermList) tail;
                } else {
                    return false;
                }
            }

            right = new Term(builder.toString());
            return left.Equ(right);
        }

        return false;
    }

    @Predicate(Signature = "char_code/2", Template = {"+character,?character_code", "-character,+character_code"}, Reference = "char_code(Char, Code) succeeds if and only if Code is the character code that corresponds to the character Char.")
    @Determined
    public static boolean predicateCHARCODE(final Goal goal, final TermStruct predicate) {
        Term left = Utils.getTermFromElement(predicate.getElement(0));
        Term right = Utils.getTermFromElement(predicate.getElement(1));

        if (left.getTermType() == Term.TYPE_ATOM) {
            left = new TermInteger((int) left.getText().charAt(0));
            return right.Equ(left);
        }

        if (right.getTermType() == Term.TYPE_ATOM) {
            right = new Term(Character.toString((char) ((TermInteger) right).getNumericValue().intValue()));
            return left.Equ(right);
        }

        return false;
    }

    @Predicate(Signature = "number_codes/2", Template = {"+number,?character_code_list", "-number,+character_code_list"}, Reference = "number_codes(Number, CodeList) succeeds if and only if CodeList is a list whose elements are the codes for the one character atoms that in order make up Number.")
    @Determined
    public static boolean predicateNUMBERCODES(final Goal goal, final TermStruct predicate) {
        Term left = Utils.getTermFromElement(predicate.getElement(0));
        final Term right = Utils.getTermFromElement(predicate.getElement(1));

        if (left.getTermType() == Term.TYPE_ATOM && right.getTermType() == Term.TYPE_VAR) {
            left = left.asCharCodeList();
            return left.Equ(right);
        }

        if (right.getTermType() == Term.TYPE_LIST) {
            final StringBuilder builder = new StringBuilder();

            TermList list = (TermList) right;
            while (list != TermList.NULLLIST) {
                final TermInteger head = (TermInteger) list.getHead();
                builder.append((char) head.getNumericValue().intValue());

                final Term tail = list.getTail();
                if (tail.getTermType() == Term.TYPE_LIST) {
                    list = (TermList) tail;
                } else {
                    return false;
                }
            }

            final String numberValue = builder.toString();

            Term number;

            try {
                if (numberValue.startsWith("0x")) {
                    number = new TermInteger(Integer.parseInt(numberValue.substring(2), 16));
                } else {
                    number = new TermInteger(numberValue);
                }
            } catch (NumberFormatException ex) {
                try {
                    number = new TermFloat(numberValue);
                } catch (NumberFormatException exx) {
                    number = new Term(numberValue);
                }
            }

            return left.Equ(number);
        }

        return false;
    }

    @Predicate(Signature = "number_chars/2", Template = {"+number,?character_list", "-number,+character_list"}, Reference = "number_chars(Number, List) succeeds if and only if List is a list whose elements are the one character atoms that in order make up Number.")
    @Determined
    public static boolean predicateNUMBERCHARS(final Goal goal, final TermStruct predicate) {
        Term left = Utils.getTermFromElement(predicate.getElement(0));
        final Term right = Utils.getTermFromElement(predicate.getElement(1));

        if (right.getTermType() == Term.TYPE_LIST) {
            final StringBuilder builder = new StringBuilder();

            TermList list = (TermList) right;
            boolean add = false;
            while (list != TermList.NULLLIST) {
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
                if (tail.getTermType() == Term.TYPE_LIST) {
                    list = (TermList) tail;
                } else {
                    return false;
                }
            }

            Term number = null;

            final String numberValue = builder.toString();

            try {
                if (numberValue.startsWith("0x")) {
                    number = new TermInteger(Integer.parseInt(numberValue.substring(2), 16));
                } else {
                    number = new TermInteger(numberValue);
                }
            } catch (NumberFormatException ex) {
                try {
                    number = new TermFloat(numberValue);
                } catch (NumberFormatException exx) {
                    throw new ProlCustomErrorException(new TermStruct(new Term("syntax_error"), new Term[]{new Term(numberValue)}), predicate);
                }
            }

            return left.Equ(number);
        }

        if (left.getTermType() == Term.TYPE_ATOM) {
            left = left.asCharList();
            return left.Equ(right);
        }

        return false;
    }

    @Predicate(Signature = "for/3", Template = {"?term,+integer,+integer"}, Reference = "Allows to make an integer counter from a variable, (Var, Start, End).")
    public static boolean predicateFOR(final Goal goal, final TermStruct predicate) {
        final Term term = predicate.getElement(0);
        if (term.getTermType() != Term.TYPE_VAR) {
            goal.noMoreVariants();
            return false;
        }

        final Var var = (Var) predicate.getElement(0);
        final int start = ((TermInteger) Utils.getTermFromElement(predicate.getElement(1))).getNumericValue().intValue();
        final int limit = ((TermInteger) Utils.getTermFromElement(predicate.getElement(2))).getNumericValue().intValue();

        Integer currentInt = (Integer) goal.getAuxObject();

        if (currentInt == null) {
            // first call
            var.changeValue(new TermInteger(start));
            goal.setAuxObject(start);
        } else {
            // not first call
            currentInt++;
            if (currentInt > limit) {
                goal.noMoreVariants();
                return false;
            } else {
                var.changeValue(new TermInteger(currentInt));
                goal.setAuxObject(currentInt);
            }
        }

        return true;
    }

    @Predicate(Signature = "rnd/2", Template = {"+integer,?integer", "+list,?term"}, Reference = "Allows to generate a pseudo randomize integer (limit,value) between 0 (inclusive) and the limit (exclusive) or select random element from the list.")
    @Determined
    public static boolean predicateRND(final Goal goal, final TermStruct predicate) {
        final Term term = Utils.getTermFromElement(predicate.getElement(0));

        if (term.getTermType() == Term.TYPE_LIST) {

            final TermList list = (TermList) term;

            final Term result;
            if (list.isNullList()) {
                result = TermList.NULLLIST;
            } else {
                // calculate length of the list
                final Term[] array = Utils.listToArray(list);
                result = array[RANDOMIZEGEN.nextInt(array.length)];
            }

            return predicate.getElement(1).Equ(result);

        } else {
            final int limit = ((TermInteger) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();

            final TermInteger genVal = new TermInteger(RANDOMIZEGEN.nextInt(limit));
            return predicate.getElement(1).Equ(genVal);
        }
    }

    @Predicate(Signature = "atom_length/2", Template = {"+atom,?integer"}, Reference = "atom_length(Atom, Length) is true if and only if the integer Length equals the number of characters in the name of the atom Atom.")
    @Determined
    public static boolean predicateATOMLENGTH(final Goal goal, final TermStruct predicate) {
        Term left = Utils.getTermFromElement(predicate.getElement(0));
        final Term right = Utils.getTermFromElement(predicate.getElement(1));

        left = new TermInteger(left.getTextLength());
        return left.Equ(right);
    }

    @Predicate(Signature = "atom_codes/2", Template = {"+atom,?character_code_list", "?atom,+list"}, Reference = "atom_codes(Atom, List) succeeds if and only if List is a list whose elements are the character codes that in order correspond to the characters that make up  Atom.")
    @Determined
    public static boolean predicateATOMCHARCODES(final Goal goal, final TermStruct predicate) {
        Term left = Utils.getTermFromElement(predicate.getElement(0));
        Term right = Utils.getTermFromElement(predicate.getElement(1));

        switch (left.getTermType()) {
            case Term.TYPE_ATOM: {
                left = left.asCharCodeList();
                return left.Equ(right);
            }
            case Term.TYPE_LIST: {
                if (((TermList) left).isNullList()) {
                    left = new Term("[]").asCharCodeList();
                    return left.Equ(right);
                } else {
                    throw new ProlTypeErrorException("atom", predicate);
                }
            }
        }

        if (left.getTermType() == Term.TYPE_ATOM) {
            left = left.asCharCodeList();

            return left.Equ(right);
        }

        if (right.getTermType() == Term.TYPE_LIST) {
            final StringBuilder builder = new StringBuilder();

            TermList list = (TermList) right;
            while (list != TermList.NULLLIST) {
                final Term head = list.getHead();

                if (!(head instanceof TermInteger)) {
                    throw new ProlRepresentationErrorException("character_code", predicate);
                }

                builder.append((char) ((TermInteger) head).getNumericValue().intValue());

                final Term tail = list.getTail();
                if (tail.getTermType() == Term.TYPE_LIST) {
                    list = (TermList) tail;
                } else {
                    return false;
                }
            }

            right = new Term(builder.toString());
            return left.Equ(right);
        }

        return false;
    }

    @Predicate(Signature = "halt/1", Template = {"+integer"}, Reference = " These predicate terminate a Prolog engine and you can send the status of a cause.")
    @PredicateSynonyms(Signatures = {"halt/0"})
    @Determined
    public static void predicateHALT(final Goal goal, final TermStruct predicate) {
        if (predicate.getArity() == 0) {
            goal.getContext().halt();
            throw new ProlHaltExecutionException();
        } else {
            final int status = ((TermInteger) Utils.getTermFromElement(predicate.getElement(0))).getNumericValue().intValue();
            goal.getContext().halt();
            throw new ProlHaltExecutionException(status);
        }
    }

    @Predicate(Signature = "abolish/1", Template = {"@predicate_indicator"}, Reference = "abolish(Pred/2) is true. It has for side effect the removal of all clauses of the predicate indicated by Pred. After abolish/1 the predicate is not found by current_predicate.")
    @Determined
    public static boolean predicateABOLISH(final Goal goal, final TermStruct predicate) {
        final String signature = Utils.extractPredicateSignatureFromStructure((TermStruct) predicate.getElement(0));
        final KnowledgeBase base = goal.getContext().getKnowledgeBase();

        if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
            throw new ProlPermissionErrorException("modify", "static_procedure", new Term(signature));
        }

        base.abolish(signature);
        return true;
    }

    @Predicate(Signature = "sort/2", Template = {"+list,?list"}, Reference = "True if Sorted can be unified with a list holding the elements of List, sorted to the standard order of terms")
    @Determined
    @SuppressWarnings("unchecked")
    public static boolean predicateSORT(final Goal goal, final TermStruct predicate) {
        final Term nonsorted = Utils.getTermFromElement(predicate.getElement(0));
        final Term sorted = Utils.getTermFromElement(predicate.getElement(1));

        final Term[] bufferarray = Utils.listToArray((TermList) nonsorted);
        Arrays.sort(bufferarray, Utils.TERM_COMPARATOR);
        final TermList sortedList = Utils.arrayToList(bufferarray);

        return sorted.Equ(sortedList);
    }

    @Predicate(Signature = "findall/3", Template = {"?term,+callable_term,?list"}, Reference = "Creates  a list of the instantiations Template gets  successively on backtracking  over Goal and unifies the  result with Bag.")
    @Determined
    public static boolean predicateFINDALL(final Goal goal, final TermStruct predicate) throws InterruptedException {
        final Term template = Utils.getTermFromElement(predicate.getElement(0));
        final Term pgoal = Utils.getTermFromElement(predicate.getElement(1));
        final Term instances = Utils.getTermFromElement(predicate.getElement(2));

        final Goal find_goal = new Goal(pgoal, goal.getContext(), goal.getTracer());

        TermList result = null;
        TermList currentList = null;

        while (!Thread.currentThread().isInterrupted()) {
            final Term nextTemplate = find_goal.solve();

            if (nextTemplate == null) {
                break;
            }

            final Term templateCopy = template.makeClone();
            final Term pgoalCopy = pgoal.makeClone();
            Utils.arrangeVariablesInsideTerms(templateCopy, pgoalCopy);

            if (pgoalCopy.Equ(nextTemplate)) {
                // good, add to the list
                if (result == null) {
                    // first
                    result = new TermList(templateCopy);
                    currentList = result;
                } else {
                    // not first
                    currentList = TermList.appendItem(currentList, templateCopy);
                }
            } else {
                throw new ProlCriticalError("Impossible situation at findall/3!");
            }
        }

        if (result == null) {
            result = TermList.NULLLIST;
        }

        return instances.Equ(result);
    }

    @Predicate(Signature = "asserta/1", Template = {"@clause"}, Reference = "Addition of a clause into the knowlwde base before all other clauses.")
    @Determined
    public static boolean predicateASSERTA(final Goal goal, final TermStruct predicate) {
        final KnowledgeBase base = goal.getContext().getKnowledgeBase();

        Term atom = Utils.getTermFromElement(predicate.getElement(0));

        if (atom.getTermType() != Term.TYPE_STRUCT) {
            atom = new TermStruct(atom);
        }

        final String signature = ((TermStruct) atom).isFunctorLikeRuleDefinition() ? ((TermStruct) atom).getElement(0).getSignature() : ((TermStruct) atom).getSignature();

        // check that we doesn't overload any static system predicate
        if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
            throw new ProlPermissionErrorException("modify", "static_procedure", new Term(signature));
        }

        base.assertA((TermStruct) atom);
        return true;
    }

    @Predicate(Signature = "assertz/1", Template = {"@clause"}, Reference = "Addition of a clause into the knowlwde base after all other clauses.")
    @PredicateSynonyms(Signatures = "assert/1")
    @Determined
    public static boolean predicateASSERTZ(final Goal goal, final TermStruct predicate) {
        final KnowledgeBase base = goal.getContext().getKnowledgeBase();
        Term atom = Utils.getTermFromElement(predicate.getElement(0));

        if (atom.getTermType() != Term.TYPE_STRUCT) {
            atom = new TermStruct(atom);
        }

        final String signature = ((TermStruct) atom).isFunctorLikeRuleDefinition() ? ((TermStruct) atom).getElement(0).getSignature() : ((TermStruct) atom).getSignature();

        // check that we doesn't overload any static system predicate
        if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
            throw new ProlPermissionErrorException("modify", "static_procedure", new Term(signature));
        }

        base.assertZ((TermStruct) atom);

        return true;
    }

    @Predicate(Signature = "retract/1", Template = {"@clause"}, Reference = "Retract the first clause which can be unified with argument. True if there is such clause in the knowledge base.")
    @PredicateSynonyms(Signatures = "retracta/1")
    @Determined
    public static boolean predicateRETRACT(final Goal goal, final TermStruct predicate) {
        final KnowledgeBase base = goal.getContext().getKnowledgeBase();

        Term atom = Utils.getTermFromElement(predicate.getElement(0));

        if (atom.getTermType() != Term.TYPE_STRUCT) {
            atom = new TermStruct(atom);
        }

        final String signature = ((TermStruct) atom).isFunctorLikeRuleDefinition() ? ((TermStruct) atom).getElement(0).getSignature() : ((TermStruct) atom).getSignature();

        // check that we doesn't overload any static system predicate
        if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
            throw new ProlPermissionErrorException("modify", "static_procedure", new Term(signature));
        }

        return base.retractA((TermStruct) atom);
    }

    @Predicate(Signature = "retractz/1", Template = {"@clause"}, Reference = "Retract the last clause which can be unified with argument. True if there is such clause in the knowledge base.")
    @Determined
    public static boolean predicateRETRACTZ(final Goal goal, final TermStruct predicate) {
        final KnowledgeBase base = goal.getContext().getKnowledgeBase();

        Term atom = Utils.getTermFromElement(predicate.getElement(0));

        if (atom.getTermType() != Term.TYPE_STRUCT) {
            atom = new TermStruct(atom);
        }

        final String signature = ((TermStruct) atom).isFunctorLikeRuleDefinition() ? ((TermStruct) atom).getElement(0).getSignature() : ((TermStruct) atom).getSignature();

        // check that we doesn't overload any static system predicate
        if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
            throw new ProlPermissionErrorException("modify", "static_procedure", new Term(signature));
        }

        return base.retractZ((TermStruct) atom);
    }

    @Predicate(Signature = "retractall/1", Template = {"@clause"}, Reference = "Retract all clauses which can be unified with argument. True if there is as minimum one clause in the knowledge base.")
    @Determined
    public static boolean predicateRETRACTALL(final Goal goal, final TermStruct predicate) {
        final KnowledgeBase base = goal.getContext().getKnowledgeBase();
        Term atom = Utils.getTermFromElement(predicate.getElement(0));

        if (atom.getTermType() != Term.TYPE_STRUCT) {
            atom = new TermStruct(atom);
        }

        final String signature = ((TermStruct) atom).isFunctorLikeRuleDefinition() ? ((TermStruct) atom).getElement(0).getSignature() : ((TermStruct) atom).getSignature();

        // check that we doesn't overload any static system predicate
        if (goal.getContext().hasPredicateAtLibraryForSignature(signature)) {
            throw new ProlPermissionErrorException("modify", "static_procedure", new Term(signature));
        }

        return base.retractAll((TermStruct) atom);
    }

    @Predicate(Signature = "catch/3", Template = "+callable_term,?term,+callable_term", Reference = "A goal catch(Goal, Catcher, Handler) is true if\n1. call(Goal) is true, or\n2. An exception is raised which throws a Ball that is caught by Catcher and Handler then succeeds ")
    public static boolean predicateCATCH(final Goal goal, final TermStruct predicate) throws InterruptedException {
        Goal catchGoal = (Goal) goal.getAuxObject();

        final Term catching = Utils.getTermFromElement(predicate.getElement(0));
        final Term catcher = Utils.getTermFromElement(predicate.getElement(1));
        final Term solver = Utils.getTermFromElement(predicate.getElement(2));

        if (catchGoal == null) {
            catchGoal = new Goal(catching, goal.getContext(), goal.getTracer());
            goal.setAuxObject(catchGoal);
        }

        if (catchGoal.getGoalTerm() == solver) {
            final Term result = catchGoal.solve();

            if (result == null) {
                goal.noMoreVariants();
                return false;
            } else {
                if (catchGoal.isCompleted()) {
                    goal.noMoreVariants();
                }
                return true;
            }
        } else {

            try {
                final Term result = catchGoal.solve();
                if (result == null) {
                    goal.noMoreVariants();
                    return false;
                } else {
                    if (catchGoal.isCompleted()) {
                        goal.noMoreVariants();
                    }
                    return true;
                }
            } catch (ProlAbstractCatcheableException ex) {
                // exception was thrown
                if (catcher.Equ(ex.getAsStruct())) {
                    catchGoal = new Goal(solver, goal.getContext(), goal.getTracer());
                    goal.setAuxObject(catchGoal);
                    final Term result = catchGoal.solve();
                    if (result == null) {
                        goal.noMoreVariants();
                        return false;
                    } else {
                        if (catchGoal.isCompleted()) {
                            goal.noMoreVariants();
                        }
                        goal.setAuxObject(catchGoal);
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
    public static void predicateTHROW(final Goal goal, final TermStruct predicate) {
        final Term term = Utils.getTermFromElement(predicate.getElement(0));
        final String exceptionSignature = term.getSignature();

        LOG.info("throw/1 " + exceptionSignature);

        if ("instantiation_error/0".equals(exceptionSignature)) {
            throw new ProlInstantiationErrorException(predicate);
        }

        if ("type_error/2".equals(exceptionSignature)) {
            final TermStruct struct = predicate;
            throw new ProlTypeErrorException(struct.getElement(0).forWrite(), struct.getElement(1));
        }

        if ("domain_error/2".equals(exceptionSignature)) {
            final TermStruct struct = predicate;
            throw new ProlDomainErrorException(struct.getElement(0).forWrite(), struct.getElement(1));
        }

        if ("permission_error/3".equals(exceptionSignature)) {
            final TermStruct struct = predicate;
            throw new ProlPermissionErrorException(struct.getElement(0).forWrite(), struct.getElement(1).forWrite(), struct.getElement(2));
        }

        if ("representation_error/1".equals(exceptionSignature)) {
            final TermStruct struct = predicate;
            throw new ProlRepresentationErrorException(struct.getElement(0).forWrite(), predicate);
        }

        if ("evaluation_error/1".equals(exceptionSignature)) {
            final TermStruct struct = predicate;
            throw new ProlEvaluationErrorException(struct.getElement(0).forWrite(), predicate);
        }

        // all other errors make as custom
        //-------------------------------------
        Term arg = predicate.getElement(0);
        if (arg.getTermType() != Term.TYPE_STRUCT) {
            arg = new TermStruct(arg);
        }
        throw new ProlCustomErrorException(arg, predicate);
    }

    @Predicate(Signature = "beep/0", Reference = "Make a short sound. It depends on the OS.")
    @Determined
    public static void predicateBEEP(final Goal goal, final TermStruct predicate) {
        Toolkit.getDefaultToolkit().beep();
    }

    @Predicate(Signature = "pause/1", Template = {"+number"}, Reference = "Make a pause for defined millisecond number.")
    @Determined
    public static void predicatePAUSE(final Goal goal, final TermStruct predicate) throws InterruptedException {
        final NumericTerm term = (NumericTerm) predicate.getElement(0);

        final int milliseconds = term.getNumericValue().intValue();
        if (milliseconds > 0) {

            Thread.sleep(milliseconds);
        }
    }

    // inside function used by facts/1 and rules/1 predicates
    private static TermStruct processIterator(final Goal goal, final ClauseIterator iterator) {
        TermStruct result = null;

        if (iterator.hasNext()) {
            result = iterator.next();
        } else {
            goal.noMoreVariants();
        }

        return result;
    }

    @Predicate(Signature = "facts/1", Template = {"+callable_term"}, Reference = "Finds only facts at the knowledge base.")
    public static boolean predicateFACTS(final Goal goal, final TermStruct predicate) {
        FactIterator factIterator = (FactIterator) goal.getAuxObject();
        final Term origterm = Utils.getTermFromElement(predicate.getElement(0));

        if (factIterator == null) {
            Term term = origterm;
            if (term.getTermType() == Term.TYPE_ATOM) {
                // we have make the term as a struct
                term = new TermStruct(term);
            }

            // it's the first call so we have to get fact iterator from the base
            final KnowledgeBase base = goal.getContext().getKnowledgeBase();
            factIterator = base.getFactIterator((TermStruct) term);

            if (factIterator == null) {
                goal.noMoreVariants();
                return false;
            } else {
                goal.setAuxObject(factIterator);
            }
        }

        boolean result = false;

        final TermStruct nextFact = processIterator(goal, factIterator);
        if (nextFact == null) {
            goal.setAuxObject(null);
            goal.noMoreVariants();
        } else if (origterm.getTermType() == Term.TYPE_ATOM) {
            result = true;
        } else {
            if (!origterm.Equ(nextFact)) {
                throw new ProlCriticalError("Critical error, not Equ!");
            }
            result = true;
        }

        return result;
    }

    @Predicate(Signature = "rules/1", Template = {"+callable_term"}, Reference = "Finds and call only rules at the knowledge base.")
    public static boolean predicateRULES(final Goal goal, final TermStruct predicate) throws InterruptedException {

        RuleAuxObject ruleAuxObject = (RuleAuxObject) goal.getAuxObject();
        if (ruleAuxObject == null) {
            Term term = Utils.getTermFromElement(predicate.getElement(0));

            if (term.getTermType() == Term.TYPE_ATOM) {
                // atom, we have to make a struct
                term = new TermStruct(term);
            }

            // it's the first call so we have to get fact iterator from the base
            final KnowledgeBase base = goal.getContext().getKnowledgeBase();
            final RuleIterator ruleIterator = base.getRuleIterator((TermStruct) term);

            if (ruleIterator == null) {
                goal.noMoreVariants();
                return false;
            } else {
                ruleAuxObject = new RuleAuxObject(ruleIterator);
                goal.setAuxObject(ruleAuxObject);
            }
        }

        boolean result = false;

        while (!Thread.currentThread().isInterrupted()) {
            final Goal currentGoal = ruleAuxObject.currentActiveGoal;
            if (currentGoal == null) {
                final TermStruct nextRule = processIterator(goal, ruleAuxObject.iterator);
                if (nextRule == null) {
                    // end
                    goal.setAuxObject(null);
                    goal.noMoreVariants();
                    break;
                }

                ruleAuxObject.rule = nextRule;
                ruleAuxObject.currentActiveGoal = new Goal(nextRule, goal.getContext(), goal.getTracer());
            } else {
                final Term goalresult = currentGoal.solve();
                if (goalresult == null) {
                    // end for the goal
                    ruleAuxObject.currentActiveGoal = null;
                } else {
                    // solved

                    final Term term = Utils.getTermFromElement(predicate.getElement(0));

                    if (term.getTermType() == Term.TYPE_STRUCT) {
                        final TermStruct ruleClone = (TermStruct) ruleAuxObject.rule.makeClone();
                        if (!term.Equ(ruleClone.getElement(0))) {
                            // error critical situation
                            throw new ProlCriticalError("Can't make Equ, impossible case!");
                        }
                    }

                    result = true;
                    break;
                }
            }
        }

        return result;
    }

    @Predicate(Signature = "lock/1", Template = {"+atom"}, Reference = "Block current thread until it will be possible to lock an atom, don't forget unlock.")
    @Determined
    public static void predicateLOCK(final Goal goal, final TermStruct predicate) {
        final String atomName = predicate.getElement(0).getText();
        goal.getContext().lockLockerForName(atomName);
    }

    @Predicate(Signature = "unlock/1", Template = {"+atom"}, Reference = "Unlock a locker for its name and allow to continue work of waiting threads. If any other thread is the owner for the locker then permission_error/3 will be thrown.")
    @Determined
    public static void predicateUNLOCK(final Goal goal, final TermStruct predicate) {
        final String atomName = predicate.getElement(0).getText();

        try {
            goal.getContext().unlockLockerForName(atomName);
        } catch (IllegalArgumentException ex) {
            final ProlExistenceErrorException exx = new ProlExistenceErrorException("locker", "unlock", predicate, ex);
            LOG.log(Level.SEVERE, "unlock/1, unknown locker name", ex);
            LOG.throwing(ProlCoreLibrary.class.getCanonicalName(), "predicateUNLOCK()", exx);
            throw exx;
        } catch (IllegalMonitorStateException ex) {
            LOG.log(Level.SEVERE, "unlock/1, wrong monitor state", ex);
            final ProlPermissionErrorException exx = new ProlPermissionErrorException("locker", "unlock", predicate, ex);
            LOG.throwing(ProlCoreLibrary.class.getCanonicalName(), "predicateUNLOCK()", exx);
            throw exx;
        }
    }

    @Predicate(Signature = "trylock/1", Template = {"+atom"}, Reference = "Try make lock for a named locker, if it is being locked already then fail else success.")
    @Determined
    public static boolean predicateTRYLOCK(final Goal goal, final TermStruct predicate) {
        final String atomName = predicate.getElement(0).getText();
        return goal.getContext().trylockLockerForName(atomName);
    }

    @Predicate(Signature = "async/1", Template = {"+callable_term"}, Reference = "Allows to solve a goal asynchronously, it will be started as a daemon so it will be stopped when the main goal will be solved or failed. If there will be uncatched exception it will be just out at the log.")
    @Determined
    public static void predicateASYNC(final Goal goal, final TermStruct predicate) {
        final Term goalToSolve = Utils.getTermFromElement(predicate.getElement(0));
        final ProlContext context = goal.getContext();

        // we have to check the goal because it must not have any noninstantiated variable!!!!
        final Map<String, Var> vars = Utils.fillTableWithVars(goalToSolve);
        for (Var var : vars.values()) {
            if (!var.isAnonymous() && var.isUndefined()) {
                throw new ProlInstantiationErrorException("Variable \'" + var.getText() + "\' is not instantiated, you must have all variables instantiated for async/1 .", predicate);
            }
        }

        context.solveAsynchronously(goalToSolve, goal.getTracer());
    }

    @Predicate(Signature = "waitasync/0", Reference = "Blocking waiting until all daemon threads (started with either fork/1 or async/1) in the context will be completed and deactivated (it checks the queue of the threads and their activity). Always true.")
    @Determined
    public static void predicateWAITASYNC(final Goal goal, final TermStruct predicate) throws InterruptedException {
        final ThreadPoolExecutor service = goal.getContext().getContextExecutorService();

        while (!Thread.currentThread().isInterrupted() && (!service.getQueue().isEmpty() || service.getActiveCount() > 0)) {
            Thread.sleep(1);
        }
    }

    @SuppressWarnings("unchecked")
    private static List<Future<Term>> startListAsFork(final Goal goal, final TermStruct predicate, final TermList termlist) throws InterruptedException {
        List<AuxForkTask> goalList = (List<AuxForkTask>) goal.getAuxObject();

        if (goalList == null) {
            Set<Integer> varFlagTable = null; // the lazy initing map allows us to find out that there are non instantiated shared variables

            // the first call
            goalList = new ArrayList<>();
            TermList tlist = termlist;
            final ProlContext context = goal.getContext();
            while (!tlist.isNullList()) {
                final Term term = tlist.getHead();

                // ---- we have to check that user doesn't try share noninstantiated variables between parallel goals
                if (term.getTermType() != Term.TYPE_ATOM) {
                    // find vars
                    final Map<String, Var> varTable = Utils.fillTableWithVars(term);
                    if (!varTable.isEmpty()) {
                        if (varFlagTable == null) {
                            varFlagTable = new HashSet<>();
                        }
                        for (final Map.Entry<String, Var> pair : varTable.entrySet()) {
                            final Var variable = pair.getValue();
                            if (!variable.isAnonymous() && variable.isUndefined()) {
                                // we check only undefined vars
                                final Integer varUID = variable.getVarUID();
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
                if (tail.getTermType() == Term.TYPE_LIST) {
                    tlist = (TermList) tail;
                } else {
                    break;
                }
            }
            goal.setAuxObject(goalList);
        }

        List<Future<Term>> resultList = new ArrayList<>();

        final ExecutorService executor = goal.getContext().getContextExecutorService();

        for (final AuxForkTask task : goalList) {
            resultList.add(executor.submit(task));
        }

        return resultList;
    }

    @Predicate(Signature = "fork/1", Template = {"+list"}, Reference = "Allows to prove a few goals (non linked between each other) in separated threads simultaneously, it is blocking the calling thread until all threads (started by the predicate) are completed. The fork implements AND operation (i.e. all goals have to be true else the predicate will fail).You must not have the same noninstantiated variables in terms that will be executed in different threads. The fork_error/1 will be thrown if any thread will throw an exception.")
    public static boolean predicateFORK(final Goal goal, final TermStruct predicate) throws InterruptedException {
        TermList termlist = (TermList) Utils.getTermFromElement(predicate.getElement(0));

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
                    if (!head.Equ(resultOfTask)) {
                        final ProlCriticalError err = new ProlCriticalError("Impossible situation, the proven fork task goal is not equal the etalon task goal.");
                        LOG.throwing(ProlCoreLibrary.class.getCanonicalName(), "fork/1", err);
                        throw err;
                    }
                }
            } catch (ExecutionException ex) {
                LOG.log(Level.SEVERE, "predicateFORK()[" + predicate.toString() + "] task index=" + taskindex, ex.getCause() == null ? ex : ex.getCause());

                if (forkExceptions == null) {
                    forkExceptions = new ArrayList<>();
                }

                forkExceptions.add(ex);
            }

            final Term tail = termlist.getTail();
            if (tail.getTermType() == Term.TYPE_LIST) {
                termlist = (TermList) tail;
            } else {
                break;
            }

            taskindex++;
        }

        if (forkExceptions != null) {

            final ProlForkExecutionException ex = new ProlForkExecutionException(predicate, forkExceptions.toArray(new Throwable[forkExceptions.size()]));
            LOG.throwing(ProlCoreLibrary.class.getCanonicalName(), "fork/1", ex);
            throw ex;
        }

        if (!result) {
            goal.noMoreVariants();
        }

        return result;
    }

    @Predicate(Signature = "ifork/1", Template = {"+list"}, Reference = "It works like fork/1 but it will interrupt all noncompleted threads of the fork if any proven result is fail.")
    public static boolean predicateIFORK(final Goal goal, final TermStruct predicate) throws InterruptedException {
        final TermList termlist = (TermList) Utils.getTermFromElement(predicate.getElement(0));

        // invoke all taska and wait for them all
        final List<Future<Term>> taskList = startListAsFork(goal, predicate, termlist);

        boolean result = true;

        Exception forkException = null;
        Term termThrowsException = null;

        // parse the list for terms
        final Term[] parsedgoal = Utils.listToArray(termlist);

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
                            if (!originalGoal.Equ(resultterm)) {
                                final ProlCriticalError err = new ProlCriticalError("Impossible situation, the proven fork task goal is not equal the etalon task goal. [index=" + threadindex + ']');
                                LOG.throwing(ProlCoreLibrary.class.getCanonicalName(), "ifork/1", err);
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
                for (final Future<Term> task : workingThreads.values()) {
                    task.cancel(true);
                }
            }
        }

        if (forkException != null) {

            final ProlForkExecutionException ex = new ProlForkExecutionException(termThrowsException, new Throwable[]{forkException});
            LOG.throwing(ProlCoreLibrary.class.getCanonicalName(), "ifork/1", ex);
            throw ex;
        }

        if (!result) {
            goal.noMoreVariants();
        }

        return result;
    }

    @Predicate(Signature = "regtrigger/3", Template = {"+predicate_indicator,+triggerevent,+callable_term"}, Reference = "regtrigger(somepredicate/3,onassert,triggerhandler) is always true. The predicate allows to register a trigger handler for distinguished predicate signature. The handled trigger event can be selected from the list [onassert, onretract, onassertretract].")
    @Determined
    public static boolean predicateREGTRIGGER(final Goal goal, final TermStruct predicate) {
        final String signature = Utils.extractPredicateSignatureFromStructure((TermStruct) predicate.getElement(0));
        final String triggerevent = Utils.getTermFromElement(predicate.getElement(1)).getText();
        final Term callableTerm = Utils.getTermFromElement(predicate.getElement(2));
        final ProlContext context = goal.getContext();

        final ProlTriggerGoal triggergoal = new ProlTriggerGoal(callableTerm, context, goal.getTracer());

        if (null == triggerevent) {
          throw new ProlCriticalError("Unsupported trigger event detected [" + triggerevent + ']');
        } else switch (triggerevent) {
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

        context.registerTrigger(triggergoal);

        return true;
    }

    @Predicate(Signature = "nl/0", Reference = "Out the next line char symbol into current output stream")
    @Determined
    public final void predicateNL(final Goal goal, final TermStruct predicate) {
        final ProlTextWriter outStream = goal.getContext().getCurrentOutStream();
        if (outStream == null) {
            return;
        }
        try {
            outStream.writeChar(NEXT_LINE);
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
        }
    }

    @Predicate(Signature = "tab/1", Template = {"+integer"}, Reference = "Out a number of space symbols into current output stream")
    @Determined
    public final void predicateTAB(final Goal goal, final TermStruct predicate) {
        final ProlTextWriter outStream = goal.getContext().getCurrentOutStream();
        final int spaces = ((NumericTerm) predicate.getElement(0)).getNumericValue().intValue();

        if (outStream == null) {
            return;
        }
        try {
            for (int li = 0; li < spaces; li++) {
                outStream.writeChar(SPACE);
            }
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
        }
    }

    @Predicate(Signature = "copy_term/2", Template = {"?term,?term"}, Reference = "copy_term(X,Y) is true if and only if Y unifies with a term T which is a renamed copy of X.")
    @Determined
    public final boolean predicateCOPYTERM(final Goal goal, final TermStruct predicate) {
        final Term left = Utils.getTermFromElement(predicate.getElement(0)).makeClone();
        final Term right = Utils.getTermFromElement(predicate.getElement(1));
        return right.Equ(left);
    }

    @Predicate(Signature = "time/1", Template = "+callable_term", Reference = "Execute  Goal just but  print used time, It supports choice point (!) for inside goal.")
    public final boolean predicateTime(final Goal goal, final TermStruct predicate) throws InterruptedException {
        long time = System.nanoTime();
        final boolean result = predicateCALL(goal, predicate);

        time = ((System.nanoTime() - time) + 500) / 1000; //microseconds

        final ProlTextWriter outStream = goal.getContext().getCurrentOutStream();

        if (outStream != null) {
            try {
                outStream.writeTerm(new Term("% " + (time / 1000) + '.' + (time % 1000) + " millisec \n"));
            } catch (IOException ex) {
                throw new ProlPermissionErrorException("write", "text_output", predicate);
            }
        }
        if (!result) {
            goal.noMoreVariants();
        }
        return result;
    }

    @Predicate(Signature = "\\+/1", Template = "+callable_term", Reference = "\\+(Term) is true if and only if call(Term) is false.")
    @Determined
    public final boolean predicateCannotBeProven(final Goal goal, final TermStruct predicate) throws InterruptedException {
        final Term argument = predicate.getElement(0);
        final Goal subgoal = new Goal(argument, goal.getContext(), goal.getTracer());
        return subgoal.solve() == null;
    }

    @Predicate(Signature = "time/4", Template = {"?integer,?integer,?integer,?integer"}, Reference = "Get current time Hours,Minutes,Seconds,Milliseconds.")
    @Determined
    public final boolean predicateTIME(final Goal goal, final TermStruct predicate) {
        final Calendar date = Calendar.getInstance();
        final TermInteger hours = new TermInteger(date.get(Calendar.HOUR_OF_DAY));
        final TermInteger minutes = new TermInteger(date.get(Calendar.MINUTE));
        final TermInteger seconds = new TermInteger(date.get(Calendar.SECOND));
        final TermInteger milliseconds = new TermInteger(date.get(Calendar.MILLISECOND));

        return predicate.getElement(0).Equ(hours) && predicate.getElement(1).Equ(minutes) && predicate.getElement(2).Equ(seconds) && predicate.getElement(3).Equ(milliseconds);
    }

    @Predicate(Signature = "date/3", Template = {"?integer,?integer,?integer"}, Reference = "Get current date Year, Month, Day. The January is 1st month")
    @Determined
    public final boolean predicateDATE(final Goal goal, final TermStruct predicate) {
        final Calendar date = Calendar.getInstance();
        final TermInteger year = new TermInteger(date.get(Calendar.YEAR));
        final TermInteger month = new TermInteger(date.get(Calendar.MONTH) + 1);
        final TermInteger day = new TermInteger(date.get(Calendar.DAY_OF_MONTH));

        return predicate.getElement(0).Equ(year) && predicate.getElement(1).Equ(month) && predicate.getElement(2).Equ(day);
    }

    @Predicate(Signature = "write/1", Reference = "Write a term into the current output stream.")
    @Determined
    public final void predicateWrite(final Goal goal, final TermStruct predicate) {
        final ProlTextWriter outStream = goal.getContext().getCurrentOutStream();
        if (outStream == null) {
            return;
        }
        try {
            outStream.writeTerm(predicate.getElement(0));
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
        }
    }

    @Predicate(Signature = "put/1", Template = "+number", Reference = "Write a char for its code into the current output stream.")
    @Determined
    public final void predicatePUT(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final ProlTextWriter outStream = goal.getContext().getCurrentOutStream();
        if (outStream == null) {
            return;
        }
        try {
            outStream.writeChar(arg);
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
        }
    }

    @Predicate(Signature = "get/1", Template = "?number", Reference = "Read next non-blank char code from the current input stream.")
    @Determined
    public final boolean predicateGET(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final ProlTextReader inStream = goal.getContext().getCurrentInputStream();
        if (inStream == null) {
            throw new ProlPermissionErrorException("read", "text_input", predicate);
        }
        try {
            final Term nextchar = inStream.readChar();
            return arg.Equ(nextchar);
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
        }
    }

    @Predicate(Signature = "get0/1", Template = "?number", Reference = "Read next char code from the current input stream.")
    @Determined
    public final boolean predicateGET0(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final ProlTextReader inStream = goal.getContext().getCurrentInputStream();
        if (inStream == null) {
            throw new ProlPermissionErrorException("read", "text_input", predicate);
        }
        try {
            while (!Thread.currentThread().isInterrupted()) {
                final TermInteger nextchar = inStream.readChar();
                final int num = nextchar.getNumericValue().intValue();
                if (num >= 0 && Character.isSpaceChar((char) num)) {
                    continue;
                }
                return arg.Equ(nextchar);
            }
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
        }
        
        return false;
    }

    @Predicate(Signature = "read/1", Reference = " Read  the next Prolog term from the current input stream.")
    @Determined
    public final boolean predicateRead(final Goal goal, final TermStruct predicate) throws InterruptedException {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final ProlTextReader outStream = goal.getContext().getCurrentInputStream();
        if (outStream == null) {
            throw new ProlPermissionErrorException("read", "text_input", predicate);
        }
        try {
            Term term = outStream.readTerm();
            if (term == null) {
                term = ProlStream.END_OF_FILE;
            }
            return arg.Equ(term);
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("read", "text_input", predicate, ex);
        }
    }

    private TermInteger readChar(final Goal goal, final TermStruct predicate) {
        final ProlTextReader outStream = goal.getContext().getCurrentInputStream();
        if (outStream == null) {
            throw new ProlPermissionErrorException("read", "text_input", predicate);
        }

        try {
          return outStream.readChar();
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("read", "text_input", predicate, ex);
        }
    }
    
    private String readFromCurrentInputStreamUntilNL(final Goal goal, final TermStruct predicate) {
        final ProlTextReader outStream = goal.getContext().getCurrentInputStream();
        if (outStream == null) {
            throw new ProlPermissionErrorException("read", "text_input", predicate);
        }

        try {
            final StringBuilder builder = new StringBuilder();
            boolean working = true;
            while (working) {
                final TermInteger integer = outStream.readChar();
                final int code = integer.getNumericValue().intValue();
                switch (code) {
                    case -1: {
                        if (builder.length() <= 0) {
                            builder.setLength(0);
                            builder.append(ProlStream.END_OF_FILE_STR);
                        }
                        working = false;
                    }
                    break;
                    case '\r': {
                        // ignore
                    }
                    break;
                    case '\n': {
                        working = false;
                    }
                    break;
                    case 8: // backspace
                    {
                        if (builder.length() > 0) {
                            builder.setLength(builder.length() - 1);
                        }
                    }
                    break;
                    default: {
                        builder.append((char) code);
                    }
                    break;
                }
            }

            return builder.toString();
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("read", "text_input", predicate, ex);
        }

    }

    @Predicate(Signature = "readln/1", Reference = " Read  the next line (until NL symbol) from the current input stream as an atom. It sypports backspace to remove last symbol from buffer.")
    @Determined
    public final boolean predicateReadLn(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        return arg.Equ(new Term(readFromCurrentInputStreamUntilNL(goal, predicate)));
    }

    @Predicate(Signature = "readint/1", Reference = " Read  an integer number (and ignore white space) until NL symbol from the current input stream as an integer atom or the end_of_file atom. It sypports backspace to remove last symbol from buffer. If the input string can't be converted to an integer atom, the predicate will return false.")
    @Determined
    public final boolean predicateReadInt(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final String str = readFromCurrentInputStreamUntilNL(goal, predicate).trim();
        Term term;
        if (str.equals(ProlStream.END_OF_FILE_STR)) {
            term = ProlStream.END_OF_FILE;
        } else {
            try {
                term = new TermInteger(str);
            } catch (NumberFormatException ex) {
                return false;
            }
        }
        return arg.Equ(term);
    }

    @Predicate(Signature = "readchar/1", Reference = " Read  char from the current input stream as an integer atom or the end_of_file atom")
    @Determined
    public final boolean predicateReadChar(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final TermInteger value = readChar(goal, predicate);
        Term term = value.getValue()<0 ? ProlStream.END_OF_FILE : value;
        return arg.Equ(term);
    }

    @Predicate(Signature = "readreal/1", Reference = " Read  an real number (and ignore white space) until NL symbol from the current input stream as an real atom or the end_of_file atom. It sypports backspace to remove last symbol from buffer. If the input string can't be converted to a real atom, the predicate will return false.")
    @Determined
    public final boolean predicateReadReal(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final String str = readFromCurrentInputStreamUntilNL(goal, predicate).trim();
        final Term term;
        if (str.equals(ProlStream.END_OF_FILE_STR)) {
            term = ProlStream.END_OF_FILE;
        } else {
            try {
                term = new TermFloat(str);
            } catch (NumberFormatException ex) {
                return false;
            }
        }
        return arg.Equ(term);
    }

    @Predicate(Signature = "see/1", Template = "+atom", Reference = "Open SrcDest for reading and make it the current input")
    @Determined
    public final void predicateSEE(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final String name = arg.getText();
        try {
            goal.getContext().see(name);
        } catch (IOException ex) {
            if (ex instanceof FileNotFoundException) {
                throw new ProlExistenceErrorException("source_sink", predicate, ex);
            } else {
                throw new ProlPermissionErrorException("create", "text_stream", predicate, ex);
            }
        }
    }

    @Predicate(Signature = "seen/0", Reference = "Close the current input stream.")
    @Determined
    public final void predicateSEEN(final Goal goal, final TermStruct predicate) {
        try {
            goal.getContext().seen();
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("close", "text_stream", predicate, ex);
        }
    }

    @Predicate(Signature = "seeing/1", Template = "?term", Reference = "Return the current input stream name.")
    @Determined
    public final boolean predicateSEEING(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final ProlTextReader inStream = goal.getContext().getCurrentInputStream();
        Term result = TermList.NULLLIST;
        if (inStream != null) {
            result = new Term(inStream.getResourceId());
        }
        return arg.Equ(result);
    }

    @Predicate(Signature = "telling/1", Template = "?term", Reference = "Return the current output stream name.")
    @Determined
    public final boolean predicateTELLING(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final ProlTextWriter outStream = goal.getContext().getCurrentOutStream();
        Term result = TermList.NULLLIST;
        if (outStream != null) {
            result = new Term(outStream.getResourceId());
        }
        return arg.Equ(result);
    }

    @Predicate(Signature = "told/0", Reference = "Close the current output stream.")
    @Determined
    public final void predicateTOLD(final Goal goal, final TermStruct predicate) {
        try {
            goal.getContext().told();
        } catch (IOException ex) {
            throw new ProlPermissionErrorException("close", "text_stream", predicate, ex);
        }
    }

    @Predicate(Signature = "tell/1", Template = "+atom", Reference = "Open SrcDest for writing and make it the current output")
    @Determined
    public final void predicateTELL(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final String name = arg.getText();
        try {
            goal.getContext().tell(name, false);
        } catch (IOException ex) {
            if (ex instanceof FileNotFoundException) {
                throw new ProlExistenceErrorException("source_sink", predicate, ex);
            } else {
                throw new ProlPermissionErrorException("create", "text_stream", predicate, ex);
            }
        }
    }

    @Predicate(Signature = "append/1", Template = "+atom", Reference = "Open SrcDest to append new data and make it the current input")
    @Determined
    public final void predicateAPPEND(final Goal goal, final TermStruct predicate) {
        final Term arg = Utils.getTermFromElement(predicate.getElement(0));
        final String name = arg.getText();
        try {
            goal.getContext().tell(name, true);
        } catch (IOException ex) {
            if (ex instanceof FileNotFoundException) {
                throw new ProlExistenceErrorException("source_sink", predicate, ex);
            } else {
                throw new ProlPermissionErrorException("create", "text_stream", predicate, ex);
            }
        }
    }

    /**
     * Inside auxiliary object-helper for facts/1 and rules/1 predicates, it saves
     * intermediate data
     */
    private static class RuleAuxObject {

        private final RuleIterator iterator;
        private Goal currentActiveGoal;
        private TermStruct rule;

        RuleAuxObject(final RuleIterator iterator) {
            this.iterator = iterator;
        }
    }

    /**
     * The class is an auxuliary class for the fork/1 predicate, it describes a
     * task for the predicate
     */
    private static class AuxForkTask implements Callable<Term> {

        private final Term term;
        private final Goal goal;

        public AuxForkTask(final Term termToSolve, final ProlContext context) {
            this.term = Utils.getTermFromElement(termToSolve.makeClone()); // we need to isolate the value to avoid any change the object

            if (termToSolve.getTermType() == Term.TYPE_VAR) {
                this.goal = null;
            } else {
                this.goal = new Goal(this.term, context, null);
            }
        }

        public Goal getGoal() {
            return goal;
        }

        public Term getTerm() {
            return term;
        }

        @Override
        public Term call() throws InterruptedException {
            return this.goal == null ? null : this.goal.solve();
        }

        @Override
        public String toString() {
            return term.toString();
        }
    }
}
