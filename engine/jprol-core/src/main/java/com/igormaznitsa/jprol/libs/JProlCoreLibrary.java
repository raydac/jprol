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

import static com.igormaznitsa.jprol.data.SourcePosition.UNKNOWN;
import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;
import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.TermType.STRUCT;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.EMPTY_LIST_ATOM;
import static com.igormaznitsa.jprol.data.Terms.LIST_FUNCTOR;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newDouble;
import static com.igormaznitsa.jprol.data.Terms.newList;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertCharacterCode;
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertInteger;
import static com.igormaznitsa.jprol.utils.ProlUtils.createOrAppendToList;
import static com.igormaznitsa.jprol.utils.ProlUtils.makeContextAwareGlobalValueName;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.FY;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFX;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.XFY;
import static com.igormaznitsa.prologparser.tokenizer.OpAssoc.YFX;

import com.igormaznitsa.jprol.annotations.JProlConsultText;
import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlAbstractCatchableException;
import com.igormaznitsa.jprol.exceptions.ProlChoicePointInterruptedException;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlCustomErrorException;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlKnowledgeBaseException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.exceptions.ProlRepresentationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.JProlTreeBuilder;
import com.igormaznitsa.jprol.logic.KeyValueTermStore;
import com.igormaznitsa.jprol.logic.triggers.JProlContextTriggerGoalCaller;
import com.igormaznitsa.jprol.logic.triggers.JProlTriggerType;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.ProlUtils;
import com.igormaznitsa.jprol.utils.lazy.LazySet;
import com.igormaznitsa.prologparser.exceptions.PrologParserException;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.Charset;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Common core predicates.
 *
 * @since 3.0.0
 */
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
@JProlOperator(priority = 700, type = XFX, name = "@=")
@JProlOperator(priority = 700, type = XFX, name = "=@=")
@JProlOperator(priority = 700, type = XFX, name = "@<")
@JProlOperator(priority = 700, type = XFX, name = "@>")
@JProlOperator(priority = 700, type = XFX, name = "@=<")
@JProlOperator(priority = 700, type = XFX, name = "@>=")
@JProlOperator(priority = 700, type = XFX, name = "=:=")
@JProlOperator(priority = 700, type = XFX, name = "=..")
@JProlOperator(priority = 500, type = YFX, name = "/\\")
@JProlOperator(priority = 500, type = YFX, name = "\\/")
@JProlOperator(priority = 500, type = YFX, name = "xor")
@JProlOperator(priority = 500, type = YFX, name = "+")
@JProlOperator(priority = 500, type = YFX, name = "-")
@JProlOperator(priority = 500, type = FX, name = "+")
@JProlOperator(priority = 500, type = FX, name = "-")
@JProlOperator(priority = 400, type = YFX, name = "*")
@JProlOperator(priority = 400, type = YFX, name = "/")
@JProlOperator(priority = 400, type = YFX, name = "//")
@JProlOperator(priority = 400, type = YFX, name = "div")
@JProlOperator(priority = 400, type = YFX, name = "rdiv")
@JProlOperator(priority = 400, type = YFX, name = "rem")
@JProlOperator(priority = 400, type = YFX, name = "mod")
@JProlOperator(priority = 400, type = YFX, name = "<<")
@JProlOperator(priority = 400, type = YFX, name = ">>")
@JProlOperator(priority = 400, type = YFX, name = "mod")
@JProlOperator(priority = 200, type = FY, name = "\\")
@JProlOperator(priority = 200, type = XFX, name = "**")
@JProlOperator(priority = 200, type = XFY, name = "^")
@JProlConsultText(
    value = "forall(Generator, Test) :- \\+ (Generator, \\+ Test). " +
        "append([], Zs, Zs). append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs). " +
        "member(X,[X|_]). member(X,[A|Rest]):-member(X,Rest). " +
        "between(Lower, Upper, Lower) :- Lower =< Upper. between(Lower1, Upper, X) :- Lower1 < Upper, Lower2 is Lower1 + 1, between(Lower2, Upper, X)."
    ,
    declaredPredicates = {
        @JProlPredicate(signature = "between/3", validate = {
            "+integer,+integer,?integer"}, reference = "between(Lower, Upper, X) is true if X is greater than or equal to Lower, and less than or equal to Upper."),
        @JProlPredicate(signature = "forall/2", validate = {
            "+callable,+callable"}, reference = "forall(Cond, Action) for all alternative bindings of Cond, Action can be proven."),
        @JProlPredicate(signature = "append/3", validate = {
            "?term,?term,?term"}, reference = "append(Xs, Ys, Zs) is true if Zs is the concatenation of the lists Xs and Ys. More precisely, append(Xs, Ys, Zs) is true iff the list Xs is a list prefix of Zs and Ys is Zs with prefix Xs removed."),
        @JProlPredicate(signature = "member/2", validate = {
            "?term,?list"}, reference = "member(X, List) is true if and only if X is an element contained in List. If X is not instantiated, it will be instantiated with all the values in List.")
    })
public final class JProlCoreLibrary extends AbstractJProlLibrary {

  private static final SecureRandom RND = new SecureRandom();

  public JProlCoreLibrary() {
    super("jprol-core-lib");
  }

  @JProlPredicate(determined = true, signature = "=:=/2", validate = {
      "@evaluable,@evaluable"}, reference = "Arithmetic equal")
  public static boolean predicateARITHMETICEQU2(final JProlChoicePoint goal,
                                                final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0));
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1));
    return left.compare(right) == 0;
  }

  @JProlPredicate(determined = true, signature = "compare/3", validate = {
      "?atom,@term,@term"}, reference = "Determine or test the order between two terms in the standard order of terms. Order is one of <, > or =, with the obvious meaning.")
  public static boolean predicateORDER3(final JProlChoicePoint choicePoint,
                                        final TermStruct predicate) {
    final Term order = predicate.getArgumentAt(0).tryGround();
    final Term termA = predicate.getArgumentAt(1).tryGround();
    final Term termB = predicate.getArgumentAt(2).tryGround();

    final Term resultOrder;
    switch (choicePoint.compare(termA, termB)) {
      case -1:
        resultOrder = Terms.newAtom("<");
        break;
      case 1:
        resultOrder = Terms.newAtom(">");
        break;
      default:
        resultOrder = Terms.newAtom("=");
        break;
    }
    return order.unifyWith(resultOrder);
  }

  @JProlPredicate(determined = true, signature = "@</2", validate = {
      "?term,?term"}, reference = "Term less than")
  public static boolean predicateTERMLESS2(final JProlChoicePoint choicePoint,
                                           final TermStruct predicate) {
    return choicePoint.compare(predicate.getArgumentAt(0), predicate.getArgumentAt(1)) < 0;
  }

  @JProlPredicate(determined = true, signature = "@=/2", synonyms = "=@=/2", validate = {
      "?term,?term"}, reference = "Check term standard order equality.")
  public static boolean predicateTermEquals(final JProlChoicePoint choicePoint,
                                            final TermStruct predicate) {
    final Term term1 = predicate.getArgumentAt(0).tryGround();
    final Term term2 = predicate.getArgumentAt(1).tryGround();

    final Term cloned1 = term1.makeClone();
    final Term cloned2 = term2.makeClone();

    final AtomicInteger counter = new AtomicInteger(0);
    cloned1.variables().forEach(x -> {
      if (x.isUnground()) {
        x.setValue(Terms.newLong(counter.incrementAndGet()));
      }
    });

    counter.set(0);
    cloned2.variables().forEach(x -> {
      if (x.isUnground()) {
        x.setValue(Terms.newLong(counter.incrementAndGet()));
      }
    });

    return cloned1.unifyWith(cloned2);
  }

  @JProlPredicate(determined = true, signature = "@=</2", validate = {
      "?term,?term"}, reference = "Term less than or equal to.")
  public static boolean predicateTERMLESSEQU2(final JProlChoicePoint choicePoint,
                                              final TermStruct predicate) {
    return choicePoint.compare(predicate.getArgumentAt(0), predicate.getArgumentAt(1)) <= 0;
  }

  @JProlPredicate(determined = true, signature = "@>/2", validate = {
      "?term,?term"}, reference = "Term greater than")
  public static boolean predicateTERMGREATER2(final JProlChoicePoint choicePoint,
                                              final TermStruct predicate) {
    return choicePoint.compare(predicate.getArgumentAt(0), predicate.getArgumentAt(1)) > 0;
  }

  @JProlPredicate(determined = true, signature = "@>=/2", validate = {
      "?term,?term"}, reference = "Term greater than or equal to.")
  public static boolean predicateTERMGREATEREQU2(final JProlChoicePoint choicePoint,
                                                 final TermStruct predicate) {
    return choicePoint.compare(predicate.getArgumentAt(0), predicate.getArgumentAt(1)) >= 0;
  }

  @JProlPredicate(determined = true, signature = "==/2", validate = {
      "?term,?term"}, reference = "Check term strict identity.")
  public static boolean predicateEQU2(final JProlChoicePoint choicePoint,
                                      final TermStruct predicate) {
    final Term termA = predicate.getArgumentAt(0).tryGround();
    final Term termB = predicate.getArgumentAt(1).tryGround();

    int result = choicePoint.compare(termA, termB);

    if (termA instanceof NumericTerm && result == 0) {
      if (termA.getClass() != termB.getClass()) {
        result = 1;
      }
    }
    return result == 0;
  }

  @JProlPredicate(determined = true, signature = "\\==/2", validate = {
      "?term,?term"}, reference = "Term not identical")
  public static boolean predicateNOTEQU2(final JProlChoicePoint choicePoint,
                                         final TermStruct predicate) {
    return choicePoint.compare(predicate.getArgumentAt(0), predicate.getArgumentAt(1)) != 0;
  }

  @JProlPredicate(determined = true, signature = "atom_number/2", validate = {
      "?atom,?number"}, reference = "convert between atom and number")
  public static boolean predicateATOMNUMBER2(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term atom = predicate.getArgumentAt(0).tryGround();
    final Term number = predicate.getArgumentAt(1).tryGround();

    if (atom.getTermType() == VAR && number.getTermType() == VAR) {
      throw new ProlInstantiationErrorException("Arguments are not sufficiently instantiated",
          predicate);
    }
    if (atom.getTermType() == VAR) {
      if (number instanceof NumericTerm) {
        return atom.unifyWith(Terms.newAtom(number.forWrite()));
      } else {
        throw new ProlTypeErrorException("number", number);
      }
    } else if (number.getTermType() == VAR) {
      final String text = atom.getText();
      try {
        return number.unifyWith(Terms.newLong(text));
      } catch (NumberFormatException ex) {
        try {
          return number.unifyWith(Terms.newDouble(text));
        } catch (NumberFormatException exx) {
          return false;
        }
      }
    } else {
      final String text = atom.getText();
      NumericTerm numericTerm = null;
      try {
        numericTerm = Terms.newLong(text);
      } catch (NumberFormatException ex) {
        try {
          numericTerm = Terms.newDouble(text);
        } catch (NumberFormatException exx) {
          // do nothing
        }
      }
      if (numericTerm == null) {
        return false;
      }
      return number.unifyWith(numericTerm);
    }
  }

  @JProlPredicate(determined = true, signature = ">/2", validate = {
      "+evaluable,+evaluable"}, reference = "Arithmetic greater than")
  public static boolean predicateARITHGREATER2(final JProlChoicePoint goal,
                                               final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());
    return left.compare(right) > 0;
  }

  @JProlPredicate(determined = true, signature = "</2", validate = {
      "+evaluable,+evaluable"}, reference = "Arithmetic less than")
  public static boolean predicateARITHLESS2(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());
    return left.compare(right) < 0;
  }

  @JProlPredicate(determined = true, signature = ">=/2", validate = {
      "+evaluable,+evaluable"}, reference = "Arithmetic greater than or equal to")
  public static boolean predicateARITHGREATEREQU2(final JProlChoicePoint goal,
                                                  final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());
    return left.compare(right) >= 0;
  }

  @JProlPredicate(determined = true, signature = "=</2", validate = {
      "+evaluable,+evaluable"}, reference = "Arithmetic less than or equal to")
  public static boolean predicateARITHLESSEQU2(final JProlChoicePoint goal,
                                               final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());
    return left.compare(right) <= 0;
  }

  @JProlPredicate(determined = true, signature = "=\\=/2", validate = {
      "+evaluable,+evaluable"}, reference = "Arithmetic Not equal")
  public static boolean predicateARITHNOTEQU2(final JProlChoicePoint goal,
                                              final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());
    return left.compare(right) != 0;
  }

  @JProlPredicate(evaluable = true, signature = "xor/2", validate = {
      "+evaluable,+evaluable"}, reference = "Bitwise exclusive or.")
  public static Term predicateXOR2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

    if (goal.isVerify()) {
      assertInteger(left);
      assertInteger(right);
    }

    return newLong(left.toNumber().longValue() ^ right.toNumber().longValue(), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "\\/1", validate = {
      "+evaluable"}, reference = "Bitwise 'not'")
  public static Term predicateBITWISENOT1(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0));
    if (goal.isVerify()) {
      assertInteger(arg);
    }
    return newLong(~arg.toNumber().longValue(), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "\\//2", validate = {
      "+evaluable,+evaluable"}, reference = "Bitwise 'or'")
  public static Term predicateBITWISEOR2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0));
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1));

    if (goal.isVerify()) {
      assertInteger(left);
      assertInteger(right);
    }

    return newLong(left.toNumber().longValue() | right.toNumber().longValue(), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "/\\/2", validate = {
      "+evaluable,+evaluable"}, reference = "Bitwise 'and'")
  public static Term predicateBITWISEAND2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

    if (goal.isVerify()) {
      assertInteger(left);
      assertInteger(right);
    }

    return newLong(left.toNumber().longValue() & right.toNumber().longValue(), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "mod/2", validate = {
      "+evaluable,+evaluable"}, reference = "Modulus")
  public static Term predicateMOD2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

    if (goal.isVerify()) {
      assertInteger(left);
      assertInteger(right);
    }

    final long rightNum = right.toNumber().longValue();
    if (rightNum == 0L) {
      throw new ProlEvaluationErrorException("zero divisor", predicate);
    }
    return newLong(left.toNumber().longValue() % rightNum, UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "rem/2", validate = {
      "+evaluable,+evaluable"}, reference = "Remainder")
  public static Term predicateREM2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

    if (goal.isVerify()) {
      assertInteger(left);
      assertInteger(right);
    }

    final long leftNum = left.toNumber().longValue();
    final long rightNum = right.toNumber().longValue();
    if (rightNum == 0L) {
      throw new ProlEvaluationErrorException("zero divisor", predicate);
    }
    return newLong(leftNum - (leftNum / rightNum) * rightNum, UNKNOWN);
  }

  private static Term tryAsLong(final double result) {
    if (Double.isFinite(result)) {
      final long round = Math.round(result);
      return round == result ? newLong(round) : newDouble(result);
    } else {
      return newDouble(result, UNKNOWN);
    }
  }

  @JProlPredicate(evaluable = true, synonyms = "^/2", signature = "**/2", validate = {
      "+evaluable,+evaluable"}, reference = "Power")
  public static Term predicatePOWER2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

    final double valueLeft = left.toNumber().doubleValue();
    final double valueRight = right.toNumber().doubleValue();

    return tryAsLong(Math.pow(valueLeft, valueRight));
  }

  @JProlPredicate(evaluable = true, signature = "+/2", validate = {
      "+evaluable,+evaluable"}, reference = "Addition")
  public static Term predicateADDTWO2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());
    return left.add(right);
  }

  @JProlPredicate(evaluable = true, signature = "sin/1", validate = {
      "+evaluable"}, reference = "Sine")
  public static Term predicateSIN1(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    return tryAsLong(Math.sin(arg.toNumber().doubleValue()));
  }

  @JProlPredicate(evaluable = true, signature = "float_integer_part/1", validate = {
      "+evaluable"}, reference = "Integer part")
  public static Term predicateFLOATINTEGERPART1(final JProlChoicePoint goal,
                                                final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    return newLong(arg.toNumber().longValue(), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "float_fractional_part/1", validate = {
      "+evaluable"}, reference = "Fractional part")
  public static Term predicateFLOATFRACTIONALPART1(final JProlChoicePoint goal,
                                                   final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    final long valueInt = (long) value;
    return tryAsLong(value - (double) valueInt);
  }

  @JProlPredicate(evaluable = true, signature = "floor/1", validate = {
      "+evaluable"}, reference = "Floor")
  public static Term predicateFLOOR1(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return newLong((long) Math.floor(value), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "truncate/1", validate = {
      "+evaluable"}, reference = "Truncate")
  public static Term predicateTRUNCATE1(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return newLong(value < 0 ? (long) Math.ceil(value) : (long) Math.floor(value), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "round/1", validate = {
      "+evaluable"}, reference = "Round")
  public static Term predicateROUND1(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return newLong(Math.round(value), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "ceiling/1", validate = {
      "+evaluable"}, reference = "Ceiling")
  public static Term predicateCEILING1(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return newLong((long) Math.ceil(value), UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = "cos/1", validate = {
      "+evaluable"}, reference = "Cosine")
  public static Term predicateCOS1(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return tryAsLong(Math.cos(value));
  }

  @JProlPredicate(evaluable = true, signature = "atan/1", validate = {
      "+evaluable"}, reference = "Arc tangent")
  public static Term predicateATAN(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return tryAsLong(Math.atan(value));
  }

  @JProlPredicate(evaluable = true, signature = "exp/1", validate = {
      "+evaluable"}, reference = "Exponentiation")
  public static Term predicateEXP(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return tryAsLong(Math.exp(value));
  }

  @JProlPredicate(evaluable = true, signature = "log/1", validate = {
      "+evaluable"}, reference = "Log")
  public static Term predicateLOG(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return tryAsLong(Math.log(value));
  }

  @JProlPredicate(evaluable = true, signature = "sqrt/1", validate = {
      "+evaluable"}, reference = "Square root")
  public static Term predicateSQRT(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final double value = arg.toNumber().doubleValue();
    return tryAsLong(Math.sqrt(value));
  }

  @JProlPredicate(evaluable = true, signature = "abs/1", validate = {
      "+evaluable"}, reference = "Absolute value")
  public static Term predicateABS(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    return arg.abs();
  }

  @JProlPredicate(evaluable = true, signature = "sign/1", validate = {
      "+evaluable"}, reference = "SIGN")
  public static Term predicateSIGN(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm arg = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    return arg.sign();
  }

  @JProlPredicate(signature = "sub_atom/5", validate = {
      "+atom,?integer,?integer,?integer,?term"}, reference = "Breaking atoms")
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

        final boolean result = before.unifyWith(Terms.newLong(this.currentBefore, UNKNOWN))
            && length.unifyWith(Terms.newLong(this.currentLength, UNKNOWN))
            && after.unifyWith(Terms.newLong(this.currentAfter, UNKNOWN))
            && sub.unifyWith(Terms.newAtom(currentSub));

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

    SubAtomIterator iterator = goal.getInternalObject();
    if (iterator == null) {
      final Term arg1 = predicate.getArgumentAt(0).tryGround();
      final Term arg2 = predicate.getArgumentAt(1).tryGround();
      final Term arg3 = predicate.getArgumentAt(2).tryGround();
      final Term arg4 = predicate.getArgumentAt(3).tryGround();
      final Term arg5 = predicate.getArgumentAt(4).tryGround();

      iterator = new SubAtomIterator(arg1, arg2, arg3, arg4, arg5);
      goal.setInternalObject(iterator);
    }

    return iterator.next(
        predicate.getArgumentAt(1),
        predicate.getArgumentAt(2),
        predicate.getArgumentAt(3),
        predicate.getArgumentAt(4)
    );
  }

  @JProlPredicate(evaluable = true, signature = "-/2", validate = {
      "+evaluable,+evaluable"}, reference = "Subtraction")
  public static Term predicateSUBTWO(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());
    return left.sub(right);
  }

  @JProlPredicate(evaluable = true, signature = "-/1", validate = {
      "+evaluable"}, reference = "Negation")
  public static Term predicateNeg(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm val = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    return val.neg();
  }

  @JProlPredicate(evaluable = true, signature = "+/1", validate = {
      "+evaluable"}, reference = "Not action over a number")
  public static Term predicateTheSame(final JProlChoicePoint goal, final TermStruct predicate) {
    return calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
  }

  @JProlPredicate(evaluable = true, signature = "*/2", validate = {
      "+evaluable,+evaluable"}, reference = "Multiplication")
  public static Term predicateMUL(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());
    return left.mul(right);
  }

  @JProlPredicate(evaluable = true, signature = "//2", validate = {
      "+evaluable,+evaluable"}, reference = "Division")
  public static Term predicateDIV(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

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

  @JProlPredicate(evaluable = true, signature = "///2", synonyms = "div/2", validate = {
      "+evaluable,+evaluable"}, reference = "Integer division.")
  public static Term predicateIDIV2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

    if (goal.isVerify()) {
      assertInteger(left);
      assertInteger(right);
    }

    try {
      return left.div(right);
    } catch (ArithmeticException ex) {
      throw new ProlEvaluationErrorException(ex.getMessage(), predicate, ex);
    }
  }

  @JProlPredicate(evaluable = true, signature = "<</2", validate = {
      "+evaluable,+evaluable"}, reference = "Bitwise left shift")
  public static Term predicateSHIFTL2(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

    final long value = left.toNumber().longValue();
    final long shift = right.toNumber().longValue();

    return newLong(value << shift, UNKNOWN);
  }

  @JProlPredicate(evaluable = true, signature = ">>/2", validate = {
      "+evaluable,+evaluable"}, reference = "Bitwise right shift")
  public static Term predicateSHIFTR(final JProlChoicePoint goal, final TermStruct predicate) {
    final NumericTerm left = calcEvaluable(goal, predicate.getArgumentAt(0).tryGround());
    final NumericTerm right = calcEvaluable(goal, predicate.getArgumentAt(1).tryGround());

    final long value = left.toNumber().longValue();
    final long shift = right.toNumber().longValue();

    return newLong(value >> shift, UNKNOWN);
  }

  @JProlPredicate(signature = "repeat/0", reference = "repeat is true. It just places a choice point every call.")
  public static void predicateREPEAT0(final JProlChoicePoint goal, final TermStruct predicate) {
    // we just make choose point
  }

  @JProlPredicate(signature = "call/1", validate = {
      "+callable"}, reference = "call(G) is true if and only if G represents a goal which is true.")
  public static boolean predicateCALL(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argument = predicate.getArgumentAt(0).tryGround();

    JProlChoicePoint newChoicePoint = goal.getInternalObject();
    if (newChoicePoint == null) {
      newChoicePoint = goal.getContext().makeChoicePoint(argument);
      goal.setInternalObject(newChoicePoint);
    }
    final Term nextResult = newChoicePoint.prove();
    boolean result = false;

    if (nextResult != null) {
      result = assertUnify(argument, nextResult);
      if (newChoicePoint.isCompleted()) {
        goal.resetLogicalAlternativesFlag();
      }
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "once/1", validate = {
      "+callable"}, reference = "once(Term) is true. once/1 is not re-executable.")
  public static boolean predicateONCE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argument = predicate.getArgumentAt(0).tryGround();
    final JProlChoicePoint currentGoal =
        goal.getContext().makeChoicePoint(argument, goal.getAssociatedObject());
    final Term nextResult = currentGoal.prove();

    return nextResult != null;
  }

  @JProlPredicate(changesChooseChain = true, signature = "->/2", validate = ":callable,:term", reference = "'->'(If, Then) is true if and only if If is true and Then is true for the first solution of If")
  public static boolean predicateIFTHEN(final JProlChoicePoint goal, final TermStruct predicate) {
    // if-then
    final JProlChoicePoint leftSubbranch =
        goal.getContext().makeChoicePoint(predicate.getArgumentAt(0), goal.getAssociatedObject());
    boolean result = false;
    if (leftSubbranch.prove() != null) {
      // replace current goal by the 'then' goal
      final JProlChoicePoint thenPart = goal.replaceLastGoalAtChain(predicate.getArgumentAt(1));
      thenPart.resetPreviousChoicePoint(); // remove all previous choice points
      result = true;
    } else {
      goal.resetLogicalAlternativesFlag();
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "var/1", validate = "@term", reference = "var(X) is true if and only if X is a variable.")
  public static boolean predicateVAR(final JProlChoicePoint goal, final TermStruct predicate) {
    return predicate.getArgumentAt(0).tryGround().getTermType() == VAR;
  }

  @JProlPredicate(determined = true, signature = "nonvar/1", validate = "@term", reference = "nonvar(X) is true if and only if X is not a variable.")
  public static boolean predicateNONVAR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0);
    if (arg.getTermType() == VAR) {
      return !arg.isUnground();
    } else {
      return true;
    }
  }

  @JProlPredicate(determined = true, signature = "atom/1", validate = "@term", reference = "atom(X) is true if and only if X is an atom.")
  public static boolean predicateATOM(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    return arg.getTermType() == ATOM && !(arg instanceof NumericTerm);
  }

  @JProlPredicate(determined = true, signature = "ground/1", validate = "@term", reference = "True if Term holds no free variables.")
  public static boolean predicateGROUND(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    return arg.isGround();
  }

  @JProlPredicate(determined = true, signature = "callable/1", validate = "@term", reference = "True if Term is bound to an atom or a compound term.")
  public static boolean predicateCALLABLE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    return (arg.getTermType() == ATOM && !(arg instanceof NumericTerm)) ||
        arg.getTermType() == STRUCT;
  }

  @JProlPredicate(determined = true, signature = "string/1", validate = "@term", reference = "True if Term is bound to a string.")
  public static boolean predicateSTRING(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    if (arg.getTermType() == LIST) {
      final TermList termList = (TermList) arg;
      if (termList.isNullList()) {
        return false;
      }
      return termList.doesContainOnlyCharCodes();
    } else {
      return false;
    }
  }

  @JProlPredicate(determined = true, signature = "integer/1", validate = "@term", reference = "integer(X) is true if and only if X is an integer.")
  public static boolean predicateINTEGER(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    return arg instanceof TermLong;
  }

  @JProlPredicate(determined = true, signature = "number/1", validate = "@term", reference = "number(X) is true if and only if X is an integer or a float.")
  public static boolean predicateNUMBER(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    return arg instanceof NumericTerm;
  }

  @JProlPredicate(determined = true, signature = "float/1", validate = "@term", reference = "float(X) is true if and only if X is a float.")
  public static boolean predicateFLOAT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    return arg instanceof TermDouble;
  }

  @JProlPredicate(determined = true, signature = "compound/1", validate = "@term", reference = "compound(X) is true if and only if X is a compound term, that is neither atomic nor a variable.")
  public static boolean predicateCOMPOUND(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term atom = predicate.getArgumentAt(0).tryGround();
    TermType termType = atom.getTermType();
    if (Objects.requireNonNull(termType) == STRUCT) {
      return true;
    } else if (termType == LIST) {
      return !atom.isNullList();
    }
    return false;
  }

  @JProlPredicate(determined = true, signature = "atomic/1", validate = "@term", reference = "atomic(X) is true if and only if X is atomic (that is an atom, an integer or a float).")
  public static boolean predicateATOMIC(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    return arg.getTermType() == ATOM || arg.isNullList();
  }

  @JProlPredicate(determined = true, signature = "arg/3", validate = {
      "+integer,+compound_term,?term"}, reference = "arg(N,Term, Arg) is true if nad only if the Nth argument of Term is Arg")
  public static boolean predicateARG(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term number = predicate.getArgumentAt(0).tryGround();
    final Term compound_term = predicate.getArgumentAt(1).tryGround();
    final Term element = predicate.getArgumentAt(2).tryGround();

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
        result = element.unifyWith(struct.getArgumentAt((int) elementIndex));
      }
    }
    return result;
  }

  @JProlPredicate(determined = true, signature = "functor/3", validate = {
      "?term,?name,?arity"},
      reference = "functor(Term, Name, Arity) is true if and only if Term is a compound term with functor name Name and arity Arity or Term is an atomic term equal to Name and Arity is 0.")
  public static boolean predicateFUNCTOR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argTerm = predicate.getArgumentAt(0).tryGround();
    final Term argName = predicate.getArgumentAt(1).tryGround();
    final Term argArity = predicate.getArgumentAt(2).tryGround();

    if (argTerm.getTermType() == VAR && argName.getTermType() == VAR) {
      throw new ProlInstantiationErrorException("Either term or name must be instantiated",
          predicate);
    }

    switch (argTerm.getTermType()) {
      case ATOM: {
        final Term arity = newLong(0);
        return argName.unifyWith(argTerm) && argArity.unifyWith(arity);
      }
      case STRUCT: {
        final TermStruct struct = (TermStruct) argTerm;
        final Term functor = newAtom(struct.getFunctor().getText());
        final Term arity = newLong(struct.getArity());
        return argName.unifyWith(functor) && argArity.unifyWith(arity);
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
        return argName.unifyWith(name) && argArity.unifyWith(arity);
      }
      case VAR: {
        final int arity = (int) argArity.toNumber().longValue();
        if (arity < 0) {
          throw new ProlRepresentationErrorException("Wrong arity value", predicate);
        }

        if (argName instanceof NumericTerm) {
          if (arity == 0) {
            return argTerm.unifyWith(argName);
          } else {
            throw new ProlTypeErrorException("atom", predicate);
          }
        }

        Term[] elements = null;

        if (arity > 0) {
          elements = new Term[arity];
          for (int li = 0; li < arity; li++) {
            elements[li] = Terms.newAnonymousVar();
          }
        }

        final TermStruct newStruct = newStruct(argName, elements);

        return argTerm.unifyWith(newStruct);
      }
      default:
        throw new ProlCriticalError("Unexpected type:" + argTerm.getTermType());
    }

  }

  @JProlPredicate(determined = true, signature = "ll2r/2", validate = {"+list,+list"},
      reference = "Iterate right list elements and unification of them with first allowed element in left list, false if any right list element to unified. Allows extract data in case like ll2r([index=1,body='hello'],[index=Index,body=Body]).")
  public static boolean predicateLL2R(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    final TermList listLeft = ProlAssertions.assertList(argLeft);
    final TermList listRight = ProlAssertions.assertList(argRight);

    Set<Integer> freeIndex = new LazySet<>();
    int index = 0;
    for (final Term r : listRight) {
      freeIndex.add(index++);
    }

    for (final Term l : listLeft) {
      index = 0;
      for (final Term r : listRight) {
        if (freeIndex.contains(index)) {
          if (l.unifyWith(r)) {
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

  @JProlPredicate(determined = true, signature = "string_bytes/3", validate = {"?atom,?list,+atom"},
      reference = "True when the string is represented by bytes in encoding. If the string is instantiated it may represent text as an atom. Bytes is always a list of integers in the range 0 ... 255.")
  public static boolean predicateSTRING_BYTES(final JProlChoicePoint goal,
                                              final TermStruct predicate) {
    final Term argString = predicate.getArgumentAt(0).tryGround();
    final Term argListBytes = predicate.getArgumentAt(1).tryGround();
    final Term encoding = predicate.getArgumentAt(2).tryGround();

    final String encodingText = encoding.getText().trim().toUpperCase(Locale.ROOT);
    final Charset charset;
    if (Charset.isSupported(encodingText)) {
      charset = Charset.forName(encodingText);
    } else {
      throw new ProlDomainErrorException(Charset.availableCharsets().keySet().stream()
          .sorted()
          .map(x -> '\'' + x + '\'')
          .collect(Collectors.joining(",", "[", "]"))
          , encoding);
    }

    if (argString.getTermType() == VAR) {
      if (argListBytes.getTermType() == LIST) {
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        for (final Term t : (TermList) argListBytes) {
          buffer.write(t.tryGround().toNumber().byteValue());
        }
        return argString.unifyWith(Terms.newAtom(new String(buffer.toByteArray(),
            charset))); // keep such construction for android compatibility
      } else {
        return false;
      }
    } else {
      final List<Term> codes = new ArrayList<>();
      for (final byte b : argString.getText().getBytes(charset)) {
        codes.add(Terms.newLong(b & 0xFF));
      }
      final TermList list = TermList.listOf(codes);
      return argListBytes.unifyWith(list);
    }
  }

  @JProlPredicate(determined = true, signature = "=../2", validate = {"+term,?list",
      "?term,?list"}, reference = "Term =.. List is true if and only if\n* Term is an atomic term and List is the list whose only element is Term, or\n* Term is a compound term and List is the list whose head is the functor name of Term and whose tail is the list of the arguments of Term. ")
  public static boolean predicateUNIV(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    if (argLeft.getTermType() == STRUCT) {
      if (((TermStruct) argLeft).getArity() == 0) {
        throw new ProlDomainErrorException("compound_non_zero_arity", predicate);
      }
    }

    if (argRight.getTermType() == VAR) {
      TermList list = TermList.makeListFromElementWithSplitStructure(argLeft);
      return argRight.unifyWith(list);
    } else {
      final Term atom = ((TermList) argRight).toAtom();
      if (atom.getTermType() == STRUCT) {
        final TermStruct atomAsStruct = (TermStruct) atom;
        atomAsStruct.setPredicateProcessor(goal.getContext().findProcessor(atomAsStruct));
      }
      return argLeft.unifyWith(atom);
    }
  }

  @JProlPredicate(determined = true, signature = "append/2", validate = {"+list,?list"},
      reference = "Concatenate a list of lists. Is true if the left argument is a list of lists, and the right one is the concatenation of these lists.")
  public static boolean predicateAPPEND(final JProlChoicePoint goal,
                                        final TermStruct predicate) {
    Term left = predicate.getArgumentAt(0).tryGround();
    Term right = predicate.getArgumentAt(1).tryGround();

    if (left.getTermType() != LIST) {
      return false;
    }

    final List<Term> concatenated = ((TermList) left).streamChildren()
        .flatMap(x -> {
          final Term xx = x.tryGround();
          return xx.getTermType() == LIST ? ((TermList) xx).streamChildren() : Stream.of(xx);
        })
        .collect(Collectors.toList());

    return right.unifyWith(TermList.listOf(concatenated));
  }

  @JProlPredicate(determined = true, signature = "atom_chars/2", validate = {"+atom,?char_list",
      "?atom,+char_list"}, reference = "atom_chars(Atom, List) succeeds if and only if List is a list whose elements are the one character atoms that in order make up  Atom.")
  public static boolean predicateATOMCHARS(final JProlChoicePoint goal,
                                           final TermStruct predicate) {
    Term left = predicate.getArgumentAt(0).tryGround();
    Term right = predicate.getArgumentAt(1).tryGround();

    switch (left.getTermType()) {
      case ATOM: {
        return ProlUtils.toCharList(left, left.getSourcePosition()).unifyWith(right);
      }
      case LIST: {
        if (left.isNullList()) {
          return ProlUtils.toCharList(EMPTY_LIST_ATOM, left.getSourcePosition()).unifyWith(right);
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
          throw new ProlInstantiationErrorException(predicate);
        }
      }

      right = newAtom(builder.toString());
      return left.unifyWith(right);
    }

    return false;
  }

  @JProlPredicate(determined = true, signature = "char_code/2", validate = {
      "+char,?integer",
      "-char,+integer"}, reference = "char_code(Char, Code) succeeds if and only if Code is the character code that corresponds to the character Char.")
  public static boolean predicateCHARCODE(final JProlChoicePoint goal, final TermStruct predicate) {
    Term left = predicate.getArgumentAt(0).tryGround();
    Term right = predicate.getArgumentAt(1).tryGround();

    if (left.getTermType() == ATOM) {
      left = newLong((int) left.getText().charAt(0));
      return right.unifyWith(left);
    }

    if (right.getTermType() == ATOM) {
      assertCharacterCode(right);
      right = newAtom(Character.toString((char) right.toNumber().shortValue()));
      return left.unifyWith(right);
    }

    return false;
  }

  @JProlPredicate(determined = true, signature = "number_codes/2", validate = {
      "?number,?code_list"}, reference = "number_codes(Number, CodeList) succeeds if and only if CodeList is a list whose elements are the codes for the one character atoms that in order make up Number.")
  public static boolean predicateNUMBERCODES(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    Term left = predicate.getArgumentAt(0).tryGround();
    final Term right = predicate.getArgumentAt(1).tryGround();

    if (goal.isVerify()) {
      if (left.getTermType() == VAR) {
        ProlAssertions.assertCharCodeList(right);
      } else {
        ProlAssertions.assertNumber(left);
        if (right.getTermType() != VAR) {
          ProlAssertions.assertCharCodeList(right);
        }
      }
    }

    if (left.getTermType() == ATOM && right.getTermType() == VAR) {
      return ProlUtils.toCharCodeList(left).unifyWith(right);
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

      return left.unifyWith(number);
    }

    return false;
  }

  @JProlPredicate(signature = "current_predicate_all/1",
      validate = {"?predicate_indicator"},
      reference = "True if PredicateIndicator is a currently defined predicate. It looks for predicates both in knowledge base and attached libraries."
  )
  public static boolean predicateCURRENTPREDICATEALL(final JProlChoicePoint choicePoint,
                                                     final TermStruct predicate) {
    final Term predicateIndicator = predicate.getArgumentAt(0).tryGround();
    List<TermStruct> list = choicePoint.getInternalObject();
    if (list == null) {
      list = new ArrayList<>(
          choicePoint.getContext().findAllForPredicateIndicatorInLibs(predicateIndicator));

      try (final CloseableIterator<TermStruct> knowledgeBaseIterator = choicePoint.getContext()
          .getKnowledgeBase().iterateSignatures(
              predicateIndicator.getTermType() == VAR ?
                  Terms.newStruct("/",
                      new Term[] {Terms.newAnonymousVar(), Terms.newAnonymousVar()}, UNKNOWN) :
                  (TermStruct) predicateIndicator)) {
        while (knowledgeBaseIterator.hasNext()) {
          list.add(knowledgeBaseIterator.next());
        }
        list.sort(choicePoint);
        choicePoint.setInternalObject(list);
      } catch (IOException ex) {
        throw new ProlKnowledgeBaseException(ex);
      }
    }

    if (list.isEmpty()) {
      return false;
    } else {
      return predicateIndicator.unifyWith(list.remove(0));
    }
  }

  @JProlPredicate(signature = "current_predicate/1",
      validate = {"?predicate_indicator"},
      reference = "True if PredicateIndicator is a currently defined predicate. It looks for predicates only in current knowledge base."
  )
  public static boolean predicateCURRENTPREDICATE(final JProlChoicePoint choicePoint,
                                                  final TermStruct predicate) {
    final Term predicateIndicator = predicate.getArgumentAt(0).tryGround();
    List<TermStruct> list = choicePoint.getInternalObject();
    if (list == null) {
      list = new ArrayList<>();
      try (final CloseableIterator<TermStruct> knowledgeBaseIterator = choicePoint.getContext()
          .getKnowledgeBase().iterateSignatures(
              predicateIndicator.getTermType() == VAR ?
                  Terms.newStruct("/",
                      new Term[] {Terms.newAnonymousVar(), Terms.newAnonymousVar()}, UNKNOWN) :
                  (TermStruct) predicateIndicator)) {
        while (knowledgeBaseIterator.hasNext()) {
          list.add(knowledgeBaseIterator.next());
        }
        list.sort(choicePoint);
        choicePoint.setInternalObject(list);
      } catch (IOException ex) {
        throw new ProlKnowledgeBaseException(ex);
      }
    }

    if (list.isEmpty()) {
      return false;
    } else {
      return predicateIndicator.unifyWith(list.remove(0));
    }
  }

  @JProlPredicate(signature = "atom_concat/3",
      validate = {"?atom,?atom,?atom"},
      reference = "Atom3 forms the concatenation of Atom1 and Atom2.")
  public static boolean predicateATOMCONCAT3(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term atom1 = predicate.getArgumentAt(0).tryGround();
    final Term atom2 = predicate.getArgumentAt(1).tryGround();
    final Term atom3 = predicate.getArgumentAt(2).tryGround();

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
        return newAtom(this.seq1.toString(), UNKNOWN);
      }

      Term getSeq2AsTerm() {
        return newAtom(this.seq2.toString(), UNKNOWN);
      }
    }

    if (bounded < 2) {
      throw new ProlInstantiationErrorException(predicate);
    } else {
      final AtomConcatState state = goal.getInternalObject();
      if (state == null) {
        if (atom1.isGround() && atom2.isGround()) {
          goal.resetLogicalAlternativesFlag();
          return atom3.unifyWith(newAtom(atom1.getText() + atom2.getText(), UNKNOWN));
        } else if (atom1.isGround()) {
          goal.resetLogicalAlternativesFlag();
          final String text1 = atom1.getText();
          final String text3 = atom3.getText();
          if (text3.startsWith(text1)) {
            return atom2.unifyWith(newAtom(text3.substring(text1.length()), UNKNOWN));
          }
        } else if (atom2.isGround()) {
          goal.resetLogicalAlternativesFlag();
          final String text2 = atom2.getText();
          final String text3 = atom3.getText();
          if (text3.endsWith(text2)) {
            return atom1.unifyWith(
                newAtom(text3.substring(0, text3.length() - text2.length()), UNKNOWN));
          }
        } else {
          final String wholeText = atom3.getText();
          final AtomConcatState newState = new AtomConcatState(wholeText);
          goal.setInternalObject(newState);
          return atom1.unifyWith(newState.getSeq1AsTerm()) &&
              atom2.unifyWith(newState.getSeq2AsTerm());
        }
      } else {
        boolean result = state.next();
        if (result) {
          result = atom1.unifyWith(state.getSeq1AsTerm()) && atom2.unifyWith(state.getSeq2AsTerm());
        } else {
          goal.resetLogicalAlternativesFlag();
        }
        return result;
      }
    }
    return false;
  }


  @JProlPredicate(
      determined = true,
      signature = "number_chars/2",
      validate = {"?number,?char_list"},
      reference = "number_chars(Number, List) succeeds if and only if List is a list whose elements are the one character atoms that in order make up Number.")
  public static boolean predicateNUMBERCHARS2(final JProlChoicePoint goal,
                                              final TermStruct predicate) {

    Term left = predicate.getArgumentAt(0).tryGround();
    final Term right = predicate.getArgumentAt(1).tryGround();

    if (goal.isVerify()) {
      if (left.getTermType() == VAR) {
        ProlAssertions.assertCharList(right);
      } else {
        ProlAssertions.assertNumber(left);
        if (right.getTermType() != VAR) {
          ProlAssertions.assertCharList(right);
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
      if (goal.isVerify()) {
        for (int i = 0; i < text.length(); i++) {
          final char chr = text.charAt(i);
          if (Character.isISOControl(chr) || Character.isWhitespace(chr)) {
            throw new ProlCustomErrorException(Terms.newAtom("syntax_error", UNKNOWN), right);
          }
        }
      }

      final Term term;
      try (final JProlTreeBuilder treeBuilder = new JProlTreeBuilder(goal.getContext(),
          new StringReader(builder.append('.').toString()), true)) {
        term = treeBuilder.readPhraseAndMakeTree();
      } catch (PrologParserException ex) {
        throw new ProlCustomErrorException(Terms.newAtom("syntax_error", UNKNOWN), right);
      }

      if (term instanceof NumericTerm) {
        result = left.unifyWith(term);
      } else {
        throw new ProlTypeErrorException("number", "Expected numeric term: " + term, term);
      }
    } else if (left.getTermType() == ATOM) {
      result = ProlUtils.toCharList(left, left.getSourcePosition()).unifyWith(right);
    } else {
      result = false;
    }

    return result;
  }

  @JProlPredicate(signature = "for/3", validate = {
      "?term,+integer,+integer"}, reference = "Allows to make integer counter from a variable, (TermVar, Low, High).")
  public static boolean predicateFOR3(final JProlChoicePoint choicePoint,
                                      final TermStruct predicate) {
    final Term term = predicate.getArgumentAt(0).tryGround();

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

    For3CounterStorage counter = choicePoint.getInternalObject();
    if (counter == null) {
      final Term lowTerm = predicate.getArgumentAt(1).tryGround();
      final Term highTerm = predicate.getArgumentAt(2).tryGround();

      final long low = lowTerm.toNumber().longValue();
      final long high = highTerm.toNumber().longValue();

      counter = new For3CounterStorage(low, high);
      choicePoint.setInternalObject(counter);
    } else {
      counter.inc();
    }
    final long value = counter.value;
    if (counter.isEnd()) {
      choicePoint.resetLogicalAlternativesFlag();
      return false;
    } else {
      return term.unifyWith(Terms.newLong(value, UNKNOWN));
    }
  }

  @JProlPredicate(determined = true, signature = "rnd/2", validate = {"+integer,?integer",
      "+list,?term"}, reference = "Generate pseudo random in 0(included)...limit(excluded) or select random element from the list.")
  public static boolean predicateRND(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term first = predicate.getArgumentAt(0).tryGround();
    final Term second = predicate.getArgumentAt(1).tryGround();

    final Term result;
    if (first.getTermType() == LIST) {
      final TermList list = (TermList) first;
      if (list.isNullList()) {
        result = NULL_LIST;
      } else {
        final Term[] array = list.toArray(false);
        result = array[RND.nextInt(array.length)];
      }
    } else {
      result = Terms.newLong(nextLong(first.toNumber().longValue()),
          UNKNOWN);
    }
    return second.unifyWith(result);
  }

  @JProlPredicate(determined = true, signature = "atom_length/2", validate = {
      "+term,?integer"}, reference = "atom_length(Atom, Length) is true if and only if the integer Length equals the number of characters in the name of the atom Atom.")
  public static boolean predicateATOMLENGTH(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    Term left = predicate.getArgumentAt(0).tryGround();
    final Term right = predicate.getArgumentAt(1).tryGround();

    left = newLong(left.getTextLength(), UNKNOWN);
    return left.unifyWith(right);
  }

  @JProlPredicate(determined = true, signature = "atom_codes/2", validate = {
      "+atom,?code_list",
      "?atom,+code_list"}, reference = "atom_codes(Atom, List) succeeds if and only if List is a list whose elements are the character codes that in order correspond to the characters that make up  Atom.")
  public static boolean predicateATOMCHARCODES(final JProlChoicePoint goal,
                                               final TermStruct predicate) {
    Term left = predicate.getArgumentAt(0).tryGround();
    Term right = predicate.getArgumentAt(1).tryGround();

    switch (left.getTermType()) {
      case ATOM: {
        return ProlUtils.toCharCodeList(left).unifyWith(right);
      }
      case LIST: {
        if (left.isNullList()) {
          return ProlUtils.toCharCodeList(newAtom("[]", UNKNOWN)).unifyWith(right);
        } else {
          throw new ProlTypeErrorException("atom", predicate);
        }
      }
    }

    if (left.getTermType() == ATOM) {
      return ProlUtils.toCharCodeList(left).unifyWith(right);
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

      right = newAtom(builder.toString(), UNKNOWN);
      return left.unifyWith(right);
    }

    return false;
  }

  @JProlPredicate(determined = true, signature = "sort/2", validate = {
      "+list,?list"}, reference = "True if Sorted can be unified with a list holding the elements of List, sorted to the standard order of terms")
  public static boolean predicateSORT2(final JProlChoicePoint choicePoint,
                                       final TermStruct predicate) {
    final Term sourceList = predicate.getArgumentAt(0).tryGround();
    final Term targetList = predicate.getArgumentAt(1).tryGround();

    final TermList sourceListAsList;
    if (sourceList.getTermType() == LIST) {
      sourceListAsList = (TermList) sourceList;
    } else {
      throw new ProlInstantiationErrorException("list", choicePoint.getGoalTerm());
    }
    return targetList.unifyWith(sourceListAsList.sort(choicePoint, true));
  }

  @JProlPredicate(determined = true, signature = "msort/2", validate = {
      "+list,?list"}, reference = "Equivalent to sort/2, but does not remove duplicates. Raises a type_error if List is a cyclic list or not a list.")
  public static boolean predicateMSORT2(final JProlChoicePoint choicePoint,
                                        final TermStruct predicate) {
    final Term sourceList = predicate.getArgumentAt(0).tryGround();
    final Term targetList = predicate.getArgumentAt(1).tryGround();

    final TermList sourceListAsList;
    if (sourceList.getTermType() == LIST) {
      sourceListAsList = (TermList) sourceList;
    } else {
      throw new ProlInstantiationErrorException("list", choicePoint.getGoalTerm());
    }
    return targetList.unifyWith(sourceListAsList.sort(choicePoint, false));
  }

  @JProlPredicate(determined = true, signature = "findall/3", validate = {
      "?term,+callable,?list"}, reference = "Creates a list of the instantiations Template gets successively on backtracking over Goal and unifies the result with Bag.")
  public static boolean predicateFINDALL3(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term template = predicate.getArgumentAt(0).tryGround();
    final Term scopeGoal = predicate.getArgumentAt(1).tryGround();
    final Term instances = predicate.getArgumentAt(2).tryGround();

    final JProlChoicePoint find_goal =
        goal.getContext().makeChoicePoint(scopeGoal.makeClone(), goal.getAssociatedObject());

    TermList result = null;
    TermList currentList = null;

    while (true) {
      final Term nextTemplate = find_goal.prove();

      if (nextTemplate == null) {
        break;
      }

      final Term templateCopy = template.makeClone();
      final Term scopeGoalCopy = scopeGoal.makeClone();
      templateCopy.arrangeVariablesWith(scopeGoalCopy);

      assertUnify(scopeGoalCopy, nextTemplate);
      // good, add to the list
      if (result == null) {
        // first
        result = newList(templateCopy.tryGround().makeClone());
        currentList = result;
      } else {
        // not first
        currentList =
            createOrAppendToList(currentList, templateCopy.tryGround().makeClone());
      }
    }

    if (result == null) {
      result = NULL_LIST;
    }

    return instances.unifyWith(result);
  }

  @JProlPredicate(signature = "bagof/3", validate = {
      "?term,+callable,?list"}, reference = "Unify Bag with the alternatives of Template. If Goal has free variables besides the one sharing with Template, bagof/3 will backtrack over the alternatives of these free variables, unifying Bag with the corresponding alternatives of Template. The construct +TermVar^Goal tells bagof/3 not to bind TermVar in Goal. bagof/3 fails if Goal has no solutions.")
  public static boolean predicateBAGOF(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term templateTerm = predicate.getArgumentAt(0).tryGround();
    final Term goalTerm = predicate.getArgumentAt(1).tryGround();
    final Term bagTerm = predicate.getArgumentAt(2).tryGround();

    final class BagOfKey {

      private final Map<String, Term> variables;
      private final int hash;

      BagOfKey(final JProlChoicePoint goal, final Set<String> excludedVariables) {
        final Map<String, Term> variablesSnapshot = goal.findAllGroundedVars();
        excludedVariables.forEach(variablesSnapshot::remove);
        final List<String> orderedNames = new ArrayList<>(variablesSnapshot.keySet());
        Collections.sort(orderedNames);
        this.hash = orderedNames.stream().map(n -> variablesSnapshot.get(n).getText())
            .collect(Collectors.joining(":")).hashCode();
        this.variables = variablesSnapshot;
      }

      public void restoreVarValues(final JProlChoicePoint goal) {
        final Term goalTerm = goal.getGoalTerm();
        goalTerm.variables().forEach(v -> {
          final Term variableValue = this.variables.get(v.getText());
          if (variableValue != null && !v.unifyWith(variableValue)) {
            throw new Error("Unexpectedly can't unify data");
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

        if (that instanceof BagOfKey &&
            ((BagOfKey) that).variables.size() == this.variables.size()) {
          final BagOfKey thatKey = (BagOfKey) that;
          result = this.variables.entrySet().stream()
              .allMatch(e -> thatKey.variables.containsKey(e.getKey()) &&
                  thatKey.variables.get(e.getKey()).isUnifiableWith(e.getValue()));
        }
        return result;
      }

    }

    Map<BagOfKey, TermList> bagOfMap = goal.getInternalObject();

    if (bagOfMap == null) {
      bagOfMap = new LinkedHashMap<>();

      final Set<String> excludedVars = new LazySet<>(templateTerm.findAllNamedVariables().keySet());

      Term processingGoal = goalTerm;
      while (processingGoal.getTermType() == STRUCT
          && ((TermStruct) processingGoal).getArity() == 2
          && "^".equals(((TermStruct) processingGoal).getFunctor().getText())) {

        final TermStruct theStruct = (TermStruct) processingGoal;
        final Term left = theStruct.getArgumentAt(0);

        if (left.getTermType() == VAR) {
          excludedVars.add(left.getText());
        } else {
          throw new ProlTypeErrorException("var", "Expected VAR as left side argument", left);
        }

        processingGoal = theStruct.getArgumentAt(1);
      }

      final JProlChoicePoint findGoal =
          goal.getContext().makeChoicePoint(processingGoal.makeClone(), goal.getAssociatedObject());

      while (true) {
        final Term nextTemplate = findGoal.proveIgnoringUnknownPredicates();

        if (nextTemplate == null) {
          break;
        }

        final Term templateCopy = templateTerm.makeClone();
        final Term scopeGoalCopy = processingGoal.makeClone();
        templateCopy.arrangeVariablesWith(scopeGoalCopy);

        assertUnify(scopeGoalCopy, nextTemplate);
        final BagOfKey key = new BagOfKey(findGoal, excludedVars);
        final TermList resultList;
        if (bagOfMap.containsKey(key)) {
          resultList = bagOfMap.get(key);
          createOrAppendToList(resultList, templateCopy.tryGround().makeClone());
        } else {
          resultList = newList(templateCopy.tryGround().makeClone());
          bagOfMap.put(key, resultList);
        }
      }

      goal.setInternalObject(bagOfMap);
    }

    if (bagOfMap.isEmpty()) {
      return false;
    } else {
      final BagOfKey firstFoundKey = bagOfMap.keySet().stream().findFirst().get();
      final TermList list = bagOfMap.remove(firstFoundKey);
      if (bagTerm.unifyWith(list)) {
        firstFoundKey.restoreVarValues(goal);
        return true;
      } else {
        return false;
      }
    }
  }

  @JProlPredicate(signature = "setof/3", validate = {
      "?term,+callable,?list"}, reference = "Equivalent to bagof/3, but sorts the result using sort/2 to get a sorted list of alternatives without duplicates.")
  public static boolean predicateSETOF3(final JProlChoicePoint choicePoint,
                                        final TermStruct predicate) {
    final Term template = predicate.getArgumentAt(0).tryGround();
    final Term scopeGoal = predicate.getArgumentAt(1).tryGround();
    final Term instances = predicate.getArgumentAt(2).tryGround();

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
        this.vars.keySet()
            .forEach(name -> goal.findVar(name).ifPresent(v -> v.unifyWith(this.vars.get(name))));
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
                  thatKey.vars.get(e.getKey()).isUnifiableWith(e.getValue()));
        }
        return result;
      }

    }

    Map<SofKey, TermList> preparedMap = choicePoint.getInternalObject();

    if (preparedMap == null) {
      preparedMap = new LinkedHashMap<>();

      final Set<String> excludedVars = new HashSet<>(template.findAllNamedVariables().keySet());

      Term processingGoal = scopeGoal;
      while (processingGoal.getTermType() == STRUCT
          && ((TermStruct) processingGoal).getArity() == 2
          && "^".equals(((TermStruct) processingGoal).getFunctor().getText())) {

        final TermStruct theStruct = (TermStruct) processingGoal;
        final Term left = theStruct.getArgumentAt(0);

        if (left.getTermType() == VAR) {
          excludedVars.add(left.getText());
        } else {
          throw new ProlTypeErrorException("var", "Expected VAR as left side argument", left);
        }

        processingGoal = theStruct.getArgumentAt(1);
      }

      final JProlChoicePoint find_goal = choicePoint.getContext()
          .makeChoicePoint(processingGoal.makeClone(), choicePoint.getAssociatedObject());

      while (true) {
        final Term nextTemplate = find_goal.proveIgnoringUnknownPredicates();

        if (nextTemplate == null) {
          break;
        }

        final Term templateCopy = template.makeClone();
        final Term scopeGoalCopy = processingGoal.makeClone();
        templateCopy.arrangeVariablesWith(scopeGoalCopy);

        assertUnify(scopeGoalCopy, nextTemplate);
        final SofKey key = new SofKey(find_goal, excludedVars);
        final TermList resultList;
        if (preparedMap.containsKey(key)) {
          resultList = preparedMap.get(key);
          ProlUtils.createOrAppendToList(resultList, templateCopy.tryGround().makeClone());
        } else {
          resultList = newList(templateCopy.tryGround().makeClone());
          preparedMap.put(key, resultList);
        }
      }

      final Map<SofKey, TermList> sortedMap = new LinkedHashMap<>();
      preparedMap.forEach((key, value) -> {
        final Term[] tmpArray = value.toArray(true);
        Arrays.sort(tmpArray, choicePoint);
        final TermList sortedList = TermList.listOf(
            Arrays.stream(tmpArray)
                .distinct().collect(Collectors.toList()));

        sortedMap.put(key, sortedList);
      });

      preparedMap = sortedMap;

      choicePoint.setInternalObject(preparedMap);
    }

    if (preparedMap.isEmpty()) {
      return false;
    } else {
      final SofKey firstKey = preparedMap.keySet().stream().findFirst().get();
      final TermList list = preparedMap.remove(firstKey);
      if (instances.unifyWith(list)) {
        firstKey.restoreVarValues(choicePoint);
        return true;
      } else {
        return false;
      }
    }
  }

  @JProlPredicate(determined = true, signature = "length/2", validate = {
      "?list,?number"}, reference = "True if Length represents the number of elements in List. This predicate is a true relation and can be used to find the length of a list or produce a list (holding variables) of length Length.")
  public static boolean predicateLENGTH(final JProlChoicePoint goal,
                                        final TermStruct predicate) {
    final Term list = predicate.getArgumentAt(0).tryGround();
    final Term length = predicate.getArgumentAt(1).tryGround();

    if (list.getTermType() == VAR) {
      if (length.getTermType() == VAR) {
        return list.unifyWith(NULL_LIST) && length.unifyWith(Terms.newLong(0));
      } else if (length instanceof NumericTerm) {
        final int expectedLength = length.toNumber().intValue();
        TermList result = NULL_LIST;
        for (int i = 0; i < expectedLength; i++) {
          result = Terms.newList(Terms.newAnonymousVar(), result);
        }
        return list.unifyWith(result);
      } else {
        throw new ProlTypeErrorException("numeric", length);
      }
    } else if (list.getTermType() == LIST) {
      final TermList asList = (TermList) list;
      final NumericTerm calculatedLength = Terms.newLong(asList.calculateLength());
      if (length.getTermType() == VAR) {
        final TermVar asVar = (TermVar) length;
        if (asVar.isAnonymous()) {
          return true;
        }
        final Term tail = asList.findLastTail().tryGround();
        if (tail.getTermType() == VAR) {
          final TermVar tailVar = (TermVar) tail;
          if (tailVar.isPresentedInVarChain(asVar) || asVar.isPresentedInVarChain(tailVar)) {
            return false;
          }
        }
        return length.unifyWith(calculatedLength);
      }
      if (length instanceof NumericTerm) {
        return length.unifyWith(calculatedLength);
      }
      throw new ProlTypeErrorException("numeric", length);
    } else {
      return false;
    }
  }

  @JProlPredicate(determined = true, signature = "reverse/2", validate = {
      "?list,?list"}, reference = "Is true when the elements of List2 are in reverse order compared to List1.")
  public static boolean predicateREVERSE(final JProlChoicePoint goal,
                                         final TermStruct predicate) {
    final Term list1 = predicate.getArgumentAt(0).tryGround();
    final Term list2 = predicate.getArgumentAt(1).tryGround();

    if (list1.getTermType() == VAR) {
      if (list2.getTermType() == LIST) {
        return list1.unifyWith(((TermList) list2).reverse());
      } else if (list2.getTermType() == VAR) {
        return list1.unifyWith(list2) && list2.unifyWith(NULL_LIST);
      } else {
        throw new ProlTypeErrorException("list", list2);
      }
    } else if (list2.getTermType() == VAR) {
      if (list1.getTermType() == LIST) {
        return list2.unifyWith(((TermList) list1).reverse());
      } else if (list1.getTermType() == VAR) {
        return list2.unifyWith(list1) && list1.unifyWith(NULL_LIST);
      } else {
        throw new ProlTypeErrorException("list", list1);
      }
    } else if (list1.getTermType() == LIST) {
      return list2.unifyWith(((TermList) list1).reverse());
    } else if (list2.getTermType() == LIST) {
      return list1.unifyWith(((TermList) list2).reverse());
    } else {
      throw new ProlTypeErrorException("list", list1);
    }
  }

  @JProlPredicate(signature = "catch/3", validate = "+callable,?term,+callable", reference = "A goal catch(Goal, Catcher, Handler) is true if\n1. call(Goal) is true, or\n2. An exception is raised which throws a Ball that is caught by Catcher and Handler then succeeds ")
  public static boolean predicateCATCH(final JProlChoicePoint goal, final TermStruct predicate) {
    JProlChoicePoint catchGoal = goal.getInternalObject();

    final Term catching = predicate.getArgumentAt(0).tryGround();
    final Term catcher = predicate.getArgumentAt(1).tryGround();
    final Term solver = predicate.getArgumentAt(2).tryGround();

    if (catchGoal == null) {
      catchGoal = goal.getContext().makeChoicePoint(catching);
      goal.setInternalObject(catchGoal);
    }

    if (catchGoal.getGoalTerm() == solver) {
      final Term result = catchGoal.prove();

      if (result == null) {
        goal.resetLogicalAlternativesFlag();
        return false;
      } else {
        if (catchGoal.isCompleted()) {
          goal.resetLogicalAlternativesFlag();
        }
        return true;
      }
    } else {

      try {
        final Term result = catchGoal.prove();
        if (result == null) {
          goal.resetLogicalAlternativesFlag();
          return false;
        } else {
          if (catchGoal.isCompleted()) {
            goal.resetLogicalAlternativesFlag();
          }
          return true;
        }
      } catch (final ProlAbstractCatchableException ex) {
        if (catcher.unifyWith(ex.getAsStruct())) {
          catchGoal = goal.getContext().makeChoicePoint(solver, goal.getAssociatedObject());
          goal.setInternalObject(catchGoal);
          final Term result = catchGoal.prove();
          if (result == null) {
            goal.resetLogicalAlternativesFlag();
            return false;
          } else {
            if (catchGoal.isCompleted()) {
              goal.resetLogicalAlternativesFlag();
            }
            goal.setInternalObject(catchGoal);
            return true;
          }
        } else {
          return false;
        }
      }
    }
  }

  @JProlPredicate(determined = true, signature = "throw/1", validate = "+callable", reference = "Throw an exception which can be caught by catch/3")
  public static void predicateTHROW(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getArgumentAt(0).tryGround();
    final String exceptionSignature = term.getSignature();

    if ("instantiation_error/0".equals(exceptionSignature)) {
      throw new ProlInstantiationErrorException(predicate);
    }

    if ("type_error/2".equals(exceptionSignature)) {
      throw new ProlTypeErrorException(predicate.getArgumentAt(0).forWrite(),
          predicate.getArgumentAt(1));
    }

    if ("domain_error/2".equals(exceptionSignature)) {
      throw new ProlDomainErrorException(predicate.getArgumentAt(0).forWrite(),
          predicate.getArgumentAt(1));
    }

    if ("permission_error/3".equals(exceptionSignature)) {
      throw new ProlPermissionErrorException(predicate.getArgumentAt(0).forWrite(),
          predicate.getArgumentAt(1).forWrite(), predicate.getArgumentAt(2));
    }

    if ("representation_error/1".equals(exceptionSignature)) {
      throw new ProlRepresentationErrorException(predicate.getArgumentAt(0).forWrite(), predicate);
    }

    if ("evaluation_error/1".equals(exceptionSignature)) {
      throw new ProlEvaluationErrorException(predicate.getArgumentAt(0).forWrite(), predicate);
    }

    // all other errors make as custom
    //-------------------------------------
    Term arg = predicate.getArgumentAt(0);
    if (arg.getTermType() != STRUCT) {
      arg = newStruct(arg);
    }
    throw new ProlCustomErrorException(arg, predicate);
  }

  private static KeyValueTermStore findNamedTermGlobalStore(final JProlContext context,
                                                            final TermStruct predicate) {
    return context.getGlobalVariablesStore()
        .orElseThrow(() -> new ProlCriticalError(
            "Attempt to find global variables store but it is not provided for context: " +
                context.getName()));
  }

  @JProlPredicate(determined = true, signature = "nb_setval/2", validate = "+name, +term", reference = "Associate the term with the atom name or replace the currently associated value with value in global variables store. Critical error raises if global variables store is not provided for context.")
  public static void predicateNBSETVAL2(final JProlChoicePoint goal, final TermStruct predicate) {
    final String name = predicate.getArgumentAt(0).tryGround().getText();
    final Term value = predicate.getArgumentAt(1);
    final String globalValueName = makeContextAwareGlobalValueName(goal.getContext(), name);
    findNamedTermGlobalStore(goal.getContext(), predicate).setValue(globalValueName, value);
  }

  @JProlPredicate(determined = true, signature = "nb_getval/2", validate = "+name, ?term", reference = "Get the value from context named term global store associated with the global variable name and unify it with value. Raises existence_error(variable, Name) if the variable does not exist. Critical error raises if global variables store is not provided for context.")
  public static boolean predicateNBGETVAL2(final JProlChoicePoint goal,
                                           final TermStruct predicate) {
    final String name = predicate.getArgumentAt(0).tryGround().getText();
    final Term value = predicate.getArgumentAt(1).tryGround();
    final String globalValueName = makeContextAwareGlobalValueName(goal.getContext(), name);
    final Term stored =
        findNamedTermGlobalStore(goal.getContext(), predicate).findValue(globalValueName)
            .orElseThrow(() -> new ProlExistenceErrorException("variable", name, predicate));
    return value.unifyWith(stored.makeClone());
  }

  @JProlPredicate(determined = true, signature = "nb_delete/1", validate = "+name", reference = "Delete the named global variable in the context NbStore. Succeeds also if the named variable does not exist. Critical error raises if global variables store is not provided for context.")
  public static void predicateNBDELETE1(final JProlChoicePoint goal, final TermStruct predicate) {
    final String name = predicate.getArgumentAt(0).tryGround().getText();
    final String globalValueName = makeContextAwareGlobalValueName(goal.getContext(), name);
    findNamedTermGlobalStore(goal.getContext(), predicate).remove(globalValueName);
  }

  @JProlPredicate(determined = true, signature = "pause/1", validate = {
      "+integer"}, reference = "Pauses execution of the current thread for the specified time in milliseconds.")
  public static void predicatePAUSE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getArgumentAt(0).tryGround();

    final long milliseconds = term.toNumber().longValue();
    if (milliseconds > 0) {
      try {
        Thread.sleep(milliseconds);
      } catch (InterruptedException ex) {
        Thread.currentThread().interrupt();
        throw new ProlChoicePointInterruptedException("thread interrupted", goal);
      }
    }
  }

  @JProlPredicate(determined = true, signature = "addtrigger/1", validate = {
      "+predicate_indicator"}, reference = "addtrigger(somepredicate/3) is true if detected registered triggers for signature. The predicate allows to remove all registered triggers for signature.")
  public static boolean predicateREGTRIGGER1(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final TermStruct indicator =
        ProlAssertions.assertIndicator(predicate.getArgumentAt(0).tryGround());
    return goal.getContext().removeTrigger(ProlUtils.indicatorAsStringOrNull(indicator));
  }

  @JProlPredicate(determined = true, signature = "addtrigger/3", validate = {
      "+predicate_indicator,+list,+callable"}, reference = "addtrigger(somepredicate/3,['assert'],triggerhandler) is always true. The predicate allows to register a trigger handler for distinguished predicate signature. The handled trigger event can be  any combination of listed: assert, retract, abolish.")
  public static void predicateREGTRIGGER3(final JProlChoicePoint goal,
                                          final TermStruct predicate) {
    final TermStruct indicator =
        ProlAssertions.assertIndicator(predicate.getArgumentAt(0).tryGround());
    final TermList list = ProlAssertions.assertList(predicate.getArgumentAt(1).tryGround());
    final Term callable = predicate.getArgumentAt(2).tryGround();

    final String signature = ProlUtils.indicatorAsStringOrNull(indicator);
    final Term[] events = list.toArray(false);

    final Set<JProlTriggerType> types = Arrays.stream(list.toArray(false))
        .map(x -> x.getText().trim().toLowerCase(Locale.ROOT))
        .map(x -> {
          switch (x) {
            case "assert":
              return JProlTriggerType.TRIGGER_ASSERT;
            case "retract":
              return JProlTriggerType.TRIGGER_RETRACT;
            case "abolish":
              return JProlTriggerType.TRIGGER_ABOLISH;
            default:
              throw new ProlDomainErrorException("trigger", "Unsupported type of trigger: " + x,
                  predicate);
          }
        }).collect(Collectors.toSet());

    final JProlContextTriggerGoalCaller triggeringEventObserver =
        new JProlContextTriggerGoalCaller(callable);

    triggeringEventObserver.register(signature, types);
    goal.getContext().addTrigger(triggeringEventObserver);
  }

  static long nextLong(final long bound) {
    if (bound <= 0L) {
      return 0L;
    }
    return Math.abs(RND.nextLong()) % bound;
  }

  @JProlPredicate(determined = true, signature = "copy_term/2", validate = {
      "?term,?term"}, reference = "copy_term(X,Y) is true if and only if Y unifies with a term T which is a renamed copy of X.")
  public boolean predicateCOPYTERM2(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term in = predicate.getArgumentAt(0).tryGround().makeClone();
    final Term out = predicate.getArgumentAt(1).tryGround();
    return in.unifyWith(out);
  }

  @JProlPredicate(determined = true, signature = "\\+/1", validate = "+callable", reference = "\\+(Term) is true if and only if call(Term) is false.")
  public boolean predicateCannotBeProven1(final JProlChoicePoint goal,
                                          final TermStruct predicate) {
    final Term argument = predicate.getArgumentAt(0).tryGround();
    final JProlChoicePoint subGoal =
        goal.getContext().makeChoicePoint(argument, goal.getAssociatedObject());
    return subGoal.proveIgnoringUnknownPredicates() == null;
  }
}
