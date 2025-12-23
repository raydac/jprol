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
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newDouble;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertAtom;
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertInteger;
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertList;
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertNumber;
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertVar;
import static com.igormaznitsa.jprol.utils.ProlAssertions.isAtom;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.util.IllegalFormatException;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
public class JProlStrLibrary extends AbstractJProlLibrary {

  public JProlStrLibrary() {
    super("jprol-str-lib");
  }

  @JProlPredicate(determined = true, signature = "concat/3", args = {"+atom,+atom,?atom",
      "+atom,?atom,+atom", "?atom,+atom,+atom"},
      reference = "Concat two strings.")
  public static boolean predicateCONCAT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argFIRST = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argSECOND = predicate.getArgumentAt(1).findGroundOrSame();
    final Term argTHIRD = predicate.getArgumentAt(2).findGroundOrSame();

    if (goal.isValidateArguments()) {
      final boolean firstAtom = isAtom(argFIRST);
      final boolean secondAtom = isAtom(argSECOND);
      if (firstAtom && secondAtom) {
        if (argTHIRD.getTermType() != VAR) {
          assertAtom(argTHIRD);
        }
      } else if (firstAtom) {
        if (argSECOND.getTermType() != VAR) {
          assertAtom(argSECOND);
        }
        assertAtom(argTHIRD);
      } else if (secondAtom) {
        if (argFIRST.getTermType() != VAR) {
          assertAtom(argFIRST);
        }
        assertAtom(argTHIRD);
      } else {
        throw new ProlTypeErrorException("Illegal arguments: " + predicate, predicate);
      }
    }

    if (argFIRST.getTermType() == ATOM) {
      if (argSECOND.getTermType() == ATOM) {
        // the first case
        Term term = newAtom(argFIRST.getText() + argSECOND.getText());
        return argTHIRD.unifyTo(term);
      } else {
        // the second case
        String startText = argFIRST.getText();
        String fullText = argTHIRD.getText();
        if (startText.length() > fullText.length()) {
          return false;
        }
        String endText = fullText.substring(startText.length());
        Term second = newAtom(endText);
        return argSECOND.unifyTo(second);
      }
    } else {
      // the third case
      String endText = argSECOND.getText();
      String fullText = argTHIRD.getText();
      if (endText.length() > fullText.length()) {
        return false;
      }
      String startText = fullText.substring(0, fullText.length() - endText.length());
      Term first = newAtom(startText);
      return argFIRST.unifyTo(first);
    }
  }

  @JProlPredicate(determined = true, signature = "str_trim/2", args = {
      "+atom,?atom"}, reference = "Trim string.")
  public static boolean predicateSTRTRIM(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argRight = predicate.getArgumentAt(1).findGroundOrSame();

    if (goal.isValidateArguments()) {
      assertAtom(argLeft);
      if (argRight.getTermType() != VAR) {
        assertAtom(argRight);
      }
    }

    Term result = newAtom(argLeft.getText().trim());

    return argRight.unifyTo(result);
  }

  @SuppressWarnings("SpellCheckingInspection")
  @JProlPredicate(determined = true, signature = "frontstr/4", args = {
      "+integer,+atom,?atom,?atom"}, reference = "Extracts the first n characters from a string.")
  public static boolean predicateFRONTSTR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg1 = predicate.getArgumentAt(0).findGroundOrSame();
    final Term arg2 = predicate.getArgumentAt(1).findGroundOrSame();
    final Term arg3 = predicate.getArgumentAt(2).findGroundOrSame();
    final Term arg4 = predicate.getArgumentAt(3).findGroundOrSame();

    if (goal.isValidateArguments()) {
      assertInteger(arg1);
      assertAtom(arg2);
      if (arg3.getTermType() != VAR) {
        assertAtom(arg3);
      }
      if (arg4.getTermType() != VAR) {
        assertAtom(arg4);
      }
    }

    final int numberOfChars = arg1.toNumber().intValue();
    final String str1 = arg2.getText();

    if (numberOfChars > str1.length()) {
      return false;
    }

    final String fstr = str1.substring(0, numberOfChars);
    final String rstr = str1.substring(numberOfChars);

    return arg3.unifyTo(newAtom(fstr)) && arg4.unifyTo(newAtom(rstr));
  }

  @JProlPredicate(determined = true, signature = "str_format/3", args = {
      "+atom,+list,?atom"}, reference = "Fill template by listed terms and unify it with target atom.")
  public static boolean predicateSTR_FORMAT(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final Term template = predicate.getArgumentAt(0).findGroundOrSame();
    final Term list = predicate.getArgumentAt(1).findGroundOrSame();
    final Term target = predicate.getArgumentAt(2).findGroundOrSame();

    if (goal.isValidateArguments()) {
      assertAtom(template);
      assertList(list);
    }

    if (list.getTermType() != LIST) {
      throw new ProlTypeErrorException("list", list);
    }

    final Object[] args = ((TermList) list).streamChildren()
        .map(x -> {
          final Term y = x.findGroundOrSame();
          final Object result;
          if (y instanceof NumericTerm) {
            if (y instanceof TermLong) {
              result = y.toNumber().longValue();
            } else {
              result = y.toNumber().doubleValue();
            }
          } else {
            result = y.forWrite();
          }
          return result;
        }).toArray();
    try {
      return target.unifyTo(Terms.newAtom(String.format(template.getText(), args)));
    } catch (IllegalFormatException ex) {
      throw new ProlDomainErrorException("Expected valid Java format", template, ex);
    }
  }

  @JProlPredicate(determined = true, signature = "upper_lower/2", args = {"+atom,?atom",
      "?atom,+atom"}, reference = "Allows to make upper or lower case text version of an atom.")
  public static boolean predicateUPPERLOWER(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argRight = predicate.getArgumentAt(1).findGroundOrSame();

    if (goal.isValidateArguments()) {
      if (isAtom(argLeft)) {
        if (argRight.getTermType() != VAR) {
          assertAtom(argRight);
        }
      } else {
        assertVar(argLeft);
        assertAtom(argRight);
      }
    }

    if (argLeft.getTermType() == ATOM) {
      // the first case
      Term term = newAtom(argLeft.getText().toLowerCase());
      return argRight.unifyTo(term);
    } else {
      // the second case
      Term term = newAtom(argRight.getText().toUpperCase());
      return argLeft.unifyTo(term);
    }
  }

  @JProlPredicate(determined = true, signature = "str_len/2", args = {
      "+atom,?integer"}, reference = "Get string length.")
  public static boolean predicateSTRLEN(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argRight = predicate.getArgumentAt(1).findGroundOrSame();

    if (goal.isValidateArguments()) {
      assertAtom(argLeft);
      if (argRight.getTermType() != VAR) {
        assertInteger(argRight);
      }
    }

    TermLong result = newLong(argLeft.getText().length());

    return argRight.unifyTo(result);
  }

  @JProlPredicate(determined = true, signature = "str_int/2", args = {"+atom,?integer",
      "?atom,+integer"}, reference = "Convert a text atom to an integer atom (or back).")
  public static boolean predicateSTRINT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argRight = predicate.getArgumentAt(1).findGroundOrSame();

    if (goal.isValidateArguments()) {
      if (isAtom(argLeft)) {
        if (argRight.getTermType() != VAR) {
          assertInteger(argRight);
        }
      } else {
        assertVar(argLeft);
        assertInteger(argRight);
      }
    }

    if (argLeft.getTermType() == ATOM) {
      // the first case
      final TermLong result;
      try {
        result = newLong(argLeft.getText().trim());
      } catch (NumberFormatException ex) {
        return false;
      }
      return argRight.unifyTo(result);
    } else {
      // the second case
      Term result = newAtom(argRight.getText());
      return argLeft.unifyTo(result);
    }
  }

  @JProlPredicate(determined = true, signature = "str_real/2", args = {"+atom,?number",
      "?atom,+number"}, reference = "Convert a text atom to a real numeric atom (or back).")
  public static boolean predicateSTRREAL(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argRight = predicate.getArgumentAt(1).findGroundOrSame();

    if (goal.isValidateArguments()) {
      if (isAtom(argLeft)) {
        if (argLeft.getTermType() != VAR) {
          assertNumber(argRight);
        }
      } else {
        assertVar(argLeft);
        assertNumber(argRight);
      }
    }

    if (argLeft.getTermType() == ATOM) {
      // the first case
      final TermDouble result;
      try {
        result = newDouble(argLeft.getText().trim());
      } catch (NumberFormatException ex) {
        return false;
      }
      return argRight.unifyTo(result);
    } else {
      // the second case
      Term result = newAtom(argRight.getText());
      return argLeft.unifyTo(result);
    }
  }
}
