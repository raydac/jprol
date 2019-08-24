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

import com.igormaznitsa.jprol.annotations.Predicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.utils.ProlAssertions;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.*;

public class JProlStrLibrary extends AbstractJProlLibrary {

  public JProlStrLibrary() {
    super("jprol-str-lib");
  }

  @Predicate(determined = true, signature = "concat/3", args = {"+atom,+atom,?atom", "+atom,?atom,+atom", "?atom,+atom,+atom"}, reference = "Concat two strings.")
  public static boolean predicateCONCAT(final ChoicePoint goal, final TermStruct predicate) {
    final Term argFIRST = predicate.getElement(0).findNonVarOrSame();
    final Term argSECOND = predicate.getElement(1).findNonVarOrSame();
    final Term argTHIRD = predicate.getElement(2).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      final boolean firstAtom = ProlAssertions.isAtom(argFIRST);
      final boolean secondAtom = ProlAssertions.isAtom(argSECOND);
      if (firstAtom && secondAtom) {
        if (argTHIRD.getTermType() != VAR) {
          ProlAssertions.assertAtom(argTHIRD);
        }
      } else if (firstAtom) {
        if (argSECOND.getTermType() != VAR) {
          ProlAssertions.assertAtom(argSECOND);
        }
        ProlAssertions.assertAtom(argTHIRD);
      } else if (secondAtom) {
        if (argFIRST.getTermType() != VAR) {
          ProlAssertions.assertAtom(argFIRST);
        }
        ProlAssertions.assertAtom(argTHIRD);
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

  @Predicate(determined = true, signature = "str_trim/2", args = {"+atom,?atom"}, reference = "Trim string.")
  public static boolean predicateSTRTRIM(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(argLeft);
      if (argRight.getTermType() != VAR) {
        ProlAssertions.assertAtom(argRight);
      }
    }

    Term result = newAtom(argLeft.getText().trim());

    return argRight.unifyTo(result);
  }

  @Predicate(determined = true, signature = "frontstr/4", args = {"+integer,+atom,?atom,?atom"}, reference = "Extracts the first n characters from a string.")
  public static boolean predicateFRONTSTR(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg1 = predicate.getElement(0).findNonVarOrSame();
    final Term arg2 = predicate.getElement(1).findNonVarOrSame();
    final Term arg3 = predicate.getElement(2).findNonVarOrSame();
    final Term arg4 = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertInteger(arg1);
      ProlAssertions.assertAtom(arg2);
      if (arg3.getTermType() != VAR) {
        ProlAssertions.assertAtom(arg3);
      }
      if (arg4.getTermType() != VAR) {
        ProlAssertions.assertAtom(arg4);
      }
    }

    final int numberOfChars = arg1.toNumber().intValue();
    final String str1 = arg2.getText();

    if (numberOfChars > str1.length()) {
      return false;
    }

    final String fstr = str1.substring(0, numberOfChars);
    final String rstr = str1.substring(numberOfChars);

    final Term frontStr = arg3;
    final Term restStr = arg4;

    return frontStr.unifyTo(newAtom(fstr)) && restStr.unifyTo(newAtom(rstr));
  }

  @Predicate(determined = true, signature = "upper_lower/2", args = {"+atom,?atom", "?atom,+atom"}, reference = "Allows to make upper or lower case text version of an atom.")
  public static boolean predicateUPPERLOWER(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (ProlAssertions.isAtom(argLeft)) {
        if (argRight.getTermType() != VAR) {
          ProlAssertions.assertAtom(argRight);
        }
      } else {
        ProlAssertions.assertVar(argLeft);
        ProlAssertions.assertAtom(argRight);
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

  @Predicate(determined = true, signature = "str_len/2", args = {"+atom,?integer"}, reference = "Get string length.")
  public static boolean predicateSTRLEN(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(argLeft);
      if (argRight.getTermType() != VAR) {
        ProlAssertions.assertInteger(argRight);
      }
    }

    TermLong result = newLong(argLeft.getText().length());

    return argRight.unifyTo(result);
  }

  @Predicate(determined = true, signature = "str_int/2", args = {"+atom,?integer", "?atom,+integer"}, reference = "Convert a text atom to an integer atom (or back).")
  public static boolean predicateSTRINT(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (ProlAssertions.isAtom(argLeft)) {
        if (argRight.getTermType() != VAR) {
          ProlAssertions.assertInteger(argRight);
        }
      } else {
        ProlAssertions.assertVar(argLeft);
        ProlAssertions.assertInteger(argRight);
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

  @Predicate(determined = true, signature = "str_real/2", args = {"+atom,?number", "?atom,+number"}, reference = "Convert a text atom to a real numeric atom (or back).")
  public static boolean predicateSTRREAL(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      if (ProlAssertions.isAtom(argLeft)) {
        if (argLeft.getTermType() != VAR) {
          ProlAssertions.assertNumber(argRight);
        }
      } else {
        ProlAssertions.assertVar(argLeft);
        ProlAssertions.assertNumber(argRight);
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
