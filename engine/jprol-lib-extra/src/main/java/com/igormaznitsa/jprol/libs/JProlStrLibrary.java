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

import com.igormaznitsa.jprol.annotations.Determined;
import com.igormaznitsa.jprol.annotations.Predicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.logic.ChoicePoint;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.Terms.*;

public class JProlStrLibrary extends AbstractJProlLibrary {

  public JProlStrLibrary() {
    super("jprol-str-lib");
  }

  @Predicate(signature = "concat/3", template = {"+atom,+atom,?atom", "+atom,?atom,+atom", "?atom,+atom,+atom"}, reference = "Concat two strings.")
  @Determined
  public static boolean predicateCONCAT(final ChoicePoint goal, final TermStruct predicate) {
    final Term argFIRST = predicate.getElement(0).findNonVarOrSame();
    final Term argSECOND = predicate.getElement(1).findNonVarOrSame();
    final Term argTHIRD = predicate.getElement(2).findNonVarOrSame();

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

  @Predicate(signature = "str_trim/2", template = {"+atom,?atom"}, reference = "Trim string.")
  @Determined
  public static boolean predicateSTRTRIM(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    Term result = newAtom(argLeft.getText().trim());

    return argRight.unifyTo(result);
  }

  @Predicate(signature = "frontstr/4", template = {"+integer,+atom,?atom,?atom"}, reference = "Extracts the first n characters from a string.")
  @Determined
  public static boolean predicateFRONTSTR(final ChoicePoint goal, final TermStruct predicate) {
    final int numberOfChars = predicate.getElement(0).toNumber().intValue();
    final String str1 = predicate.getElement(1).findNonVarOrDefault(null).getText();

    if (numberOfChars > str1.length()) {
      return false;
    }

    final String fstr = str1.substring(0, numberOfChars);
    final String rstr = str1.substring(numberOfChars);

    final Term frontStr = predicate.getElement(2).findNonVarOrSame();
    final Term restStr = predicate.getElement(3).findNonVarOrSame();

    return frontStr.unifyTo(newAtom(fstr)) && restStr.unifyTo(newAtom(rstr));
  }

  @Predicate(signature = "upper_lower/2", template = {"+atom,?atom", "?atom,+atom"}, reference = "Allows to make upper or lower case text version of an atom.")
  @Determined
  public static boolean predicateUPPERLOWER(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

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

  @Predicate(signature = "str_len/2", template = {"+atom,?integer"}, reference = "Get string length.")
  @Determined
  public static boolean predicateSTRLEN(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    TermLong result = newLong(argLeft.getText().length());

    return argRight.unifyTo(result);
  }

  @Predicate(signature = "str_int/2", template = {"+atom,?integer", "?atom,+integer"}, reference = "Convert a text atom to an integer atom (or back).")
  @Determined
  public static boolean predicateSTRINT(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

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

  @Predicate(signature = "str_real/2", template = {"+atom,?number", "?atom,+number"}, reference = "Convert a text atom to a real numeric atom (or back).")
  @Determined
  public static boolean predicateSTRREAL(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

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
