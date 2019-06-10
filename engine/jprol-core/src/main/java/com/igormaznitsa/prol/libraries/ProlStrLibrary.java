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

import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Predicate;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermFloat;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.utils.Utils;

import static com.igormaznitsa.prol.data.TermType.ATOM;

public class ProlStrLibrary extends AbstractProlLibrary {

  public ProlStrLibrary() {
    super("ProlStrLib");
  }

  @Predicate(Signature = "concat/3", Template = {"+atom,+atom,?atom", "+atom,?atom,+atom", "?atom,+atom,+atom"}, Reference = "Concat two strings.")
  @Determined
  public static boolean predicateCONCAT(final ChoicePoint goal, final TermStruct predicate) {
    final Term argFIRST = Utils.getTermFromElement(predicate.getElement(0));
    final Term argSECOND = Utils.getTermFromElement(predicate.getElement(1));
    final Term argTHIRD = Utils.getTermFromElement(predicate.getElement(2));

    if (argFIRST.getTermType() == ATOM) {
      if (argSECOND.getTermType() == ATOM) {
        // the first case
        Term term = new Term(argFIRST.getText() + argSECOND.getText());
        return argTHIRD.unifyTo(term);
      } else {
        // the second case
        String startText = argFIRST.getText();
        String fullText = argTHIRD.getText();
        if (startText.length() > fullText.length()) {
          return false;
        }
        String endText = fullText.substring(startText.length());
        Term second = new Term(endText);
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
      Term first = new Term(startText);
      return argFIRST.unifyTo(first);
    }
  }

  @Predicate(Signature = "str_trim/2", Template = {"+atom,?atom"}, Reference = "Trim string.")
  @Determined
  public static boolean predicateSTRTRIM(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    Term result = new Term(argLeft.getText().trim());

    return argRight.unifyTo(result);
  }

  @Predicate(Signature = "frontstr/4", Template = {"+integer,+atom,?atom,?atom"}, Reference = "Extracts the first n characters from a string.")
  @Determined
  public static boolean predicateFRONTSTR(final ChoicePoint goal, final TermStruct predicate) {
    final int numberOfChars = Utils.getNumberFromElement(predicate.getElement(0)).intValue();
    final String str1 = Utils.getStringFromElement(predicate.getElement(1));

    if (numberOfChars > str1.length()) {
      return false;
    }

    final String fstr = str1.substring(0, numberOfChars);
    final String rstr = str1.substring(numberOfChars);

    final Term frontStr = Utils.getTermFromElement(predicate.getElement(2));
    final Term restStr = Utils.getTermFromElement(predicate.getElement(3));

    return frontStr.unifyTo(new Term(fstr)) && restStr.unifyTo(new Term(rstr));
  }

  @Predicate(Signature = "upper_lower/2", Template = {"+atom,?atom", "?atom,+atom"}, Reference = "Allows to make upper or lower case text version of an atom.")
  @Determined
  public static boolean predicateUPPERLOWER(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    if (argLeft.getTermType() == ATOM) {
      // the first case
      Term term = new Term(argLeft.getText().toLowerCase());
      return argRight.unifyTo(term);
    } else {
      // the second case
      Term term = new Term(argRight.getText().toUpperCase());
      return argLeft.unifyTo(term);
    }
  }

  @Predicate(Signature = "str_len/2", Template = {"+atom,?integer"}, Reference = "Get string length.")
  @Determined
  public static boolean predicateSTRLEN(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    TermInteger result = new TermInteger(argLeft.getText().length());

    return argRight.unifyTo(result);
  }

  @Predicate(Signature = "str_int/2", Template = {"+atom,?integer", "?atom,+integer"}, Reference = "Convert a text atom to an integer atom (or back).")
  @Determined
  public static boolean predicateSTRINT(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    if (argLeft.getTermType() == ATOM) {
      // the first case
      final TermInteger result;
      try {
        result = new TermInteger(argLeft.getText().trim());
      } catch (NumberFormatException ex) {
        return false;
      }
      return argRight.unifyTo(result);
    } else {
      // the second case
      Term result = new Term(argRight.getText());
      return argLeft.unifyTo(result);
    }
  }

  @Predicate(Signature = "str_real/2", Template = {"+atom,?number", "?atom,+number"}, Reference = "Convert a text atom to a real numeric atom (or back).")
  @Determined
  public static boolean predicateSTRREAL(final ChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    if (argLeft.getTermType() == ATOM) {
      // the first case
      final TermFloat result;
      try {
        result = new TermFloat(argLeft.getText().trim());
      } catch (NumberFormatException ex) {
        return false;
      }
      return argRight.unifyTo(result);
    } else {
      // the second case
      Term result = new Term(argRight.getText());
      return argLeft.unifyTo(result);
    }
  }
}
