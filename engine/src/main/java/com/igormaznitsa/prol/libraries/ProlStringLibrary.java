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
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.utils.Utils;

/**
 * The class implements the Prol String library allows to manipulate with
 * strings
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlStringLibrary extends ProlAbstractLibrary {

  @Predicate(Signature = "concat/3", Template = {"+atom,+atom,?atom", "+atom,?atom,+atom", "?atom,+atom,+atom"}, Reference = "Concat two strings.")
  @Determined
  public static final boolean predicateCONCAT(final Goal goal, final TermStruct predicate) {
    final Term argFIRST = Utils.getTermFromElement(predicate.getElement(0));
    final Term argSECOND = Utils.getTermFromElement(predicate.getElement(1));
    final Term argTHIRD = Utils.getTermFromElement(predicate.getElement(2));

    if (argFIRST.getTermType() == Term.TYPE_ATOM) {
      if (argSECOND.getTermType() == Term.TYPE_ATOM) {
        // the first case
        StringBuilder bldr = new StringBuilder(argFIRST.getText());
        bldr.append(argSECOND.getText());
        Term term = new Term(bldr.toString());
        return argTHIRD.Equ(term);
      }
      else {
        // the second case
        String startText = argFIRST.getText();
        String fullText = argTHIRD.getText();
        if (startText.length() > fullText.length()) {
          return false;
        }
        String endText = fullText.substring(startText.length());
        Term second = new Term(endText);
        return argSECOND.Equ(second);
      }
    }
    else {
      // the third case
      String endText = argSECOND.getText();
      String fullText = argTHIRD.getText();
      if (endText.length() > fullText.length()) {
        return false;
      }
      String startText = fullText.substring(0, fullText.length() - endText.length());
      Term first = new Term(startText);
      return argFIRST.Equ(first);
    }
  }

  @Predicate(Signature = "str_trim/2", Template = {"+atom,?atom"}, Reference = "Trim string.")
  @Determined
  public static final boolean predicateSTRTRIM(final Goal goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    Term result = new Term(argLeft.getText().trim());

    return argRight.Equ(result);
  }

  @Predicate(Signature = "upper_lower/2", Template = {"+atom,?atom", "?atom,+atom"}, Reference = "Allows to make upper or lower case text version of an atom.")
  @Determined
  public static final boolean predicateUPPERLOWER(final Goal goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    if (argLeft.getTermType() == Term.TYPE_ATOM) {
      // the first case
      Term term = new Term(argLeft.getText().toLowerCase());
      return argRight.Equ(term);
    }
    else {
      // the second case
      Term term = new Term(argRight.getText().toUpperCase());
      return argLeft.Equ(term);
    }
  }

  @Predicate(Signature = "str_len/2", Template = {"+atom,?integer"}, Reference = "Get string length.")
  @Determined
  public static final boolean predicateSTRLEN(final Goal goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    TermInteger result = new TermInteger(argLeft.getText().length());

    return argRight.Equ(result);
  }

  @Predicate(Signature = "str_int/2", Template = {"+atom,?integer", "?atom,+integer"}, Reference = "Convert a text atom to an integer atom (or back).")
  @Determined
  public static final boolean predicateSTRINT(final Goal goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    if (argLeft.getTermType() == Term.TYPE_ATOM) {
      // the first case
      TermInteger result = null;
      try {
        result = new TermInteger(argLeft.getText().trim());
      }
      catch (NumberFormatException ex) {
        return false;
      }
      return argRight.Equ(result);
    }
    else {
      // the second case
      Term result = new Term(argRight.getText());
      return argLeft.Equ(result);
    }
  }

  @Predicate(Signature = "str_real/2", Template = {"+atom,?number", "?atom,+number"}, Reference = "Convert a text atom to a real numeric atom (or back).")
  @Determined
  public static final boolean predicateSTRREAL(final Goal goal, final TermStruct predicate) {
    final Term argLeft = Utils.getTermFromElement(predicate.getElement(0));
    final Term argRight = Utils.getTermFromElement(predicate.getElement(1));

    if (argLeft.getTermType() == Term.TYPE_ATOM) {
      // the first case
      TermFloat result = null;
      try {
        result = new TermFloat(argLeft.getText().trim());
      }
      catch (NumberFormatException ex) {
        return false;
      }
      return argRight.Equ(result);
    }
    else {
      // the second case
      Term result = new Term(argRight.getText());
      return argLeft.Equ(result);
    }
  }

  /**
   * The constructor
   */
  public ProlStringLibrary() {
    super("ProlStringLib");
  }
}
