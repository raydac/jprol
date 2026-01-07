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

package com.igormaznitsa.jprol.extra.libs;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newDouble;
import static com.igormaznitsa.jprol.data.Terms.newLong;

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
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.util.IllegalFormatException;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
public class JProlStrLibrary extends AbstractJProlLibrary {

  public JProlStrLibrary() {
    super("jprol-str-lib");
  }

  @JProlPredicate(determined = true, signature = "concat/3", validate = {"+atom,+atom,?atom",
      "+atom,?atom,+atom", "?atom,+atom,+atom"},
      reference = "Concat two strings.")
  public static boolean predicateCONCAT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argFIRST = predicate.getArgumentAt(0).tryGround();
    final Term argSECOND = predicate.getArgumentAt(1).tryGround();
    final Term argTHIRD = predicate.getArgumentAt(2).tryGround();

    if (argFIRST.getTermType() == ATOM) {
      if (argSECOND.getTermType() == ATOM) {
        // the first case
        Term term = newAtom(argFIRST.getText() + argSECOND.getText());
        return argTHIRD.unifyWith(term);
      } else {
        // the second case
        String startText = argFIRST.getText();
        String fullText = argTHIRD.getText();
        if (startText.length() > fullText.length()) {
          return false;
        }
        String endText = fullText.substring(startText.length());
        Term second = newAtom(endText);
        return argSECOND.unifyWith(second);
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
      return argFIRST.unifyWith(first);
    }
  }

  @JProlPredicate(determined = true, signature = "str_trim/2", validate = {
      "+atom,?atom"}, reference = "Trim string.")
  public static boolean predicateSTRTRIM(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    Term result = newAtom(argLeft.getText().trim());

    return argRight.unifyWith(result);
  }

  @SuppressWarnings("SpellCheckingInspection")
  @JProlPredicate(determined = true, signature = "frontstr/4", validate = {
      "+integer,+atom,?atom,?atom"}, reference = "Extracts the first n characters from a string.")
  public static boolean predicateFRONTSTR(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg1 = predicate.getArgumentAt(0).tryGround();
    final Term arg2 = predicate.getArgumentAt(1).tryGround();
    final Term arg3 = predicate.getArgumentAt(2).tryGround();
    final Term arg4 = predicate.getArgumentAt(3).tryGround();

    final int numberOfChars = arg1.toNumber().intValue();
    final String str1 = arg2.getText();

    if (numberOfChars > str1.length()) {
      return false;
    }

    final String fstr = str1.substring(0, numberOfChars);
    final String rstr = str1.substring(numberOfChars);

    return arg3.unifyWith(newAtom(fstr)) && arg4.unifyWith(newAtom(rstr));
  }

  @JProlPredicate(determined = true, signature = "str_format/3", validate = {
      "+atom,+list,?atom"}, reference = "Fill template by listed terms and unify it with target atom.")
  public static boolean predicateSTR_FORMAT(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final Term template = predicate.getArgumentAt(0).tryGround();
    final Term list = predicate.getArgumentAt(1).tryGround();
    final Term target = predicate.getArgumentAt(2).tryGround();

    if (list.getTermType() != LIST) {
      throw new ProlTypeErrorException("list", list);
    }

    final Object[] args = ((TermList) list).streamChildren()
        .map(x -> {
          final Term y = x.tryGround();
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
      return target.unifyWith(Terms.newAtom(String.format(template.getText(), args)));
    } catch (IllegalFormatException ex) {
      throw new ProlDomainErrorException("Expected valid Java format", template, ex);
    }
  }

  @JProlPredicate(determined = true, signature = "upper_lower/2", validate = {"+atom,?atom",
      "?atom,+atom"}, reference = "Allows to make upper or lower case text version of an atom.")
  public static boolean predicateUPPERLOWER(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    if (argLeft.getTermType() == ATOM) {
      // the first case
      Term term = newAtom(argLeft.getText().toLowerCase());
      return argRight.unifyWith(term);
    } else {
      // the second case
      Term term = newAtom(argRight.getText().toUpperCase());
      return argLeft.unifyWith(term);
    }
  }

  @JProlPredicate(determined = true, signature = "str_len/2", validate = {
      "+atom,?integer"}, reference = "Get string length.")
  public static boolean predicateSTRLEN(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    TermLong result = newLong(argLeft.getText().length());

    return argRight.unifyWith(result);
  }

  @JProlPredicate(determined = true, signature = "str_int/2", validate = {"+atom,?integer",
      "?atom,+integer"}, reference = "Convert a text atom to an integer atom (or back).")
  public static boolean predicateSTRINT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    if (argLeft.getTermType() == ATOM) {
      // the first case
      final TermLong result;
      try {
        result = newLong(argLeft.getText().trim());
      } catch (NumberFormatException ex) {
        return false;
      }
      return argRight.unifyWith(result);
    } else {
      // the second case
      Term result = newAtom(argRight.getText());
      return argLeft.unifyWith(result);
    }
  }

  @JProlPredicate(determined = true, signature = "str_real/2", validate = {"+atom,?number",
      "?atom,+number"}, reference = "Convert a text atom to a real numeric atom (or back).")
  public static boolean predicateSTRREAL(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    if (argLeft.getTermType() == ATOM) {
      // the first case
      final TermDouble result;
      try {
        result = newDouble(argLeft.getText().trim());
      } catch (NumberFormatException ex) {
        return false;
      }
      return argRight.unifyWith(result);
    } else {
      // the second case
      Term result = newAtom(argRight.getText());
      return argLeft.unifyWith(result);
    }
  }
}
