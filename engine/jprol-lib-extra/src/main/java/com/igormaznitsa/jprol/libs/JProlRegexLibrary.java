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

import static com.igormaznitsa.jprol.data.TermType.VAR;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
public class JProlRegexLibrary extends AbstractJProlLibrary {

  public JProlRegexLibrary() {
    super("jprol-regex-lib");
  }

  private static TermList splitTextAsAtomList(final String text, final Pattern pattern) {
    final List<Term> array = new ArrayList<>();
    for (final String s : pattern.split(text)) {
      array.add(Terms.newAtom(s));
    }
    return TermList.asList(array);
  }

  public static int makePatternCompileFlags(final Term flags) {
    int compileFlags = 0;
    if (flags instanceof TermList) {
      for (final Term a : ((TermList) flags)) {
        switch (a.getText().toUpperCase(Locale.ENGLISH)) {
          case "CASE_INSENSITIVE":
            compileFlags |= Pattern.CASE_INSENSITIVE;
            break;
          case "MULTILINE":
            compileFlags |= Pattern.MULTILINE;
            break;
          case "DOTALL":
            compileFlags |= Pattern.DOTALL;
            break;
          case "UNICODE_CASE":
            compileFlags |= Pattern.UNICODE_CASE;
            break;
          case "CANON_EQ":
            compileFlags |= Pattern.CANON_EQ;
            break;
          case "UNIX_LINES":
            compileFlags |= Pattern.UNIX_LINES;
            break;
          case "LITERAL":
            compileFlags |= Pattern.LITERAL;
            break;
          case "UNICODE_CHARACTER_CLASS":
            compileFlags |= Pattern.UNICODE_CHARACTER_CLASS;
            break;
          case "COMMENTS":
            compileFlags |= Pattern.COMMENTS;
            break;
          default:
            throw new ProlDomainErrorException(
                "[CASE_INSENSITIVE, MULTILINE, DOTALL, UNICODE_CASE, CANON_EQ, UNIX_LINES, LITERAL, UNICODE_CHARACTER_CLASS, COMMENTS]",
                a);
        }
      }
    }
    return compileFlags;
  }

  @JProlPredicate(determined = true, signature = "regex_split/4", args = {
      "+atom,+atom,?list,+list"},
      reference = "Splits the given input sequence around matches of this pattern, it uses Java Pattern.split(). Format is regex_split(regex, text, Target, [options]).")
  public static boolean predicateREGEX_SPLIT(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term argRegex = predicate.getElement(0).findNonVarOrSame();
    final Term argString = predicate.getElement(1).findNonVarOrSame();
    final Term argTargetList = predicate.getElement(2).findNonVarOrSame();
    final Term argListOptions = predicate.getElement(3).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertAtom(argRegex);
      ProlAssertions.assertAtom(argString);
      ProlAssertions.assertList(argListOptions);
      if (argListOptions.getTermType() != VAR) {
        ProlAssertions.assertList(argListOptions);
      }
    }
    final int compileFlags = makePatternCompileFlags(argListOptions);
    final Pattern pattern;
    try {
      pattern = Pattern.compile(argRegex.getText(), compileFlags);
    } catch (Exception ex) {
      throw new ProlDomainErrorException("Java Regular expression pattern", argRegex, ex);
    }

    final TermList resultList = TermList.asList(
        Arrays.stream(pattern.split(argString.getText())).map(Terms::newAtom)
            .collect(Collectors.toList()));
    return resultList.unifyTo(argTargetList);
  }

}
