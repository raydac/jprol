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
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertAtom;
import static com.igormaznitsa.jprol.utils.ProlAssertions.assertList;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
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
        switch (a.getText().toLowerCase(Locale.ROOT)) {
          case "case_insensitive":
            compileFlags |= Pattern.CASE_INSENSITIVE;
            break;
          case "multiline":
            compileFlags |= Pattern.MULTILINE;
            break;
          case "dotall":
            compileFlags |= Pattern.DOTALL;
            break;
          case "unicode_case":
            compileFlags |= Pattern.UNICODE_CASE;
            break;
          case "canon_eq":
            compileFlags |= Pattern.CANON_EQ;
            break;
          case "unix_lines":
            compileFlags |= Pattern.UNIX_LINES;
            break;
          case "literal":
            compileFlags |= Pattern.LITERAL;
            break;
          case "unicode_character_class":
            compileFlags |= Pattern.UNICODE_CHARACTER_CLASS;
            break;
          case "comments":
            compileFlags |= Pattern.COMMENTS;
            break;
          default:
            throw new ProlDomainErrorException(
                "[case_insensitive, multiline, dotall, unicode_case, canon_eq, unix_lines, literal, unicode_character_class, comments]",
                a);
        }
      }
    }
    return compileFlags;
  }

  @JProlPredicate(determined = true, signature = "regex_split/4", args = {
      "+atom,+atom,?list,+list"},
      reference = "Splits the given input sequence around matches of this pattern, it uses Java Pattern.split(). Format is regex_split(regex, text, TargetList, [options]).")
  public static boolean predicateREGEX_SPLIT(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term argRegex = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argString = predicate.getArgumentAt(1).findGroundOrSame();
    final Term argTargetList = predicate.getArgumentAt(2).findGroundOrSame();
    final Term argListOptions = predicate.getArgumentAt(3).findGroundOrSame();

    if (goal.isValidateArguments()) {
      assertAtom(argRegex);
      assertAtom(argString);
      assertList(argListOptions);
      if (argTargetList.getTermType() != VAR) {
        assertList(argTargetList);
      }
    }
    final int compileFlags = makePatternCompileFlags(argListOptions);
    final Pattern pattern;
    try {
      pattern = Pattern.compile(argRegex.getText(), compileFlags);
    } catch (Exception ex) {
      throw new ProlDomainErrorException("Expected valid Java regular expression", argRegex, ex);
    }

    final TermList resultList = TermList.asList(
        Arrays.stream(pattern.split(argString.getText())).map(Terms::newAtom)
            .collect(Collectors.toList()));
    return argTargetList.unifyTo(resultList);
  }

  @JProlPredicate(determined = true, signature = "regex_find/4", args = {
      "+atom,+atom,?list,+list"},
      reference = "Attempts to find the next subsequence of the input sequence that matches the pattern, it uses Java Matcher.find(). Format is regex_find(regex, text, TargetList, [options]).")
  public static boolean predicateREGEX_FIND(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final Term argRegex = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argString = predicate.getArgumentAt(1).findGroundOrSame();
    final Term argTargetList = predicate.getArgumentAt(2).findGroundOrSame();
    final Term argListOptions = predicate.getArgumentAt(3).findGroundOrSame();

    if (goal.isValidateArguments()) {
      assertAtom(argRegex);
      assertAtom(argString);
      assertList(argListOptions);
      if (argTargetList.getTermType() != VAR) {
        assertList(argTargetList);
      }
    }
    final int compileFlags = makePatternCompileFlags(argListOptions);
    final Pattern pattern;
    try {
      pattern = Pattern.compile(argRegex.getText(), compileFlags);
    } catch (Exception ex) {
      throw new ProlDomainErrorException("Expected valid Java regular expression", argRegex, ex);
    }

    final List<Term> foundGroups = new ArrayList<>();
    final Matcher matcher = pattern.matcher(argString.getText());
    while (matcher.find()) {
      if (matcher.groupCount() == 0) {
        foundGroups.add(Terms.newAtom(matcher.group(0)));
      } else {
        for (int g = 1; g < matcher.groupCount() + 1; g++) {
          final String groupValue = matcher.group(g);
          foundGroups.add(groupValue == null ? Terms.EMPTY_ATOM : Terms.newAtom(groupValue));
        }
      }
    }
    return argTargetList.unifyTo(TermList.asList(foundGroups));
  }

  @JProlPredicate(determined = true, signature = "regex_replace_all/4", args = {
      "+atom,+atom,+atom,?atom"},
      reference = "Replace all text for Java regex. Example: regex_replace_all('\\\\s+','my-url-with-spaces','-',Result).")
  public static boolean predicateREGEX_REPLACE_ALL(final JProlChoicePoint goal,
                                                   final TermStruct predicate) {
    final Term argRegex = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argSource = predicate.getArgumentAt(1).findGroundOrSame();
    final Term argReplacement = predicate.getArgumentAt(2).findGroundOrSame();
    final Term argTarget = predicate.getArgumentAt(3).findGroundOrSame();

    if (goal.isValidateArguments()) {
      assertAtom(argSource);
      assertAtom(argRegex);
      assertAtom(argReplacement);
    }

    final String processed;
    try {
      processed =
          argSource.getText().replaceAll(argRegex.getText(), argReplacement.getText());
    } catch (PatternSyntaxException ex) {
      throw new ProlDomainErrorException("Expected valid Java regular expression", argRegex, ex);
    }
    return argTarget.unifyTo(Terms.newAtom(processed));
  }

  @JProlPredicate(determined = true, signature = "regex_matches/3", args = {
      "+atom,+atom,+list"},
      reference = "Attempts to match the entire region against the pattern., it uses Java Matcher.matches(). Format is regex_matches(regex, text, [options]).")
  public static boolean predicateREGEX_MATCH(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term argRegex = predicate.getArgumentAt(0).findGroundOrSame();
    final Term argString = predicate.getArgumentAt(1).findGroundOrSame();
    final Term argListOptions = predicate.getArgumentAt(2).findGroundOrSame();

    if (goal.isValidateArguments()) {
      assertAtom(argRegex);
      assertAtom(argString);
      assertList(argListOptions);
    }
    final int compileFlags = makePatternCompileFlags(argListOptions);
    final Pattern pattern;
    try {
      pattern = Pattern.compile(argRegex.getText(), compileFlags);
    } catch (Exception ex) {
      throw new ProlDomainErrorException("Expected valid Java regular expression", argRegex, ex);
    }

    return pattern.matcher(argString.getText()).matches();
  }

}
