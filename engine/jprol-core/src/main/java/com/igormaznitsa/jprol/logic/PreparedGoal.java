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

package com.igormaznitsa.jprol.logic;

import static java.util.Collections.unmodifiableList;
import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.Terms;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class PreparedGoal {

  private static final Pattern PATTERN_VAR = Pattern.compile("\\{\\?}");
  private final Term preparedGoalTerm;
  private final List<String> paramNames;

  public PreparedGoal(final Term goal) {
    this.preparedGoalTerm = goal.makeClone();
    this.paramNames = Collections.emptyList();
  }

  public PreparedGoal(final String goal, final JProlContext context) {
    final List<String> orderedVarNames = new ArrayList<>();

    final StringBuilder builder = new StringBuilder();
    int varIndex = 100;

    final Matcher matcher = PATTERN_VAR.matcher(goal);

    int lastFoundEnd = 0;
    while (matcher.find(lastFoundEnd)) {
      final String varName = "_Param" + varIndex + '_';
      varIndex++;

      builder.append(goal, lastFoundEnd, matcher.start());
      builder.append(varName);
      orderedVarNames.add(varName);

      lastFoundEnd = matcher.end();
    }
    builder.append(goal, lastFoundEnd, goal.length());

    this.paramNames = unmodifiableList(orderedVarNames);
    this.preparedGoalTerm = new JProlTreeBuilder(context)
        .readPhraseAndMakeTree(new StringReader(builder.toString())).term;
  }

  public List<String> getParamNames() {
    return this.paramNames;
  }

  @Override
  public String toString() {
    return this.preparedGoalTerm.toString();
  }

  public Term once(final JProlContext context) {
    final Term target = this.preparedGoalTerm.makeClone();
    final JProlChoicePoint goal = new JProlChoicePoint(target, context);
    return goal.prove();
  }

  public JProlChoicePoint makeChoicePoint(final JProlContext context, final long... parameters) {
    return this.makeChoicePoint(context,
        Arrays.stream(parameters).mapToObj(Terms::newLong).toArray(Term[]::new));
  }

  public JProlChoicePoint makeChoicePoint(final JProlContext context, final String... parameters) {
    return this.makeChoicePoint(context,
        Arrays.stream(parameters).map(Terms::newAtom).toArray(Term[]::new));
  }

  public JProlChoicePoint makeChoicePoint(final JProlContext context, final double... parameters) {
    return this.makeChoicePoint(context,
        Arrays.stream(parameters).mapToObj(Terms::newDouble).toArray(Term[]::new));
  }

  public JProlChoicePoint makeChoicePoint(final JProlContext context, final Term... parameters) {
    if (this.paramNames.size() != parameters.length) {
      throw new IllegalArgumentException(
          String.format("Wrong params number, expected %d", this.paramNames.size()));
    }

    final Term clonedGoal = this.preparedGoalTerm.makeClone();

    for (int i = 0; i < this.paramNames.size(); i++) {
      final String varName = this.paramNames.get(i);
      final Term parameter = requireNonNull(parameters[i]);
      clonedGoal.variables()
          .filter(x -> varName.equals(x.getText()))
          .findFirst()
          .orElseThrow(() -> new IllegalArgumentException(
              String.format("Can't find variable '%s'", varName)))
          .setValue(parameter);
    }

    return new JProlChoicePoint(clonedGoal, context);
  }

  public JProlChoicePoint makeChoicePoint(final JProlContext context,
                                          final Map<String, Term> vars) {
    final Term clonedGoal = this.preparedGoalTerm.makeClone();
    vars.forEach((key, value) -> clonedGoal.variables()
        .filter(x -> key.equals(x.getText()))
        .findFirst()
        .orElseThrow(
            () -> new IllegalArgumentException(String.format("Can't find variable '%s'", key)))
        .setValue(value));

    return new JProlChoicePoint(clonedGoal, context);
  }

  public JProlChoicePoint makeChoicePoint(final JProlContext context) {
    return new JProlChoicePoint(this.preparedGoalTerm.makeClone(), context);
  }
}
