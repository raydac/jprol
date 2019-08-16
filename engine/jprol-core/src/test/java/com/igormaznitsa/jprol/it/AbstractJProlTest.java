/*
 * Copyright 2019 Igor Maznitsa.
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

package com.igormaznitsa.jprol.it;

import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.kbase.inmemory.InMemoryKnowledgeContextFactory;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.libs.JProlIoLibrary;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;

import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.*;

public abstract class AbstractJProlTest {

  public JProlContext makeTestContext() {
    return new JProlContext(
        new InMemoryKnowledgeContextFactory(),
        "test-context",
        new JProlCoreLibrary(),
        new JProlIoLibrary()
    );
  }

  public JProlContext makeContextAndConsult(final String knowledgeBase) {
    final JProlContext context = this.makeTestContext();
    context.consult(new StringReader(knowledgeBase));
    return context;
  }

  protected ChoicePoint prepareGoal(String goal) {
    return prepareGoal(goal, makeTestContext());
  }

  protected ChoicePoint prepareGoal(String goal, final JProlContext context) {
    return new ChoicePoint(goal, context);
  }

  protected ChoicePoint prepareGoal(String consult, String goal) {
    return new ChoicePoint(goal, makeContextAndConsult(consult));
  }

  protected void checkException(final String goal) {
    final JProlContext context = makeTestContext();
    final ChoicePoint thisGoal = new ChoicePoint(goal, context);
    assertThrows(ProlException.class, thisGoal::next);
  }

  protected ChoicePoint proveGoal(String goal) {
    final ChoicePoint thisGoal = this.prepareGoal(goal);
    assertNotNull(thisGoal.next());
    return thisGoal;
  }

  protected void checkOnceVar(String goal, String var, Object... result) {
    this.checkOnceVar(makeTestContext(), goal, var, result);
  }

  protected void checkOnceVar(JProlContext context, String goal, String var, Object... result) {
    final ChoicePoint thisGoal = new ChoicePoint(goal, context);

    for (final Object res : result) {
      assertNotNull(thisGoal.next());
      if (res instanceof Number) {
        if (res instanceof Double) {
          assertEquals(0, Double.compare(thisGoal.getVarAsNumber(var).doubleValue(), (Double) res));
        } else {
          assertEquals(res, thisGoal.getVarAsNumber(var));
        }

      } else {
        assertEquals(res.toString(), thisGoal.getVarAsText(var));
      }
    }
    assertNull(thisGoal.next());
  }

  protected void checkVarsAfterCall(String goal, String[][] varsAndValues) {
    assertTrue((varsAndValues.length & 1) == 0);

    final JProlContext context = makeTestContext();
    final ChoicePoint thisGoal = new ChoicePoint(goal, context);

    for (int i = 0; i < varsAndValues.length / 2; i++) {
      assertNotNull(thisGoal.next(), "Index " + i);
      final int index = i * 2;
      final String[] names = varsAndValues[index];
      final String[] values = varsAndValues[index + 1];
      assertEquals(names.length, values.length);
      for (int v = 0; v < names.length; v++) {
        final TermVar thevar = thisGoal.getVarForName(names[v]);
        assertNotNull(thevar, "Can't find var: " + names[v]);
        assertEquals(values[v], thevar.getValue().getText(), i + ": Var=" + names[v]);
      }
    }
    assertNull(thisGoal.next());
  }

  protected void checkOnce(String goal, boolean expectedResult) {
    this.checkOnce("", goal, expectedResult);
  }

  protected void checkOnce(String consult, String goal, boolean expectedResult) {
    final JProlContext context = makeContextAndConsult(consult);
    final ChoicePoint thisGoal = new ChoicePoint(goal, context);
    if (expectedResult) {
      assertNotNull(thisGoal.next());
      assertNull(thisGoal.next());
    } else {
      assertNull(thisGoal.next());
    }
  }

}
