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
import com.igormaznitsa.jprol.libs.JProlThreadLibrary;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;

import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;

import static org.junit.jupiter.api.Assertions.*;

public abstract class AbstractJProlTest {

  public JProlContext makeTestContext(final IoResourceProvider... ioProviders) {
    final JProlContext context = new JProlContext(
        new InMemoryKnowledgeContextFactory(),
        "test-context",
        new JProlCoreLibrary(),
        new JProlIoLibrary(),
        new JProlThreadLibrary()
    );

    if (ioProviders.length == 0) {
      context.addIoResourceProvider(new IoResourceProvider() {
        @Override
        public Reader findReader(final JProlContext context, final String readerId) {
          return new StringReader("");
        }

        @Override
        public Writer findWriter(final JProlContext context, final String writerId, final boolean append) {
          return new StringWriter();
        }
      });
    } else {
      for (final IoResourceProvider p : ioProviders) {
        context.addIoResourceProvider(p);
      }
    }
    return context;
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

  protected void assertProlException(final String goal, final Class<? extends ProlException> exceptionClass) {
    final JProlContext context = makeTestContext();
    final ChoicePoint thisGoal = new ChoicePoint(goal, context);
    assertThrows(exceptionClass, thisGoal::next);
  }

  protected ChoicePoint proveGoal(String goal) {
    final ChoicePoint thisGoal = this.prepareGoal(goal);
    assertNotNull(thisGoal.next());
    return thisGoal;
  }

  protected void consultAndCheckVar(final String consult, final String goal, final String varName, final Object... results) {
    final JProlContext context = makeTestContext();
    context.consult(new StringReader(consult));
    this.checkVarValues(context, goal, varName, results);
  }

  protected void checkVarValues(final String goal, final String var, final Object... result) {
    this.checkVarValues(makeTestContext(), goal, var, result);
  }

  protected void checkVarValues(JProlContext context, String goal, String varName, Object... results) {
    final ChoicePoint thisGoal = new ChoicePoint(goal, context);

    for (final Object res : results) {
      assertNotNull(thisGoal.next());
      if (res instanceof Number) {
        if (res instanceof Double) {
          assertEquals((double) res, thisGoal.getVarAsNumber(varName).doubleValue());
        } else {
          assertEquals(((Number) res).longValue(), thisGoal.getVarAsNumber(varName).longValue());
        }
      } else {
        assertEquals(res.toString(), thisGoal.getVarAsText(varName));
      }
    }
    assertNull(thisGoal.next());
  }

  protected void checkVarsAfterCall(String goal, String[][] varsAndValues) {
    checkVarsAfterCall(null, goal, varsAndValues);
  }

  protected void checkVarsAfterCall(String consult, String goal, String[][] varsAndValues) {
    assertTrue((varsAndValues.length & 1) == 0);

    final JProlContext context = makeTestContext();
    if (consult != null) {
      context.consult(new StringReader(consult));
    }
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
