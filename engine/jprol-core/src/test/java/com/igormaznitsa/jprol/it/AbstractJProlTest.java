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

import static java.util.Objects.requireNonNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.kbase.inmemory.InMemoryKnowledgeBase;
import com.igormaznitsa.jprol.libs.JProlCoreGuardedLibrary;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.libs.JProlIoLibrary;
import com.igormaznitsa.jprol.libs.JProlThreadLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.io.IoResourceProvider;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.function.Consumer;

public abstract class AbstractJProlTest {

  public static String getVarAsText(final JProlChoicePoint cp, final String varName) {
    final TermVar variable = cp.findVar(varName).orElseThrow(
        () -> new IllegalArgumentException("Unknown variable for name '" + varName + '\''));
    final Term value = variable.getValue();
    if (value == null) {
      return null;
    } else {
      return value.toSrcString();
    }
  }

  public static Number getVarAsNumber(final JProlChoicePoint cp, final String varName) {
    final TermVar variable = cp.findVar(varName).orElseThrow(
        () -> new IllegalArgumentException("Unknown variable for name '" + varName + '\''));
    return variable.toNumber();
  }

  public JProlContext makeTestContext(final IoResourceProvider... ioProviders) {
    return this.makeTestContext(a -> {
    }, ioProviders);
  }

  public JProlContext makeTestContext(final String consult,
                                      final IoResourceProvider... ioProviders) {
    final JProlContext context = this.makeTestContext(a -> {
    }, ioProviders);
    context.consult(new StringReader(consult));
    return context;
  }

  public JProlContext makeTestContext(Consumer<JProlContext> contextPostprocessor,
                                      final IoResourceProvider... ioProviders) {
    final JProlContext context = new JProlContext(
        "test-context",
        InMemoryKnowledgeBase::new,
        new JProlCoreLibrary(),
        new JProlCoreGuardedLibrary(),
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
        public Writer findWriter(final JProlContext context, final String writerId,
                                 final boolean append) {
          return new StringWriter();
        }
      });
    } else {
      for (final IoResourceProvider p : ioProviders) {
        context.addIoResourceProvider(p);
      }
    }

    contextPostprocessor.accept(context);

    return context;
  }

  public JProlContext makeContextAndConsult(final Consumer<JProlContext> contextPostProcessor,
                                            final String knowledgeBase) {
    final JProlContext context = this.makeTestContext(contextPostProcessor);
    context.consult(new StringReader(knowledgeBase));
    return context;
  }

  public JProlContext makeContextAndConsult(final String knowledgeBase) {
    return this.makeContextAndConsult(ctx -> {
    }, knowledgeBase);
  }

  public JProlContext makeContextAndConsultFromResource(final String resource) throws IOException {
    final StringBuilder knowledgeBase = new StringBuilder();
    try (final BufferedReader reader = new BufferedReader(new InputStreamReader(
        requireNonNull(this.getClass().getResourceAsStream(resource),
            () -> "Can't find " + resource), StandardCharsets.UTF_8))) {
      String line;
      while ((line = reader.readLine()) != null) {
        knowledgeBase.append(line).append('\n');
      }
    }
    return this.makeContextAndConsult(ctx -> {
    }, knowledgeBase.toString());
  }

  protected JProlChoicePoint prepareGoal(String goal) {
    return prepareGoal(goal, makeTestContext());
  }

  protected JProlChoicePoint prepareGoal(String goal, final JProlContext context) {
    return context.makeChoicePoint(goal);
  }

  protected JProlChoicePoint prepareGoal(String consult, String goal) {
    return makeContextAndConsult(consult).makeChoicePoint(goal);
  }

  protected void assertProlException(final String goal,
                                     final Class<? extends ProlException> exceptionClass) {
    final JProlContext context = makeTestContext();
    final JProlChoicePoint thisGoal = context.makeChoicePoint(goal);
    assertThrows(exceptionClass, thisGoal::prove);
  }

  protected void assertProlException(
      final String consult,
      final String goal,
      final Class<? extends ProlException> exceptionClass) {
    final JProlContext context = makeTestContext();
    context.consult(new StringReader(consult));
    final JProlChoicePoint thisGoal = context.makeChoicePoint(goal);
    assertThrows(exceptionClass, thisGoal::prove);
  }

  protected JProlChoicePoint proveGoal(String goal) {
    final JProlChoicePoint thisGoal = this.prepareGoal(goal);
    assertNotNull(thisGoal.prove());
    return thisGoal;
  }

  protected void consultAndCheckVar(final String consult, final String goal, final String varName,
                                    final Object... results) {
    final JProlContext context = makeTestContext();
    context.consult(new StringReader(consult));
    this.checkVarValues(context, goal, varName, results);
  }

  protected void checkVarValues(final String goal, final String var, final Object... result) {
    this.checkVarValues(makeTestContext(), goal, var, result);
  }

  protected void consultAndCheckVarValues(final String consult, final String goal, final String var,
                                          final Object... result) {
    this.checkVarValues(makeTestContext(consult), goal, var, result);
  }

  protected void checkVarValues(JProlContext context, String goal, String varName,
                                Object... results) {
    final JProlChoicePoint thisGoal = context.makeChoicePoint(goal);

    for (final Object res : results) {
      assertNotNull(thisGoal.prove());
      if (res instanceof Number) {
        if (res instanceof Double) {
          assertEquals((double) res, getVarAsNumber(thisGoal, varName).doubleValue());
        } else {
          assertEquals(((Number) res).longValue(), getVarAsNumber(thisGoal, varName).longValue());
        }
      } else {
        assertEquals(res.toString(), getVarAsText(thisGoal, varName));
      }
    }
    assertNull(thisGoal.prove());
  }

  protected void checkVarsAfterCall(String goal, String[][] varsAndValues) {
    checkVarsAfterCall(null, goal, varsAndValues);
  }

  protected void checkVarsAfterCall(String consult, String goal, String[][] varsAndValues) {
    assertEquals(0, (varsAndValues.length & 1));

    final JProlContext context = makeTestContext();
    if (consult != null) {
      context.consult(new StringReader(consult));
    }
    final JProlChoicePoint thisGoal = context.makeChoicePoint(goal);

    for (int i = 0; i < varsAndValues.length / 2; i++) {
      assertNotNull(thisGoal.prove(), "Index " + i);
      final int index = i * 2;
      final String[] names = varsAndValues[index];
      final String[] values = varsAndValues[index + 1];
      assertEquals(names.length, values.length);
      for (int v = 0; v < names.length; v++) {
        final String varName = names[v];
        final TermVar foundVar = thisGoal.findVar(varName)
            .orElseThrow(() -> new IllegalArgumentException("Can't find var: " + varName));
        assertEquals(values[v], foundVar.getValue().getText(), i + ": Var=" + varName);
      }
    }
    assertNull(thisGoal.prove());
  }

  protected void checkOnce(String goal, boolean expectedResult) {
    this.checkOnce("", goal, expectedResult);
  }

  protected void checkOnce(String consult, String goal, boolean expectedResult) {
    final JProlContext context = makeContextAndConsult(consult);
    final JProlChoicePoint thisGoal = context.makeChoicePoint(goal);
    if (expectedResult) {
      assertNotNull(thisGoal.prove());
      assertNull(thisGoal.prove());
    } else {
      assertNull(thisGoal.prove());
    }
  }

}
