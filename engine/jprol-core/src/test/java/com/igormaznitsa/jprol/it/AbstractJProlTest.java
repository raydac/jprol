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

import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import com.igormaznitsa.jprol.libs.JProlIoLibrary;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;

import java.io.StringReader;

public abstract class AbstractJProlTest {
  public JProlContext makeTestContext() {
    return new JProlContext("test-context",
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

}
