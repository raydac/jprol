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

package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.libraries.ProlCoreLibrary;
import com.igormaznitsa.prol.libraries.ProlIoLibrary;
import com.igormaznitsa.prol.libraries.ProlStrLibrary;
import com.igormaznitsa.prol.logic.ProlContext;

import java.io.StringReader;

public abstract class AbstractProlTest {
  public ProlContext makeTestContext() {
    return new ProlContext("test-context",
        new ProlCoreLibrary(),
        new ProlIoLibrary(),
        new ProlStrLibrary()
    );
  }

  public ProlContext makeContextAndConsult(final String knowledgeBase) {
    final ProlContext context = this.makeTestContext();
    context.consult(new StringReader(knowledgeBase));
    return context;
  }

}
