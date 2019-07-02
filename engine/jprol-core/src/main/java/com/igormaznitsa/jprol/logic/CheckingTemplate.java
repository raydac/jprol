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

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;

public class CheckingTemplate {

  private final TemplateValueType type;
  private final TemplateValueModifier modifier;

  public CheckingTemplate(final String string) {
    super();
    if (string == null || string.length() <= 3) {
      throw new IllegalArgumentException("Can\'t parse template parameter \'" + string + '\'');
    }

    this.modifier = TemplateValueModifier.findForName(string.charAt(0));
    if (modifier == null) {
      throw new IllegalArgumentException("Unsupported template modifier at \'" + string + '\'');
    }
    this.type = TemplateValueType.findForName(string.substring(1));
    if (this.type == null) {
      throw new ProlCriticalError("Unsupported template \'" + string + '\'');
    }
  }

  public final boolean isTermMustNotBeAltered(final Term term) {
    if (this.modifier.shouldCheckTemplate(term)) {
      this.type.check(term.findNonVarOrDefault(term));
    }
    return this.modifier.isShouldNotBeAltered();
  }

}
