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

package com.igormaznitsa.jprol.logic.triggers;

import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlContext;

public class TriggerEvent {

  private final JProlContext context;
  private final Term term;
  private final String signature;
  private final JProlTriggerType eventType;

  public TriggerEvent(
      final JProlContext context,
      final Term term,
      final String signature,
      final JProlTriggerType eventType
  ) {
    this.context = requireNonNull(context);
    this.term = term;
    this.signature = requireNonNull(signature);
    this.eventType = requireNonNull(eventType);
  }

  public Term getTerm() {
    return this.term;
  }

  public JProlContext getContext() {
    return this.context;
  }

  public String getSignature() {
    return this.signature;
  }

  public JProlTriggerType getEventType() {
    return this.eventType;
  }

  @Override
  public String toString() {
    return "Trigger Event ("
        + "context=" + context.getName()
        + ", term=" + this.term
        + ", signature='" + signature + "',event=" + eventType.name() + ')';
  }
}
