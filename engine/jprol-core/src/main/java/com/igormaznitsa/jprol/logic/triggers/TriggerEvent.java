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

import com.igormaznitsa.jprol.logic.JProlContext;

import static java.util.Objects.requireNonNull;

public class TriggerEvent {

  private final JProlContext context;
  private final String normalizedSignature;
  private final JProlTriggerType eventType;

  public TriggerEvent(final JProlContext context, final String normalizedSignature, final JProlTriggerType eventType) {
    this.context = requireNonNull(context);
    this.normalizedSignature = requireNonNull(normalizedSignature);
    this.eventType = requireNonNull(eventType);
  }

  public JProlContext getContext() {
    return this.context;
  }

  public String getSignature() {
    return this.normalizedSignature;
  }

  public JProlTriggerType getEventType() {
    return this.eventType;
  }

  @Override
  public String toString() {
    return "Trigger Event (context=" + context.toString() + ',' + "signature=\'" + normalizedSignature + "\',event=" + eventType.name() + ')';
  }
}
