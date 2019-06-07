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

package com.igormaznitsa.prol.logic.triggers;

import com.igormaznitsa.prol.logic.ProlContext;

public class TriggerEvent {

  private final ProlContext context;
  private final String normalizedSignature;
  private final ProlTriggerType eventType;

  public TriggerEvent(final ProlContext context, final String normalizedSignature, final ProlTriggerType eventType) {
    if (context == null || normalizedSignature == null || eventType == null) {
      throw new NullPointerException();
    }
    this.context = context;
    this.normalizedSignature = normalizedSignature;
    this.eventType = eventType;
  }

  public ProlContext getContext() {
    return this.context;
  }

  public String getSignature() {
    return this.normalizedSignature;
  }

  public ProlTriggerType getEventType() {
    return this.eventType;
  }

  @Override
  public String toString() {
    return "Trigger Event (context=" + context.toString() + ',' + "signature=\'" + normalizedSignature + "\',event=" + eventType.name() + ')';
  }
}
