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

/**
 * This object is a container of parameters to notify a trigger about an event
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class TriggerEvent {

  /**
   * This varable contains the context of the event
   */
  private final ProlContext context;
  /**
   * The variable contains the normalized signature of the event predicate
   */
  private final String normalizedSignature;
  /**
   * The variable contains the event type
   */
  private final ProlTriggerType eventType;

  /**
   * The constructor
   *
   * @param context the context of the event, must not be null
   * @param normalizedSignature the predicate normalized signature, must not be
   * null
   * @param eventType the event type, must not be null
   * @throws NullPointerException if anyone from arguments is null
   */
  public TriggerEvent(final ProlContext context, final String normalizedSignature, final ProlTriggerType eventType) {
    if (context == null || normalizedSignature == null || eventType == null) {
      throw new NullPointerException();
    }
    this.context = context;
    this.normalizedSignature = normalizedSignature;
    this.eventType = eventType;
  }

  /**
   * Getter for the event context
   *
   * @return the event context
   */
  public ProlContext getContext() {
    return this.context;
  }

  /**
   * Getter for the predicate signature for the event
   *
   * @return the predicate signature as String
   */
  public String getSignature() {
    return this.normalizedSignature;
  }

  /**
   * Getter for the event type
   *
   * @return the event type
   */
  public ProlTriggerType getEventType() {
    return this.eventType;
  }

  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder("Trigger Event (context=");
    builder.append(context.toString()).append(',').append("signature=\'").append(normalizedSignature).append("\',event=").append(eventType.name()).append(')');
    return builder.toString();
  }
}
