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
import java.util.Map;

/**
 * Interface describes a trigger which will be notified about assert/retract
 * operations for predicates distinguished by their signatures
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface ProlTrigger {

  /**
   * Get the map containing signatures and triggered events of target predicates
   * for the trigger
   *
   * @return a map contains signatures and associated event types, must not be
   * null
   * @see ProlTriggerType
   */
  public Map<String, ProlTriggerType> getSignatures();

  /**
   * The funnction will be called when the context fidn out that there is inside
   * assert or retract operation over a predicate mapped on the trigger
   *
   * @param event the event object describes the operation over the predicate,
   * must not be null
   * @throws InterruptedException it will be thrown if handling is interrupted
   */
  public void onTriggerEvent(TriggerEvent event) throws InterruptedException;

  /**
   * The function will be called only once when a prol context (where the
   * trigger has been registered one or more times), is halting (the halted
   * state already set on the context so it is not possible to process goals)
   *
   * @param context the signaler , must not be null
   */
  public void onContextHalting(ProlContext context);
}
