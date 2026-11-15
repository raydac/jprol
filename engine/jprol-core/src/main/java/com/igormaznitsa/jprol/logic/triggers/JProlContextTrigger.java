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
import java.util.Map;
import java.util.Set;

/**
 * Callback notified when a context changes a predicate in its knowledge base.
 *
 * <p>Register the watched predicates before attaching the trigger. {@link JProlContext#addTrigger}
 * reads {@link #getSignatures()} once and keeps that mapping. A later change of the map is not
 * seen until the trigger is added again. Extend {@link AbstractJProlContextTrigger} and call
 * {@code register} so the signature is checked up front. Remove the trigger with
 * {@link JProlContext#removeTrigger(JProlContextTrigger)}.
 *
 * <p>{@link #onTriggerEvent} runs synchronously on the thread that asserted, retracted, or
 * abolished the clause, before that Prolog goal returns. An exception thrown there escapes to
 * that caller. {@link #onContextDispose} runs while the context is disposed; an exception there
 * is reported on the root context as
 * {@link com.igormaznitsa.jprol.exceptions.ProlContextUncaughtException}.
 *
 * <pre>{@code
 * public final class WatchParent extends AbstractJProlContextTrigger {
 *   public WatchParent() {
 *     this.register("parent/2", EnumSet.of(
 *         JProlTriggerType.TRIGGER_ASSERT, JProlTriggerType.TRIGGER_RETRACT));
 *   }
 *
 *   @Override
 *   public void onTriggerEvent(TriggerEvent event) {
 *     String signature = event.getSignature();
 *     Term changed = event.getTerm();
 *   }
 * }
 *
 * context.addTrigger(new WatchParent());
 * }</pre>
 *
 * @see JProlTriggerType
 * @see TriggerEvent
 * @since 3.0.0
 */
public interface JProlContextTrigger {

  /**
   * Predicates this trigger wants to observe, and the events for each of them.
   *
   * <p>Each key is a predicate indicator {@code name/arity}, for example {@code parent/2}. The
   * set is the subset of {@link JProlTriggerType} to deliver. The context reads this map when
   * {@link JProlContext#addTrigger} is called. Return a stable map: the engine keeps the trigger
   * objects it found, it does not subscribe to later edits of the map.
   *
   * @return predicate indicator to event types, never {@code null}
   * @throws IllegalArgumentException from {@link JProlContext#addTrigger} when a key is not a
   *                                  {@code name/arity} indicator
   */
  Map<String, Set<JProlTriggerType>> getSignatures();

  /**
   * A watched predicate was asserted, retracted, or abolished.
   *
   * <p>{@link TriggerEvent#getEventType()} says which change happened.
   * {@link TriggerEvent#getSignature()} is the predicate indicator.
   * {@link TriggerEvent#getTerm()} is the clause head that was asserted or retracted.
   * For {@link JProlTriggerType#TRIGGER_ABOLISH} the term is {@code null}, because the whole
   * predicate was removed. The default does nothing.
   *
   * @param event description of the change, never {@code null}
   * @throws RuntimeException when the implementation fails; the exception reaches the caller of
   *     the assert, retract, or abolish
   */
  default void onTriggerEvent(TriggerEvent event) {

  }

  /**
   * The context that holds this trigger has started disposal.
   *
   * <p>Called once per trigger while {@link JProlContext#dispose()} runs, after listeners and
   * before libraries. The default does nothing.
   *
   * @param context context being disposed, never {@code null}
   * @throws RuntimeException when the implementation fails; the failure is reported on the root
   *     context as {@link com.igormaznitsa.jprol.exceptions.ProlContextUncaughtException}
   */
  default void onContextDispose(JProlContext context) {

  }
}
