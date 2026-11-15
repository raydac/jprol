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

package com.igormaznitsa.jprol.trace;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.exceptions.ProlAbortExecutionException;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.util.concurrent.CompletableFuture;

/**
 * Observer of one {@link JProlContext}.
 *
 * <p>Register it with {@link JProlContext#addContextListener} and remove it with
 * {@link JProlContext#removeContextListener}. Every method has an empty default, so an
 * implementation overrides only the events it needs. The callback runs on the thread that
 * produced the event: the proving thread for trace and undefined-predicate warnings, the
 * caller of {@code dispose} for disposal, and the caller that started the task for async start.
 *
 * <p>Keep the callback short and do not call back into the same proof. An exception from
 * {@link #onContextDispose}, {@link #onLibraryAdded}, {@link #onAsyncTaskStarted},
 * {@link #onAsyncTaskAborted}, or {@link #onAsyncUncaughtTaskException} is reported on the root
 * context as {@link com.igormaznitsa.jprol.exceptions.ProlContextUncaughtException}. An exception
 * from {@link #onChoicePointTraceEvent} or {@link #onUndefinedPredicateWarning} escapes into the
 * proving thread. {@link #onLibraryRemoved} escapes from {@link JProlContext#removeLibrary}
 * and is reported as {@link com.igormaznitsa.jprol.exceptions.ProlContextUncaughtException}
 * when the removal happens during disposal.
 *
 * <pre>{@code
 * context.addContextListener(new JProlContextListener() {
 *   @Override
 *   public void onChoicePointTraceEvent(JProlContext source, JProlChoicePoint choicePoint,
 *                                        TraceEvent event) {
 *     System.out.println(event + " " + choicePoint.forTrace());
 *   }
 * });
 * }</pre>
 *
 * @see TraceEvent
 */
public interface JProlContextListener {

  /**
   * The context has started disposal.
   *
   * <p>Called once, before async tasks are cancelled and before libraries are released.
   * {@code source} is the context being disposed. The listener is dropped after disposal, so it
   * does not need to unregister itself.
   *
   * @param source disposing context, never {@code null}
   */
  default void onContextDispose(JProlContext source) {
  }

  /**
   * A trace port was reached while the {@code trace} flag is on.
   *
   * <p>Ports are {@link TraceEvent#CALL}, {@link TraceEvent#EXIT}, {@link TraceEvent#REDO}, and
   * {@link TraceEvent#FAIL}. Conjunction and disjunction goals are not ports. No call is made
   * while trace is off. {@code choicePoint} is the live goal; read it, do not drive it.
   *
   * @param source context that is proving, never {@code null}
   * @param choicePoint choice point that reached the port, never {@code null}
   * @param event port, never {@code null}
   */
  default void onChoicePointTraceEvent(JProlContext source, JProlChoicePoint choicePoint,
                                       TraceEvent event) {
  }

  /**
   * An undefined predicate was met while the {@code unknown} flag is {@code warning}.
   *
   * <p>The goal then fails. In {@code error} mode the engine throws instead of calling this
   * method. In {@code fail} mode it fails silently.
   *
   * @param source context that met the predicate, never {@code null}
   * @param choicePoint choice point of the undefined goal, never {@code null}
   * @param undefinedPredicateSignature predicate indicator such as {@code unknown_goal/1},
   *     never {@code null}
   */
  default void onUndefinedPredicateWarning(JProlContext source, JProlChoicePoint choicePoint,
                                           String undefinedPredicateSignature) {
  }

  /**
   * An async proof has been submitted and has not finished yet.
   *
   * <p>{@code startedTask} completes with the proved term, or completes exceptionally when the
   * task fails. {@code taskContext} is the context copy that runs the task. Cancel the future to
   * abort the task. Completion is reported later by {@link #onAsyncTaskAborted} or
   * {@link #onAsyncUncaughtTaskException}; a normal result is only visible on the future.
   *
   * @param source context on which the async proof was requested, never {@code null}
   * @param taskContext context copy used by the task, never {@code null}
   * @param taskId id of this task, unique for the life of the engine process
   * @param startedTask future of the task result, never {@code null}
   * @since 3.0.0
   */
  default void onAsyncTaskStarted(JProlContext source, JProlContext taskContext, long taskId,
                                  CompletableFuture<Term> startedTask) {

  }

  /**
   * An async task ended because execution was aborted.
   *
   * <p>The future of the task is already completed exceptionally. A pure interrupt of the proving
   * thread does not produce this callback.
   *
   * @param source context that started the task, never {@code null}
   * @param taskContext context copy that was proving, never {@code null}
   * @param taskId id passed to {@link #onAsyncTaskStarted}
   * @param exception abort raised by the task, never {@code null}
   */
  default void onAsyncTaskAborted(JProlContext source, JProlContext taskContext, long taskId,
                                  ProlAbortExecutionException exception) {

  }

  /**
   * An async task ended with an exception other than an abort.
   *
   * <p>The future is already completed exceptionally. {@code error} is the throwable captured from
   * the task, often a {@code ProlForkExecutionException} whose cause is the original failure.
   *
   * @param source context that started the task, never {@code null}
   * @param taskContext context copy that was proving, never {@code null}
   * @param taskId id passed to {@link #onAsyncTaskStarted}
   * @param error failure captured from the task, never {@code null}
   * @since 3.0.0
   */
  default void onAsyncUncaughtTaskException(JProlContext source, JProlContext taskContext,
                                            final long taskId,
                                            Throwable error) {

  }

  /**
   * A library was attached to the context and its consult step has finished.
   *
   * @param context context that received the library, never {@code null}
   * @param library library that was added, never {@code null}
   * @since 3.0.0
   */
  default void onLibraryAdded(JProlContext context, AbstractJProlLibrary library) {
  }

  /**
   * A library was detached from the context.
   *
   * <p>During {@link JProlContext#removeLibrary} this callback runs only when the library was
   * actually removed. During disposal it runs once per library that was still attached.
   *
   * @param context context that lost the library, never {@code null}
   * @param library library that was removed, never {@code null}
   * @since 3.0.0
   */
  default void onLibraryRemoved(JProlContext context, AbstractJProlLibrary library) {

  }
}
