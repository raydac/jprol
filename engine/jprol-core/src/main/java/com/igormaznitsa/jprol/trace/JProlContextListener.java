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
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import java.util.concurrent.CompletableFuture;

/**
 * Interface of a JProl context listener, allows to catch events from specified context.
 */
public interface JProlContextListener {

  /**
   * Get notification on context dispose.
   *
   * @param source disposing context
   */
  default void onContextDispose(JProlContext source) {
  }

  /**
   * Get trace events from choice points if trace mode is on
   * @param source source context
   * @param choicePoint source choice point
   * @param event event
   */
  default void onChoicePointTraceEvent(JProlContext source, JProlChoicePoint choicePoint,
                                       TraceEvent event) {
  }

  /**
   * Get notification about some undefined predicate meet
   * @param source source context
   * @param choicePoint source choice point
   * @param undefinedPredicateSignature signature of undefined predicate like 'abort/1'
   */
  default void onUndefinedPredicateWarning(JProlContext source, JProlChoicePoint choicePoint,
                                           String undefinedPredicateSignature) {
  }

  /**
   * Notification about start of async task
   *
   * @param source      source context, must not be null
   * @param taskContext context to be used for the task
   * @param taskId      task id
   * @param startedTask future for the task
   * @since 3.0.0
   */
  default void onAsyncTaskStarted(JProlContext source, JProlContext taskContext, long taskId,
                                  CompletableFuture<Term> startedTask) {

  }

  /**
   * Notification that an async task was aborted.
   *
   * @param source    the source context for aborted task
   * @param taskId    the task id
   * @param exception an abort exception, must not be null
   */
  default void onAsyncTaskAborted(JProlContext source, JProlContext taskContext, long taskId,
                                  ProlAbortExecutionException exception) {

  }

  /**
   * Notification about exception in an async task.
   *
   * @param source source context
   * @param taskId task id
   * @param error  exception thrown in async task
   * @since 3.0.0
   */
  default void onAsyncUncaughtTaskException(JProlContext source, JProlContext taskContext,
                                            final long taskId,
                                            Throwable error) {

  }
}
