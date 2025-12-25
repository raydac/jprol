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

import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;

/**
 * Interface of a JProl context listener, allows to catch events from specified context.
 */
public interface JProlContextListener {

  default void onContextDispose(JProlContext source) {
  }

  default void onChoicePointTraceEvent(JProlContext source, JProlChoicePoint choicePoint,
                                       TraceEvent event) {
  }

  default void onUndefinedPredicateWarning(JProlContext source, JProlChoicePoint choicePoint,
                                           String undefinedPredicateSignature) {
  }

  /**
   * Notification about exception in an async task.
   *
   * @param source source context
   * @param taskId task id
   * @param error  exception thrown in async task
   * @since 3.0.0
   */
  default void onAsyncUncaughtTaskException(JProlContext source, final long taskId,
                                            final Throwable error) {

  }
}
