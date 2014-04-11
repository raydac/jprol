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
package com.igormaznitsa.prol.trace;

import com.igormaznitsa.prol.logic.Goal;

/**
 * Interface describes a trace listener which can get events during a goal
 * processing
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface TraceListener {

  /**
   * It will be called when the goal is called the first time
   *
   * @param goal the goal to be called
   * @return true if the goal can be processed, if it is false, the goal will
   * not be processed and it will return fail
   */
  public boolean onProlGoalCall(Goal goal);

  /**
   * It will be called when the goal is recalled
   *
   * @param goal the goal to be recalled
   * @return true if the goal can be processed, if it is false, the goal will
   * not be processed and it will return fail
   */
  public boolean onProlGoalRedo(Goal goal);

  /**
   * It will be called when a goal is failed
   *
   * @param goal the goal which one is failed
   */
  public void onProlGoalFail(Goal goal);

  /**
   * It will be called when a goal is completed
   *
   * @param goal the source goal
   */
  public void onProlGoalExit(Goal goal);
}
