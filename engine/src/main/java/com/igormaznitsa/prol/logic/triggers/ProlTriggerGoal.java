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

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.PreparedGoal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.trace.TraceListener;
import java.io.IOException;

/**
 * Class implements the Prol Trigger and allows to process a goal when there is
 * the trigger condition It's impossible to process the context halting state
 * with a goal trigger because the context has halted status already when it is
 * calling triggers
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlTriggerGoal extends AbstractProlTrigger {

  /**
   * Inside prepared goal which will be called when a condition
   */
  protected final PreparedGoal triggerGoal;
  /**
   * Inside variable containing work context for the trigger
   */
  protected final ProlContext context;

  /**
   * A constructor allows to make a goal from a string automatically
   *
   * @param triggerGoal the goal as String, it will be solved when there is the
   * trigger event, can be null if it is undefined
   * @param context the work context for the trigger, must not be null
   * @param tracer the tracer to listen events of the trigger goal, it can be
   * null
   * @throws IOException it will be thrown if there is any IO exception during
   * the constructor
   * @throws InterruptedException it will be thrown if the operation will be
   * interrupted
   */
  public ProlTriggerGoal(final String triggerGoal, final ProlContext context, final TraceListener tracer) throws IOException, InterruptedException {
    super();
    if (context == null) {
      throw new NullPointerException("Context is null");
    }
    this.triggerGoal = triggerGoal == null ? null : new PreparedGoal(triggerGoal, context, tracer);
    this.context = context;
  }

  /**
   * A constructor allows to make prepared trigger for already parsed goal
   *
   * @param triggerGoal the parsed goal, can be null if undefined
   * @param context the work context for the trigger, must not be null
   * @param tracer the tracer to listen events of the trigger goal, it can be
   * null
   * @throws NullPointerException if the context is null
   */
  public ProlTriggerGoal(final Term triggerGoal, final ProlContext context, final TraceListener tracer) {
    super();
    if (context == null) {
      throw new NullPointerException("Context is null");
    }
    this.triggerGoal = triggerGoal == null ? null : new PreparedGoal(triggerGoal, context, tracer);
    this.context = context;
  }

  @Override
  public void onTriggerEvent(final TriggerEvent event) throws InterruptedException {
    if (triggerGoal != null) {
      final Goal tobesolved = triggerGoal.getNonparametrizedGoalInstance();// we don't have parameters in the

      while (true) {
        final Term result = tobesolved.solve();
        if (result == null) {
          break;
        }
      }
    }
  }

  @Override
  public void onContextHalting(final ProlContext context) {
    // we have to do nothing
  }
}
