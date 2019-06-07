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

public class ProlTriggerGoal extends AbstractProlTrigger {

  protected final PreparedGoal triggerGoal;
  protected final ProlContext context;

  public ProlTriggerGoal(final String triggerGoal, final ProlContext context, final TraceListener tracer) throws IOException, InterruptedException {
    super();
    if (context == null) {
      throw new NullPointerException("Context is null");
    }
    this.triggerGoal = triggerGoal == null ? null : new PreparedGoal(triggerGoal, context, tracer);
    this.context = context;
  }

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

      while (!Thread.currentThread().isInterrupted()) {
        final Term result = tobesolved.solve();
        if (result == null) {
          break;
        }
      }
    }
  }

  @Override
  public void onContextHalting(final ProlContext context) {

  }
}
