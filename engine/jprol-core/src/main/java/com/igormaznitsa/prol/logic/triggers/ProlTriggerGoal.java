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
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.DeferredGoal;
import com.igormaznitsa.prol.logic.ProlContext;

public class ProlTriggerGoal extends AbstractProlTrigger {

  protected final DeferredGoal goal;
  protected final ProlContext context;

  public ProlTriggerGoal(final Term goal, final ProlContext context) {
    super();
    if (context == null) {
      throw new NullPointerException("Context is null");
    }
    this.goal = new DeferredGoal(goal, context);
    this.context = context;
  }

  @Override
  public void onTriggerEvent(final TriggerEvent event) {
    if (goal != null) {
      final ChoicePoint tobesolved = goal.getNonparametrizedGoalInstance();

      while (!Thread.currentThread().isInterrupted()) {
        final Term result = tobesolved.next();
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
