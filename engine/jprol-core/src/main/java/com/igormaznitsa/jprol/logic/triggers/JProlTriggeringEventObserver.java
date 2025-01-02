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

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.exceptions.ProlChoicePointInterruptedException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.PreparedGoal;

public class JProlTriggeringEventObserver extends AbstractJProlTrigger {

  private final PreparedGoal goal;

  public JProlTriggeringEventObserver(final Term goal) {
    super();
    this.goal = new PreparedGoal(goal);
  }

  @Override
  public void onTriggerEvent(final TriggerEvent event) {
    if (this.goal != null) {
      final JProlChoicePoint choicePoint = this.goal.makeChoicePoint(event.getContext());

      while (choicePoint.proveWithFailForUnknown() != null) {
        if (event.getContext().isDisposed()) {
          throw new ProlChoicePointInterruptedException("context disposed", choicePoint);
        }
      }
    }
  }

  @Override
  public void onContextDispose(final JProlContext context) {

  }
}
