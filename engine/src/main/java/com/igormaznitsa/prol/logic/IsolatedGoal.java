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
package com.igormaznitsa.prol.logic;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.Var;
import java.util.List;

/**
 * This class allows to make a wrapper for a Goal to avoid any changes into goal
 * parameters.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class IsolatedGoal extends Goal {

  /**
   * The variable contains the wraoped goal
   */
  private final Goal basegoal;

  @Override
  public Object getAuxObject() {
    return basegoal.getAuxObject();
  }

  /**
   * The constructor
   *
   * @param goal the goal to be wrapped
   * @throws NullPointerException if the goal is null
   */
  public IsolatedGoal(final Goal goal) {
    super();
    if (goal == null) {
      throw new NullPointerException("A Base goal must not be null");
    }
    basegoal = goal;
  }

  /**
   * Can't be called
   *
   * @throws UnsupportedOperationException
   */
  @Override
  public void cut() {
    throw new UnsupportedOperationException("Unsupported operation");
  }

  /**
   * Can't be called
   *
   * @throws UnsupportedOperationException
   */
  @Override
  public void cutLocal() {
    throw new UnsupportedOperationException("Unsupported operation");
  }

  @Override
  public List<Goal> getChainAsList() {
    return basegoal.getChainAsList();
  }

  @Override
  public ProlContext getContext() {
    return basegoal.getContext();
  }

  @Override
  public String getVarAsText(String varName) {
    return basegoal.getVarAsText(varName);
  }

  @Override
  public boolean isCompleted() {
    return basegoal.isCompleted();
  }

  /**
   * Can't be called
   *
   * @throws UnsupportedOperationException
   */
  @Override
  public void noMoreVariants() {
    throw new UnsupportedOperationException("Unsupported operation");
  }

  /**
   * Can't be called
   *
   * @throws UnsupportedOperationException
   */
  @Override
  public Goal replaceLastGoalAtChain(Term goal) {
    throw new UnsupportedOperationException("Unsupported operation");
  }

  @Override
  public Term getGoalTerm() {
    return basegoal.getGoalTerm().makeClone();
  }

  /**
   * Can't be called
   *
   * @throws UnsupportedOperationException
   */
  @Override
  public void setAuxObject(final Object obj) {
    throw new UnsupportedOperationException("Unsupported operation");
  }

  @Override
  public String toString() {
    return "{isolated}" + super.toString();
  }

  /**
   * The wrapped goal will be solved but the clone of the result will be
   * returned
   *
   * @return the clone of the base goal result or null
   * @throws InterruptedException it will be thrown if it is interrupted
   */
  @Override
  public Term solve() throws InterruptedException {
    final Term result = basegoal.solve();
    return result == null ? null : result.makeClone();
  }

  /**
   * To get the variable for its name from the wrapped goal. The clone of the
   * variable will be returned.
   *
   * @param name the variable name, must not be null
   * @return the variable or null if the variable is not found
   */
  @Override
  public Var getVarForName(final String name) {
    final Var var = basegoal.getVarForName(name);
    return var == null ? null : (Var) var.makeClone();
  }
}
