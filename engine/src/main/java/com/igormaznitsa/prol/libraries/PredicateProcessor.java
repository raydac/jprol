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
package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Evaluable;
import com.igormaznitsa.prol.annotations.ItChangesGoalChain;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.exceptions.ProlAbstractCatcheableException;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.prol.exceptions.ProlException;
import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.prol.logic.Goal;
import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * The auxulary inside class describes a predicate processor. It means that the
 * processor contains all links to method and the object instance of a library
 * which should execute the predicate.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see java.lang.reflect.Method
 * @see com.igormaznitsa.prol.libraries.ProlAbstractLibrary
 * @see com.igormaznitsa.prol.libraries.PredicateTemplate
 */
public final class PredicateProcessor {

  /**
   * The variable contains the predicate signature for the predicate which is
   * being lnked with the processor
   */
  private final String predicateSignature;
  /**
   * The variable contains the method which will be called to process the
   * predicate
   */
  private final Method method;
  /**
   * The variable contains the owner library which contains the method linked
   * with this processor
   */
  private final ProlAbstractLibrary ownerLibrary;
  /**
   * If the flag is false, then the method linked with the processor will not
   * return any result
   */
  private final boolean voidResult;
  /**
   * if the flag is true then the linked predicate is a determined one and it
   * will not place cjhoice points
   */
  private final boolean determined;
  /**
   * if the flag is true then the linked predicate is an evaluable arithmetic
   * predcicate
   */
  private final boolean evaluable;
  /**
   * if the flag is true then the linked predicate changes goal chain if it's
   * true
   */
  private final boolean changesGoalChain;
  /**
   * The array contains templates which should be checked (if they are
   * presented) before processor executing to check its arguments
   */
  private final PredicateTemplate[][] templates;
  //---inside constants for classes -------
  private static final Class<?> voidResultClass = void.class;
  private static final Class<Determined> annotationDeterminedClass = Determined.class;
  private static final Class<Evaluable> annotationEvaluableClass = Evaluable.class;
  private static final Class<ItChangesGoalChain> changesGoalChainClass = ItChangesGoalChain.class;
  //---------------------------------------
  /**
   * The constant describes a NULL_PROCESSOR which does nothing
   */
  public static final PredicateProcessor NULL_PROCESSOR = new PredicateProcessor(null, null, null, null);

  /**
   * The constructor
   *
   * @param owner the owner library instance for the processor, must not be null
   * @param signature the signature of the predicate which is linked with the
   * processor
   * @param method the method which will be called when the processor is
   * executed
   * @param templates the templates which should be checked with a predicate
   * arguments before the processor execution, can be null
   */
  protected PredicateProcessor(final ProlAbstractLibrary owner, final String signature, final Method method, final PredicateTemplate[][] templates) {
    super();
    this.predicateSignature = signature;
    this.ownerLibrary = owner;
    this.method = method;
    this.templates = templates;

    if (method == null) {
      voidResult = true;
      determined = true;
      evaluable = false;
      changesGoalChain = false;
    }
    else {
      voidResult = method.getReturnType() == voidResultClass;
      determined = method.isAnnotationPresent(annotationDeterminedClass);
      evaluable = method.isAnnotationPresent(annotationEvaluableClass);
      changesGoalChain = method.isAnnotationPresent(changesGoalChainClass);
    }
  }

  public final boolean doesChangeGoalChain() {
    return changesGoalChain;
  }

  /**
   * Get the determined flag
   *
   * @return true if the predicate has been marked as a determined one and it
   * make not any choice point
   */
  public final boolean isDetermined() {
    return determined;
  }

  /**
   * Get the evaluable flag
   *
   * @return true if the predicate can be calculated as an arithmetic predicate
   */
  public final boolean isEvaluable() {
    return evaluable;
  }

  /**
   * Get the owner library for the processor
   *
   * @return the owner library for the processor
   */
  public final ProlAbstractLibrary getLibrary() {
    return ownerLibrary;
  }

  /**
   * Get the signature of the linked predicate
   *
   * @return the signature of the linked predicate
   */
  public final String getSignature() {
    return predicateSignature;
  }

  /**
   * Get the method which will be called during the processor execution
   *
   * @return the method which implements needed functionality
   */
  public final Method getMethod() {
    return method;
  }

  /**
   * Get the templates which should be checked with predicate argument before
   * execution of the processor
   *
   * @return the templates as an array but it can be null if we don't have any
   * template
   */
  public PredicateTemplate[][] getTemplates() {
    return templates;
  }

  /**
   * Check a structure for compatibility with a template of the processor
   *
   * @param predicate the predicate to be checked, must not be null
   * @return mainly null, but if there is elements of a template which should
   * not be changed during processing, they will be returned as the array
   */
  private final Term[] checkTemplates(final TermStruct predicate) {
    final PredicateTemplate[][] templatesarray = this.templates;

    final int len = templatesarray.length;
    final Term[] structelements = predicate.getElementsAsArray();
    final int structElementsNumber = predicate.getArity();

    ProlInstantiationErrorException lastException = null;

    Term[] result = null;

    for (int li = 0; li < len; li++) {
      final PredicateTemplate[] curtemplate = templatesarray[li];
      final int lencur = curtemplate.length;

      lastException = null;
      Term[] currentResult = null;

      try {
        if (lencur == structElementsNumber) {
          for (int ld = 0; ld < lencur; ld++) {
            final PredicateTemplate curTemplate = curtemplate[ld];
            final Term element = structelements[ld];
            if (curTemplate.check(element)) {
              if (currentResult == null) {
                currentResult = new Term[lencur];
              }
              currentResult[ld] = element;
            }
          }
        }
      }
      catch (ProlInstantiationErrorException ex) {
        lastException = ex;
      }

      if (lastException == null) {
        result = currentResult;
        break;
      }
    }
    if (lastException != null) {
      throw lastException;
    }

    return result;
  }

  /**
   * Execute an evaluable predicate linked with the processor
   *
   * @param goal the goal which contain the predicate, must not be null
   * @param predicate the predicate which linked with the processor, must not be
   * null
   * @return the term as the result of the processing
   */
  public final Term executeEvaluable(final Goal goal, final TermStruct predicate) {
    try {
      Term[] nonchangeable = null;
      if (templates != null) {
        nonchangeable = checkTemplates(predicate);
      }

      Object result = null;
      result = method.invoke(ownerLibrary, goal, predicate);

      if (nonchangeable != null) {
        final Term[] elements = predicate.getElementsAsArray();
        final int len = elements.length;
        for (int li = 0; li < len; li++) {
          if (nonchangeable[li] == null) {
            continue;
          }
          if (nonchangeable[li].hasAnyDifference(elements[li])) {
            throw new ProlInstantiationErrorException("Nonchangeable element was changed [" + nonchangeable[li] + "<>" + elements[li] + "]", predicate);
          }
        }
      }

      return (Term) result;
    }
    catch (IllegalAccessException ex) {
      throw new ProlCriticalError("Illegal access exception at " + predicate, ex);
    }
    catch (InvocationTargetException thr) {
      thr.printStackTrace();

      final Throwable cause = thr.getCause();

      if (cause instanceof ArithmeticException) {
        throw new ProlEvaluationErrorException(cause.getMessage(), predicate);
      }

      if (cause instanceof ProlAbstractCatcheableException) {
        throw (ProlAbstractCatcheableException) cause;
      }
      if (cause instanceof ProlException) {
        throw (ProlException) cause;
      }
      throw new ProlCriticalError("Exception at [" + goal + ']', cause);
    }
  }

  /**
   * Execute a predicate linked with the processor
   *
   * @param goal the goal which contain the predicate, must not be null
   * @param predicate the predicate which linked with the processor, must not be
   * null
   * @return the result of the predicate processing, true if all ok and false if
   * fail
   * @throws InterruptedException it will be thrown if the thread has been
   * interrupted
   */
  public final boolean execute(final Goal goal, final TermStruct predicate) throws InterruptedException {
    try {
      Term[] nonchangeable = null;
      if (templates != null) {
        nonchangeable = checkTemplates(predicate);
      }
      if (templates != null) {
        checkTemplates(predicate);
      }

      Object result = null;
      result = method.invoke(ownerLibrary, goal, predicate);

      if (nonchangeable != null) {
        final Term[] elements = predicate.getElementsAsArray();
        final int len = elements.length;
        for (int li = 0; li < len; li++) {
          if (nonchangeable[li] == null) {
            continue;
          }
          if (nonchangeable[li].hasAnyDifference(elements[li])) {
            throw new ProlInstantiationErrorException("Nonchangeable element was changed [" + nonchangeable[li] + "<>" + elements[li] + "]", predicate);
          }
        }
      }

      if (voidResult) {
        return true;
      }
      else if (result instanceof Boolean) {
        return (Boolean) result;
      }
      else {
        return result != null;
      }
    }
    catch (IllegalAccessException ex) {
      throw new ProlException("Illegal access exception at " + predicate, ex);
    }
    catch (Exception thr) {
      if (thr instanceof ProlException) {
        throw (ProlException) thr;
      }

      final Throwable cause = thr.getCause();

      if (cause instanceof ThreadDeath) {
        throw (ThreadDeath) cause;
      }

      if (cause instanceof InterruptedException) {
        throw (InterruptedException) cause;
      }

      if (cause instanceof ProlAbstractCatcheableException) {
        throw (ProlAbstractCatcheableException) cause;
      }

      if (cause instanceof ProlException) {
        throw (ProlException) cause;
      }
      throw new ProlCriticalError("Exception at [" + goal + ']', cause);
    }
  }

  @Override
  public String toString() {
    return "Predicate processor " + predicateSignature + ' ' + method;
  }
}
