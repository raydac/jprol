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

package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.exceptions.ProlAbstractCatcheableException;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public final class PredicateInvoker {

  public static final PredicateInvoker NULL_PROCESSOR = new PredicateInvoker(null, null, null, null);
  private static final MethodHandles.Lookup METHOD_LOOKUP = MethodHandles.lookup();
  private static final Class<?> CLASS_RESULT_VOID = void.class;
  private final String predicateSignature;
  private final MethodHandle methodHandle;
  private final AbstractJProlLibrary ownerLibrary;
  private final boolean voidResult;
  private final boolean determined;
  private final boolean evaluable;
  private final boolean changesGoalChain;
  private final CheckingTemplate[][] templates;

  public PredicateInvoker(final AbstractJProlLibrary owner, final String signature, final Method method, final CheckingTemplate[][] templates) {
    super();
    this.predicateSignature = signature;
    this.ownerLibrary = owner;
    this.templates = templates;

    if (method == null) {
      this.methodHandle = null;
      this.voidResult = true;
      this.determined = true;
      this.evaluable = false;
      this.changesGoalChain = false;
    } else {
      try {
        MethodHandle mhandle = METHOD_LOOKUP.unreflect(method);
        if (!Modifier.isStatic(method.getModifiers())) {
          mhandle = mhandle.bindTo(this.ownerLibrary);
        }
        this.methodHandle = mhandle;
      } catch (IllegalAccessException ex) {
        throw new Error(String.format("Can't process library '%s' method '%s'", owner.getLibraryUid(), method.getName()), ex);
      }

      this.voidResult = method.getReturnType() == CLASS_RESULT_VOID;
      this.determined = determined;
      this.evaluable = evaluable;
      this.changesGoalChain = affectsChain;
    }
  }

  public final boolean doesChangeGoalChain() {
    return this.changesGoalChain;
  }

  public final boolean isDetermined() {
    return this.determined;
  }

  public final boolean isEvaluable() {
    return this.evaluable;
  }

  public final AbstractJProlLibrary getLibrary() {
    return this.ownerLibrary;
  }

  public final String getSignature() {
    return this.predicateSignature;
  }

  public final MethodHandle getMethod() {
    return this.methodHandle;
  }

  public CheckingTemplate[][] getTemplates() {
    return this.templates;
  }

  private Term[] checkTemplates(final TermStruct predicate) {
    final Term[] structelements = predicate.getElementsAsArray();
    final int structElementsNumber = predicate.getArity();

    ProlInstantiationErrorException lastException = null;

    Term[] result = null;

    for (final CheckingTemplate[] template : this.templates) {
      final int lencur = template.length;

      lastException = null;
      Term[] currentResult = null;

      try {
        if (lencur == structElementsNumber) {
          for (int ld = 0; ld < lencur; ld++) {
            final CheckingTemplate curTemplate = template[ld];
            final Term element = structelements[ld];
            if (curTemplate.isTermMustNotBeAltered(element)) {
              if (currentResult == null) {
                currentResult = new Term[lencur];
              }
              currentResult[ld] = element;
            }
          }
        }
      } catch (ProlInstantiationErrorException ex) {
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

  public final Term executeEvaluable(final ChoicePoint goal, final TermStruct predicate) {
    try {
      Term[] termsToNotBeChanged = null;
      if (goal.getContext().isTemplateValidate() && this.templates != null) {
        termsToNotBeChanged = checkTemplates(predicate);
      }

      final Object result = this.methodHandle.invoke(goal, predicate);

      if (termsToNotBeChanged != null) {
        final Term[] elements = predicate.getElementsAsArray();
        final int len = elements.length;
        for (int li = 0; li < len; li++) {
          if (termsToNotBeChanged[li] == null) {
            continue;
          }
          if (!termsToNotBeChanged[li].stronglyEqualsTo(elements[li])) {
            throw new ProlInstantiationErrorException("Nonchangeable element was changed [" + termsToNotBeChanged[li] + "<>" + elements[li] + "]", predicate);
          }
        }
      }

      return (Term) result;
    } catch (IllegalAccessException ex) {
      throw new ProlCriticalError("Illegal access exception at " + predicate, ex);
    } catch (Throwable thr) {
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
      throw new ProlCriticalError("Exception at [" + goal + ']', cause == null ? thr : cause);
    }
  }

  public final boolean execute(final ChoicePoint goal, final TermStruct predicate) {
    try {
      Term[] termsToNotBeChanged = null;
      if (goal.getContext().isTemplateValidate() && this.templates != null) {
        termsToNotBeChanged = checkTemplates(predicate);
      }

      final Object result;
      result = this.methodHandle.invoke(goal, predicate);

      if (termsToNotBeChanged != null) {
        final Term[] elements = predicate.getElementsAsArray();
        final int len = elements.length;
        for (int li = 0; li < len; li++) {
          if (termsToNotBeChanged[li] == null) {
            continue;
          }
          if (!termsToNotBeChanged[li].stronglyEqualsTo(elements[li])) {
            throw new ProlInstantiationErrorException("Non-changeable item is changed [" + termsToNotBeChanged[li] + "<>" + elements[li] + "]", predicate);
          }
        }
      }

      if (this.voidResult) {
        return true;
      } else if (result instanceof Boolean) {
        return (Boolean) result;
      } else {
        return result != null;
      }
    } catch (ProlException ex) {
      throw ex;
    } catch (IllegalAccessException ex) {
      throw new ProlException("Illegal access exception at " + predicate, ex);
    } catch (Throwable thr) {
      final Throwable cause = thr.getCause();

      if (cause instanceof ThreadDeath) {
        throw (ThreadDeath) cause;
      } else if (cause instanceof InterruptedException) {
        Thread.currentThread().interrupt();
      } else if (cause instanceof ProlAbstractCatcheableException) {
        throw (ProlAbstractCatcheableException) cause;
      } else if (cause instanceof ProlException) {
        throw (ProlException) cause;
      }
      throw new ProlCriticalError("Exception at [" + goal + ']', cause == null ? thr : cause);
    }
  }

  @Override
  public String toString() {
    return "Predicate processor " + predicateSignature + ' ' + methodHandle;
  }
}
