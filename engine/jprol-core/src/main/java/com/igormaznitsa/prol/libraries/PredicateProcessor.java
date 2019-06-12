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

import com.igormaznitsa.prol.annotations.ChangesChoosePointChain;
import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Evaluable;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.exceptions.*;
import com.igormaznitsa.prol.logic.ChoicePoint;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public final class PredicateProcessor {

  public static final PredicateProcessor NULL_PROCESSOR = new PredicateProcessor(null, null, null, null);
  protected static final MethodHandles.Lookup METHOD_LOOKUP = MethodHandles.lookup();
  private static final Class<?> CLASS_RESULT_VOID = void.class;
  private final String predicateSignature;
  private final MethodHandle methodHandle;
  private final AbstractProlLibrary ownerLibrary;
  private final boolean voidResult;
  private final boolean determined;
  private final boolean evaluable;
  private final boolean changesGoalChain;
  private final CheckingTemplate[][] templates;

  protected PredicateProcessor(final AbstractProlLibrary owner, final String signature, final Method method, final CheckingTemplate[][] templates) {
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
      this.determined = method.isAnnotationPresent(Determined.class);
      this.evaluable = method.isAnnotationPresent(Evaluable.class);
      this.changesGoalChain = method.isAnnotationPresent(ChangesChoosePointChain.class);
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

  public final AbstractProlLibrary getLibrary() {
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
      if (templates != null) {
        termsToNotBeChanged = checkTemplates(predicate);
      }

      final Object result = methodHandle.invoke(goal, predicate);

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
      if (templates != null) {
        termsToNotBeChanged = checkTemplates(predicate);
      }

      final Object result;
      result = methodHandle.invoke(goal, predicate);

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

      if (voidResult) {
        return true;
      } else if (result instanceof Boolean) {
        return (Boolean) result;
      } else {
        return result != null;
      }
    } catch (IllegalAccessException ex) {
      throw new ProlException("Illegal access exception at " + predicate, ex);
    } catch (Throwable thr) {
      if (thr instanceof ProlException) {
        throw (ProlException) thr;
      }

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
