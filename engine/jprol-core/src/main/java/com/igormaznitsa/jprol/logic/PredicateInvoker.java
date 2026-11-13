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
import com.igormaznitsa.jprol.exceptions.ProlAbstractCatchableException;
import com.igormaznitsa.jprol.exceptions.ProlArgumentValidationException;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.libs.JProlBootstrapLibrary;
import com.igormaznitsa.jprol.libs.JProlCoreLibrary;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.function.Predicate;

public final class PredicateInvoker {

  private static final MethodHandles.Lookup METHOD_LOOKUP = MethodHandles.publicLookup();
  private static final Class<?> CLASS_RESULT_VOID = void.class;
  private static final Class<?> CLASS_RESULT_BOOLEAN = boolean.class;
  private static final MethodType BOOLEAN_INVOKER_TYPE =
      MethodType.methodType(boolean.class, JProlChoicePoint.class, TermStruct.class);
  private static final MethodType VOID_INVOKER_TYPE =
      MethodType.methodType(void.class, JProlChoicePoint.class, TermStruct.class);
  private static final MethodType OBJECT_INVOKER_TYPE =
      MethodType.methodType(Object.class, JProlChoicePoint.class, TermStruct.class);
  private static final MethodType EVALUABLE_INVOKER_TYPE =
      MethodType.methodType(Term.class, JProlChoicePoint.class, TermStruct.class);

  public static final PredicateInvoker NULL_INVOKER =
      new PredicateInvoker(null, false, true, false, false, x -> true, null, null);

  private final String predicateSignature;
  private final MethodHandle methodHandle;
  private final MethodHandle booleanHandle;
  private final MethodHandle voidHandle;
  private final MethodHandle objectHandle;
  private final MethodHandle evaluableHandle;
  private final DirectPredicate directPredicate;
  private final DirectEvaluable directEvaluable;
  private final AbstractJProlLibrary ownerLibrary;
  private final boolean voidResult;
  private final boolean determined;
  private final boolean evaluable;
  private final boolean changesGoalChain;
  private final boolean guarded;
  private final boolean requiresValidation;
  private final Predicate<TermStruct> termValidator;

  public PredicateInvoker(
      final AbstractJProlLibrary owner,
      final boolean guarded,
      final boolean determined,
      final boolean evaluable,
      final boolean affectsChain,
      final Predicate<TermStruct> termValidator,
      final String signature,
      final Method method
  ) {
    super();
    this.termValidator = termValidator;
    this.requiresValidation = !AutoArgumentValidator.isAcceptAll(termValidator);
    this.predicateSignature = signature;
    this.ownerLibrary = owner;
    this.guarded = guarded;

    if (method == null) {
      this.methodHandle = null;
      this.booleanHandle = null;
      this.voidHandle = null;
      this.objectHandle = null;
      this.evaluableHandle = null;
      this.directPredicate = null;
      this.directEvaluable = null;
      this.voidResult = true;
      this.determined = true;
      this.evaluable = false;
      this.changesGoalChain = false;
    } else {
      try {
        MethodHandle lookupMethodHandle = METHOD_LOOKUP.unreflect(method);
        if (!Modifier.isStatic(method.getModifiers())) {
          lookupMethodHandle = lookupMethodHandle.bindTo(this.ownerLibrary);
        }
        this.methodHandle = lookupMethodHandle;
      } catch (IllegalAccessException ex) {
        throw new Error(String
            .format("Can't process library '%s' method '%s'", owner.getLibraryUid(),
                method.getName()), ex);
      }

      final Class<?> returnType = method.getReturnType();
      this.voidResult = returnType == CLASS_RESULT_VOID;
      this.determined = determined;
      this.evaluable = evaluable;
      this.changesGoalChain = affectsChain;

      if (evaluable) {
        this.evaluableHandle = this.methodHandle.asType(EVALUABLE_INVOKER_TYPE);
        this.booleanHandle = null;
        this.voidHandle = null;
        this.objectHandle = null;
        this.directEvaluable = resolveDirectEvaluable(owner, signature);
        this.directPredicate = null;
      } else {
        this.evaluableHandle = null;
        this.directEvaluable = null;
        this.directPredicate = resolveDirectPredicate(owner, signature);
        if (returnType == CLASS_RESULT_BOOLEAN) {
          this.booleanHandle = this.methodHandle.asType(BOOLEAN_INVOKER_TYPE);
          this.voidHandle = null;
          this.objectHandle = null;
        } else if (this.voidResult) {
          this.booleanHandle = null;
          this.voidHandle = this.methodHandle.asType(VOID_INVOKER_TYPE);
          this.objectHandle = null;
        } else {
          this.booleanHandle = null;
          this.voidHandle = null;
          this.objectHandle = this.methodHandle.asType(OBJECT_INVOKER_TYPE);
        }
      }
    }
  }

  private static DirectPredicate resolveDirectPredicate(
      final AbstractJProlLibrary owner,
      final String signature
  ) {
    if (owner == null || signature == null) {
      return null;
    }
    if (owner.getClass() == JProlBootstrapLibrary.class) {
      switch (signature) {
        case "is/2":
          return JProlBootstrapLibrary::predicateIS;
        case "=/2":
          return JProlBootstrapLibrary::predicateEQU;
        case "\\=/2":
          return JProlBootstrapLibrary::predicateNOTEQU;
        case "true/0":
          return (goal, predicate) -> true;
        case "fail/0":
        case "false/0":
          return JProlBootstrapLibrary::predicateFAIL;
        case "not/1":
          return JProlBootstrapLibrary::predicateNOT;
        default:
          return null;
      }
    }
    if (owner.getClass() == JProlCoreLibrary.class) {
      final JProlCoreLibrary core = (JProlCoreLibrary) owner;
      switch (signature) {
        case "=:=/2":
          return JProlCoreLibrary::predicateARITHMETICEQU2;
        case "=\\=/2":
          return JProlCoreLibrary::predicateARITHNOTEQU2;
        case ">/2":
          return JProlCoreLibrary::predicateARITHGREATER2;
        case "</2":
          return JProlCoreLibrary::predicateARITHLESS2;
        case ">=/2":
          return JProlCoreLibrary::predicateARITHGREATEREQU2;
        case "=</2":
          return JProlCoreLibrary::predicateARITHLESSEQU2;
        case "==/2":
          return JProlCoreLibrary::predicateEQU2;
        case "\\==/2":
          return JProlCoreLibrary::predicateNOTEQU2;
        case "@</2":
          return JProlCoreLibrary::predicateTERMLESS2;
        case "@=</2":
          return JProlCoreLibrary::predicateTERMLESSEQU2;
        case "@>/2":
          return JProlCoreLibrary::predicateTERMGREATER2;
        case "@>=/2":
          return JProlCoreLibrary::predicateTERMGREATEREQU2;
        case "@=/2":
        case "=@=/2":
          return JProlCoreLibrary::predicateTermEquals;
        case "compare/3":
          return JProlCoreLibrary::predicateORDER3;
        case "->/2":
          return JProlCoreLibrary::predicateIFTHEN;
        case "\\+/1":
          return core::predicateCannotBeProven1;
        case "once/1":
          return JProlCoreLibrary::predicateONCE;
        case "call/1":
          return JProlCoreLibrary::predicateCALL;
        case "repeat/0":
          return (goal, predicate) -> true;
        case "var/1":
          return JProlCoreLibrary::predicateVAR;
        case "nonvar/1":
          return JProlCoreLibrary::predicateNONVAR;
        case "atom/1":
          return JProlCoreLibrary::predicateATOM;
        case "integer/1":
          return JProlCoreLibrary::predicateINTEGER;
        case "float/1":
          return JProlCoreLibrary::predicateFLOAT;
        case "number/1":
          return JProlCoreLibrary::predicateNUMBER;
        case "atomic/1":
          return JProlCoreLibrary::predicateATOMIC;
        case "compound/1":
          return JProlCoreLibrary::predicateCOMPOUND;
        case "ground/1":
          return JProlCoreLibrary::predicateGROUND;
        case "callable/1":
          return JProlCoreLibrary::predicateCALLABLE;
        case "functor/3":
          return JProlCoreLibrary::predicateFUNCTOR;
        case "arg/3":
          return JProlCoreLibrary::predicateARG;
        case "=../2":
          return JProlCoreLibrary::predicateUNIV;
        case "copy_term/2":
          return core::predicateCOPYTERM2;
        case "between/3":
          return JProlCoreLibrary::predicateBETWEEN;
        case "for/3":
          return JProlCoreLibrary::predicateFOR3;
        case "length/2":
          return JProlCoreLibrary::predicateLENGTH;
        case "reverse/2":
          return JProlCoreLibrary::predicateREVERSE;
        case "findall/3":
          return JProlCoreLibrary::predicateFINDALL3;
        case "catch/3":
          return JProlCoreLibrary::predicateCATCH;
        default:
          return null;
      }
    }
    return null;
  }

  private static DirectEvaluable resolveDirectEvaluable(
      final AbstractJProlLibrary owner,
      final String signature
  ) {
    if (owner == null || signature == null || owner.getClass() != JProlCoreLibrary.class) {
      return null;
    }
    switch (signature) {
      case "+/2":
        return JProlCoreLibrary::predicateADDTWO2;
      case "-/2":
        return JProlCoreLibrary::predicateSUBTWO;
      case "*/2":
        return JProlCoreLibrary::predicateMUL;
      case "//2":
        return JProlCoreLibrary::predicateDIV;
      case "///2":
      case "div/2":
        return JProlCoreLibrary::predicateIDIV2;
      case "mod/2":
        return JProlCoreLibrary::predicateMOD2;
      case "rem/2":
        return JProlCoreLibrary::predicateREM2;
      case "-/1":
        return JProlCoreLibrary::predicateNeg;
      case "+/1":
        return JProlCoreLibrary::predicateTheSame;
      case "abs/1":
        return JProlCoreLibrary::predicateABS;
      case "sign/1":
        return JProlCoreLibrary::predicateSIGN;
      case "**/2":
      case "^/2":
        return JProlCoreLibrary::predicatePOWER2;
      case "/\\/2":
        return JProlCoreLibrary::predicateBITWISEAND2;
      case "\\//2":
        return JProlCoreLibrary::predicateBITWISEOR2;
      case "xor/2":
        return JProlCoreLibrary::predicateXOR2;
      case "\\/1":
        return JProlCoreLibrary::predicateBITWISENOT1;
      case "<</2":
        return JProlCoreLibrary::predicateSHIFTL2;
      case ">>/2":
        return JProlCoreLibrary::predicateSHIFTR;
      default:
        return null;
    }
  }

  public boolean doesChangeGoalChain() {
    return this.changesGoalChain;
  }

  public boolean isDetermined() {
    return this.determined;
  }

  public boolean isEvaluable() {
    return this.evaluable;
  }

  public AbstractJProlLibrary getLibrary() {
    return this.ownerLibrary;
  }

  public String getSignature() {
    return this.predicateSignature;
  }

  public MethodHandle getMethod() {
    return this.methodHandle;
  }

  public Term executeEvaluable(final JProlChoicePoint goal, final TermStruct predicate) {
    try {
      if (this.directEvaluable != null) {
        return this.directEvaluable.evaluate(goal, predicate);
      }
      return (Term) this.evaluableHandle.invokeExact(goal, predicate);
    } catch (ProlException ex) {
      throw ex;
    } catch (ArithmeticException ex) {
      throw new ProlEvaluationErrorException(ex.getMessage(), predicate, ex);
    } catch (IllegalAccessException ex) {
      throw new ProlCriticalError("Illegal access exception at " + predicate, ex);
    } catch (Throwable thr) {
      final Throwable cause = thr.getCause();

      if (cause instanceof ArithmeticException) {
        throw new ProlEvaluationErrorException(cause.getMessage(), predicate);
      }

      if (cause instanceof ProlAbstractCatchableException) {
        throw (ProlAbstractCatchableException) cause;
      }
      if (cause instanceof ProlException) {
        throw (ProlException) cause;
      }
      throw new ProlCriticalError("Exception at [" + goal + ']', cause == null ? thr : cause);
    }
  }

  public boolean execute(final JProlChoicePoint goal, final TermStruct predicate) {
    try {
      if (this.guarded
          && !goal.getContext()
          .isGuardPredicateAllowed(this.ownerLibrary.getClass(), goal, predicate.getSignature())) {
        throw new ProlPermissionErrorException("access", "prohibited_predicate", predicate);
      }

      if (this.requiresValidation && goal.isVerify() && !this.termValidator.test(predicate)) {
        throw new ProlArgumentValidationException(
            "Violation of argument modificator or type: " + predicate.toSrcString());
      }

      if (this.directPredicate != null) {
        return this.directPredicate.invoke(goal, predicate);
      }
      if (this.booleanHandle != null) {
        return (boolean) this.booleanHandle.invokeExact(goal, predicate);
      }
      if (this.voidHandle != null) {
        this.voidHandle.invokeExact(goal, predicate);
        return true;
      }

      final Object result = this.objectHandle.invokeExact(goal, predicate);
      return result instanceof Boolean ? (Boolean) result : result != null;
    } catch (ProlException ex) {
      throw ex;
    } catch (IllegalAccessException ex) {
      throw new ProlException("Illegal access exception at " + predicate, ex);
    } catch (IllegalArgumentException ex) {
      throw new ProlArgumentValidationException("Illegal argument at " + predicate, ex);
    } catch (Throwable thr) {
      final Throwable cause = thr.getCause();

      if (cause instanceof ThreadDeath) {
        throw (ThreadDeath) cause;
      } else if (cause instanceof InterruptedException) {
        Thread.currentThread().interrupt();
      } else if (cause instanceof ProlAbstractCatchableException) {
        throw (ProlAbstractCatchableException) cause;
      } else if (cause instanceof ProlException) {
        throw (ProlException) cause;
      }
      throw new ProlCriticalError("Exception at [" + goal + ']', cause == null ? thr : cause);
    }
  }

  @Override
  public String toString() {
    if (this.predicateSignature == null) {
      return "PredicateInvoker(NULL)";
    }
    return "PredicateInvoker(signature=" + this.predicateSignature + ",methodHandle=" +
        this.methodHandle + ')';
  }

  @FunctionalInterface
  private interface DirectPredicate {
    boolean invoke(JProlChoicePoint goal, TermStruct predicate);
  }

  @FunctionalInterface
  private interface DirectEvaluable {
    Term evaluate(JProlChoicePoint goal, TermStruct predicate);
  }
}
