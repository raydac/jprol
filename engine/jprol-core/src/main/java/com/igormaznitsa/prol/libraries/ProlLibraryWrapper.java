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
import com.igormaznitsa.prol.annotations.WrappedPredicate;
import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.utils.Utils;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class ProlLibraryWrapper extends AbstractProlLibrary {

  private static final Logger LOG = Logger.getLogger(ProlLibraryWrapper.class.getCanonicalName());
  private static final Method EVAL_PREDICATE_HANDLER;
  private static final Method PREDICATE_HANDLER;

  static {
    try {
      EVAL_PREDICATE_HANDLER = ProlLibraryWrapper.class.getDeclaredMethod("proxyEvaluablePredicate", Goal.class, TermStruct.class);
      PREDICATE_HANDLER = ProlLibraryWrapper.class.getDeclaredMethod("proxyPredicate", Goal.class, TermStruct.class);
    } catch (SecurityException ex) {
      LOG.throwing(ProlLibraryWrapper.class.getCanonicalName(), "static()", ex);
      final Error error = new InternalError("Can't get needed method object for security restrictions");
      LOG.throwing(ProlLibraryWrapper.class.getCanonicalName(), "static()", error);
      throw error;
    } catch (NoSuchMethodException ex) {
      LOG.throwing(ProlLibraryWrapper.class.getCanonicalName(), "static()", ex);
      final Error error = new InternalError("Can't get find needed inside method object");
      LOG.throwing(ProlLibraryWrapper.class.getCanonicalName(), "static()", error);
      throw error;
    }
  }

  private final Object wrappedObject;
  private final Map<String, WrappedItem> wrappedMethodMap;

  private static final class WrappedItem {

    final PredicateProcessor processor;
    final Method method;

    public WrappedItem(final PredicateProcessor processor, final Method method) {
      this.processor = processor;
      this.method = method;
    }
  }

  private ProlLibraryWrapper(final String libUid, final Object wrappedObj) {
    super(libUid);
    this.wrappedObject = wrappedObj;
    this.wrappedMethodMap = Collections.unmodifiableMap(makePredicateProcessorMap(wrappedObj));
  }

  public static ProlLibraryWrapper makeWrapper(final Object wrappedObj) {
    if (wrappedObj == null) {
      throw new NullPointerException("You can't supply null as the argument");
    }
    final String newwrapperid = wrappedObj.toString();

    LOG.log(Level.INFO, "Making wrapper for {0}", wrappedObj);

    return new ProlLibraryWrapper(newwrapperid, wrappedObj);
  }

  private static boolean checkNameForValidity(final String name) {
    if (name == null) {
      throw new NullPointerException("Name can't be null");
    }
    if (name.length() == 0) {
      return false;
    }
    final char firstChar = name.charAt(0);

    if (Character.isWhitespace(firstChar)) {
      return false;
    }
    if (Character.isDigit(firstChar)) {
      return false;
    }
    return firstChar != '_';
  }

  private static String methodName2PredicateName(final Method method) {
    String name = method.getName().toLowerCase();
    while (!Thread.currentThread().isInterrupted()) {
      if (name.charAt(0) == '_') {
        name = name.substring(1);
      } else {
        break;
      }
    }
    if (name.length() == 0) {
      throw new IllegalArgumentException("Can't make valid predicate name from the method name \'" + method.getName() + '\'');
    }

    return name;
  }

  @Override
  protected PredicateProcessor onBeforeFindProcessorForPredicate(final String signature) {
    final WrappedItem wrappedItem = this.wrappedMethodMap.get(signature);
    return wrappedItem == null ? null : wrappedItem.processor;
  }

  public Object getWrappedObject() {
    return wrappedObject;
  }

  private Map<String, WrappedItem> makePredicateProcessorMap(final Object wrappedObject) {
    final Map<String, WrappedItem> resultMap = new HashMap<>();

    final boolean onlyStaticMethods = wrappedObject instanceof Class;
    final Class<?> classOfWrapped = onlyStaticMethods ? (Class) wrappedObject : wrappedObject.getClass();
    final Method[] methods = classOfWrapped.getDeclaredMethods();
    for (final Method meth : methods) {
      if (meth.isAnnotationPresent(WrappedPredicate.class)) {
        LOG.log(Level.INFO, "Detected wrapped predicate based on the method ''{0}''", new Object[]{meth.getName()});

        boolean processTheMethod = true;

        if (onlyStaticMethods && !Modifier.isStatic(meth.getModifiers())) {
          processTheMethod = false;
          LOG.log(Level.WARNING, "Nonstatic wrapped predicate for the method ''{0}''", new Object[]{meth.getName()});
        }

        if (processTheMethod) {
          final WrappedPredicate annot = meth.getAnnotation(WrappedPredicate.class);
          String name = annot.Name().trim().toLowerCase();
          if (name.length() == 0) {
            // need to generate predicate name
            name = methodName2PredicateName(meth);
            LOG.log(Level.WARNING, "There is not any predefined name so autogenerate ''{0}''", new Object[]{name});
          } else {
            LOG.log(Level.INFO, "Detected the predefined name for a predicate ''{0}''", new Object[]{name});
          }

          if (!checkNameForValidity(name)) {
            throw new IllegalArgumentException("Invalid predicate name detected \'" + name + "\' [" + meth.getName() + ']');
          }

          final Class<?>[] methodArguments = meth.getParameterTypes();
          final Class<?> methodResultType = meth.getReturnType();
          final boolean evaluable = methodResultType.isAssignableFrom(int.class) || methodResultType.isAssignableFrom(float.class);

          if (!evaluable) {
            // check for result
            if (!methodResultType.isAssignableFrom(void.class) && !methodResultType.isAssignableFrom(boolean.class)) {
              throw new IllegalArgumentException("Wrapped method \'" + meth.toGenericString() + "\' returns non void unsupported type [" + methodResultType.toString() + ']');
            }
          }

          final String predicateSignature = name + '/' + methodArguments.length;
          final PredicateProcessor predicateProcessor = new PredicateProcessor(this, name, evaluable ? EVAL_PREDICATE_HANDLER : PREDICATE_HANDLER, null);

          if (this.hasPredicateForSignature(predicateSignature)) {
            LOG.log(Level.WARNING, "Duplicated predicate signature ''{0}'' detected", new Object[]{predicateSignature});
            final RuntimeException error = new IllegalArgumentException("Duplicated processor for method \'" + meth.toString() + "\' signature \'" + predicateSignature + '\'');
            LOG.throwing(this.getClass().getCanonicalName(), "Duplicated processor", error);
            throw error;
          } else {
            resultMap.put(predicateSignature, new WrappedItem(predicateProcessor, meth));
          }
          LOG.log(Level.INFO, "Predicate processor for ''{0}'' has been created", predicateSignature);
        }
      }
    }
    return resultMap;
  }

  private Object handlePredicate(final Goal goal, final TermStruct predicate) {
    final ProlContext context = goal.getContext();
    final int arity = predicate.getArity();
    final String signature = predicate.getSignature();

    final WrappedItem wrappedItemForSignature = this.wrappedMethodMap.get(signature);
    if (wrappedItemForSignature == null) {
      final ProlCriticalError error = new ProlCriticalError("Can't find mapped method for signature \'" + signature + '\'');
      LOG.throwing(this.getClass().getCanonicalName(), "Can't find mapped method", error);
      throw error;
    }

    final Class<?>[] argClasses = wrappedItemForSignature.method.getParameterTypes();

    final Object[] args = new Object[arity];
    for (int li = 0; li < arity; li++) {
      args[li] = Utils.term2obj(context, predicate.getElement(li));
    }

    Object result = Boolean.TRUE;

    try {
      result = wrappedItemForSignature.method.invoke(wrappedObject, args);
    } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
      LOG.log(Level.SEVERE, "Exception during method invoke [" + wrappedItemForSignature.method.toGenericString() + "]", ex);
      if (wrappedItemForSignature.method.getReturnType().isAssignableFrom(Number.class)) {
        // it's an evaluable term, we must throw error
        throw new ProlCriticalError("Exception during a wrapped method was thrown", ex);
      } else {
        // we return false if it is not evaluable term
        result = Boolean.FALSE;
      }
    }

    return result;
  }

  @Evaluable
  @Determined
  protected Term proxyEvaluablePredicate(final Goal goal, final TermStruct predicate) {
    final Number returned = (Number) handlePredicate(goal, predicate);

    if (returned == null) {
      final NullPointerException ex = new NullPointerException("A predicate, signed as an evaluable one, has returned null [" + predicate.toString() + ']');
      LOG.log(Level.SEVERE, "Detected null as result", ex);
      throw ex;
    }

    return Utils.obj2term(returned);
  }

  @Determined
  protected boolean proxyPredicate(final Goal goal, final TermStruct predicate) {
    final Object obj = handlePredicate(goal, predicate);

    boolean result = true;

    if (obj instanceof Boolean) {
      result = ((Boolean) obj);
    }

    return result;
  }
}
