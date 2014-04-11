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
import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermFloat;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This class allows to wrap classes which are non successors of the
 * ProlAbstractLibrary which can be used as prol libraries To sign a method as a
 * predicate you need just to place the WrappedPredicate annotation before it.
 * <pre>
 * &#64;WrappedPredicate
 *  public void helloworld(String str) {
 *      System.out.println(str);
 *  }
 * </pre> The ProlLibraryWrapper will make automatic method argument marshaling.
 * It supports primitive types, Strings, arrays, Objects, Map<Object> and
 * Set<Object>
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see ProlAbstractLibrary
 * @see WrappedPredicate
 */
public final class ProlLibraryWrapper extends ProlAbstractLibrary {

  /**
   * Inside logger which is addressed by
   * Logger.getLogger(ProlLibraryWrapper.class.getCanonicalName())
   */
  protected static final Logger LOG = Logger.getLogger(ProlLibraryWrapper.class.getCanonicalName());

  /**
   * The variable contains a wrapped object
   */
  private final Object wrappedObject;

  /**
   * The method which will be used to handle evaluable predicates
   */
  private static final Method EVAL_PREDICATE_HANDLER;

  /**
   * The method which will be used to handle other predicates
   */
  private static final Method PREDICATE_HANDLER;

  /**
   * Inside map to link signature and methods of the wrapped object
   */
  private final Map<String, Method> methodMap;

  static {
    try {
      EVAL_PREDICATE_HANDLER = ProlLibraryWrapper.class.getDeclaredMethod("proxyEvaluablePredicate", Goal.class, TermStruct.class);
      PREDICATE_HANDLER = ProlLibraryWrapper.class.getDeclaredMethod("proxyPredicate", Goal.class, TermStruct.class);
    }
    catch (SecurityException ex) {
      LOG.throwing(ProlLibraryWrapper.class.getCanonicalName(), "static()", ex);
      final Error error = new InternalError("Can't get needed method object for security restrictions");
      LOG.throwing(ProlLibraryWrapper.class.getCanonicalName(), "static()", error);
      throw error;
    }
    catch (NoSuchMethodException ex) {
      LOG.throwing(ProlLibraryWrapper.class.getCanonicalName(), "static()", ex);
      final Error error = new InternalError("Can't get find needed inside method object");
      LOG.throwing(ProlLibraryWrapper.class.getCanonicalName(), "static()", error);
      throw error;
    }
  }

  /**
   * The constructor, it is private because it must not be called outside
   *
   * @param libId the identifier of the library, must not be null
   * @param wrappedObj the object to be wrapped, must not be null
   */
  private ProlLibraryWrapper(final String libId, final Object wrappedObj) {
    super(libId);
    this.methodMap = new HashMap<String, Method>();
    this.wrappedObject = wrappedObj;
    fillPredicateTable();
  }

  /**
   * Getter for the wrapped object
   *
   * @return the wrapped object
   */
  public Object getWrappedObject() {
    return wrappedObject;
  }

  /**
   * The make a wrapper instance for a class or its instance
   *
   * @param wrappedObj a Class or an Object, must not be null
   * @return a wrapper for the object as ProlLibraryWrapper
   * @throws NullPointerException if the argument is null
   */
  public static ProlLibraryWrapper makeWrapper(final Object wrappedObj) {
    if (wrappedObj == null) {
      throw new NullPointerException("You can't supply null as the argument");
    }
    final String newwrapperid = wrappedObj.toString();

    LOG.log(Level.INFO, "Making wrapper for {0}", wrappedObj);

    return new ProlLibraryWrapper(newwrapperid, wrappedObj);
  }

  /**
   * Inside function to fill inside tables for handling
   */
  @SuppressWarnings("unchecked")
  private void fillPredicateTable() {
    final boolean onlyStatic = wrappedObject instanceof Class;
    final Class<?> classOfWrapped = onlyStatic ? (Class) wrappedObject : wrappedObject.getClass();

    final Method[] methods = classOfWrapped.getDeclaredMethods();
    for (int li = 0; li < methods.length; li++) {
      final Method meth = methods[li];

      if (meth.isAnnotationPresent(WrappedPredicate.class)) {
        LOG.log(Level.INFO, "Detected wrapped predicate based on the method ''{0}''", new Object[]{meth.getName()});

        boolean processTheMethod = true;

        if (onlyStatic && !Modifier.isStatic(meth.getModifiers())) {
          processTheMethod = false;
          LOG.log(Level.WARNING, "Nonstatic wrapped predicate for the method ''{0}''", new Object[]{meth.getName()});
        }

        if (processTheMethod) {
          final WrappedPredicate annot = meth.getAnnotation(WrappedPredicate.class);
          String name = annot.Name().trim().toLowerCase();
          if (name.length() == 0) {
            // need to generate predicate name
            name = generatePredicateNameFromMethodName(meth);
            LOG.log(Level.WARNING, "There is not any predefined name so autogenerate ''{0}''", new Object[]{name});
          }
          else {
            LOG.log(Level.INFO, "Detected the predefined name for a predicate ''{0}''", new Object[]{name});
          }

          if (!checkNameForValidity(name)) {
            throw new IllegalArgumentException("Invalid predicate name detected \'" + name + "\' [" + meth.getName() + ']');
          }

          final Class<?>[] args = meth.getParameterTypes();
          final Class<?> result = meth.getReturnType();
          final boolean evaluable = result.isAssignableFrom(int.class) || result.isAssignableFrom(float.class);

          if (!evaluable) {
            // check for result
            if (!result.isAssignableFrom(void.class) && !result.isAssignableFrom(boolean.class)) {
              throw new IllegalArgumentException("Wrapped method \'" + meth.toGenericString() + "\' returns non void unsupported type [" + result.toString() + ']');
            }
          }

          final String predicateSignature = name + '/' + args.length;
          final PredicateProcessor predprocessor = new PredicateProcessor(this, name, evaluable ? EVAL_PREDICATE_HANDLER : PREDICATE_HANDLER, null);

          if (predicateMethodsMap.put(predicateSignature, predprocessor) == null) {
            // to map the method at inside mapper
            methodMap.put(predicateSignature, meth);
          }
          else {
            LOG.log(Level.WARNING, "Duplicated processor detected for predicate ''{0}''", new Object[]{predicateSignature});
            final RuntimeException runtimeexception = new IllegalArgumentException("Duplicated processor for method \'" + meth.toString() + "\' signature \'" + predicateSignature + '\'');
            LOG.throwing(this.getClass().getCanonicalName(), "Duplicated processor", runtimeexception);
            throw runtimeexception;
          }
          LOG.log(Level.INFO, "Predicate processor for ''{0}'' has been created", predicateSignature);
        }
      }
    }
  }

  /**
   * Inside function to handle a predicate
   *
   * @param goal the goal which needs to solve the predicate, must not be null
   * @param predicate the predicate (as a structure) to be solved, must not be
   * null
   * @return a NumericTerm if the predicate is an evaluable one in other case,
   * either Boolean.TRUE if solved successfully or Boolean.FALSE if it failed or
   * any exception was thrown during a handling of nonevaluable predicate
   * @throws ProlCriticalError will be thrown if it is not possible to find
   * mapped method or there was an Exception during handling of an evaluable
   * predicate
   */
  private Object handlePredicate(final Goal goal, final TermStruct predicate) {
    final ProlContext context = goal.getContext();
    final int arity = predicate.getArity();
    final String signature = predicate.getSignature();

    final Method mappedmethod = methodMap.get(signature);
    if (mappedmethod == null) {
      final ProlCriticalError error = new ProlCriticalError("Can't find mapped method for signature \'" + signature + '\'');
      LOG.throwing(this.getClass().getCanonicalName(), "Can't find mapped method", error);
      throw error;
    }

    final Class<?>[] argClasses = mappedmethod.getParameterTypes();

    final Object[] args = new Object[arity];
    for (int li = 0; li < arity; li++) {
      args[li] = term2obj(context, argClasses[li], predicate.getElement(li));
    }

    Object result = Boolean.TRUE;

    try {
      result = mappedmethod.invoke(wrappedObject, args);
    }
    catch (Exception ex) {
      LOG.log(Level.SEVERE, "Exception during method invoke [" + mappedmethod.toGenericString() + "]", ex);
      if (mappedmethod.getReturnType().isAssignableFrom(Number.class)) {
        // it's an evaluable term, we must throw error
        throw new ProlCriticalError("Exception during a wrapped method was thrown", ex);
      }
      else {
        // we return false if it is not evaluable term
        result = Boolean.FALSE;
      }
    }

    return result;
  }

  /**
   * This method handles all wrapped evaluable predicates, these predicates have
   * to return a result as a Number
   *
   * @param goal the goal to be solved, must not be null
   * @param predicate the handled predicate, must not be null
   * @return a NumericTerm fromed from the returned value of the mapped method,
   * must not be null
   * @throws NullPointerException it will be thrown if null was returrned by
   * handled method
   */
  @Evaluable
  @Determined
  protected Term proxyEvaluablePredicate(final Goal goal, final TermStruct predicate) {
    final Number returned = (Number) handlePredicate(goal, predicate);

    if (returned == null) {
      final NullPointerException ex = new NullPointerException("A predicate, signed as an evaluable one, has returned null [" + predicate.toString() + ']');
      LOG.log(Level.SEVERE, "Detected null as result", ex);
      throw ex;
    }

    return goal.getContext().objectAsTerm(returned);
  }

  /**
   * Handle a nonevaluable predicate
   *
   * @param goal the goal to be solved, must not be null
   * @param predicate the handled predicate, must not be null
   * @return true if all ok, false if the predicate failed
   */
  @Determined
  protected boolean proxyPredicate(final Goal goal, final TermStruct predicate) {
    final Object obj = handlePredicate(goal, predicate);

    boolean result = true;

    if (obj instanceof Boolean) {
      result = ((Boolean) obj).booleanValue();
    }

    return result;
  }

  /**
   * An inside function to check validity of a formed name
   *
   * @param name the name to be checked,must not be null
   * @return true if the name is ok, else false
   * @throws NullPointerException will be thrown if the nargument is null
   */
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
    if (firstChar == '_') {
      return false;
    }

    return true;
  }

  /**
   * To form a predicate name from the method name
   *
   * @param method the method which name will be used to form the predicate
   * name, must not be null
   * @return the formed name as String
   * @throws IllegalArgumentException will be thrown if it is impossible to
   * generate a valid name from the method name
   */
  private static String generatePredicateNameFromMethodName(final Method method) {
    String name = method.getName().toLowerCase();
    while (true) {
      if (name.charAt(0) == '_') {
        name = name.substring(1);
      }
      else {
        break;
      }
    }
    if (name.length() == 0) {
      throw new IllegalArgumentException("Can't make valid predicate name from the method name \'" + method.getName() + '\'');
    }

    return name;
  }

  /**
   * Inside aux function to set an Object into a cell of an array, the array can
   * have a primitive type
   *
   * @param array the target array, must not be null
   * @param index the cell index at the array (0 or more)
   * @param element the element which should be setted to the cell
   * @throws ProlCriticalError will be thrown if the array type is not supported
   */
  private static void setObjectToArrayElement(final Object array, final int index, final Object element) {
    final Class<?> componentclass = array.getClass().getComponentType();
    if (componentclass.isPrimitive()) {
      if (componentclass == int.class) {
        final int[] arr = (int[]) array;
        arr[index] = (Integer) element;
      }
      else if (componentclass == float.class) {
        final float[] arr = (float[]) array;
        arr[index] = (Float) element;
      }
      else if (componentclass == byte.class) {
        final byte[] arr = (byte[]) array;
        arr[index] = (Byte) element;
      }
      else if (componentclass == char.class) {
        final char[] arr = (char[]) array;
        arr[index] = (Character) element;
      }
      else if (componentclass == short.class) {
        final short[] arr = (short[]) array;
        arr[index] = (Short) element;
      }
      else if (componentclass == double.class) {
        final double[] arr = (double[]) array;
        arr[index] = (Double) element;
      }
      else if (componentclass == boolean.class) {
        final boolean[] arr = (boolean[]) array;
        arr[index] = (Boolean) element;
      }
      else {
        throw new ProlCriticalError("Unsupported primitive type [" + componentclass.getCanonicalName() + ']');
      }
    }
    else {
      ((Object[]) array)[index] = element;
    }
  }

  /**
   * Generate new empty array for class (which can be a primitive type) for
   * length
   *
   * @param type the class describes the type of new array
   * @param length the length of new array
   * @return new generated array as Object
   * @throws ProlCriticalError if the type argument is not supported
   */
  private static Object newArray(final Class<?> type, final int length) {
    Object result = null;
    if (type.isPrimitive()) {
      if (type == int.class) {
        result = new int[length];
      }
      else if (type == float.class) {
        result = new float[length];
      }
      else if (type == long.class) {
        result = new long[length];
      }
      else if (type == double.class) {
        result = new double[length];
      }
      else if (type == byte.class) {
        result = new byte[length];
      }
      else if (type == short.class) {
        result = new short[length];
      }
      else if (type == char.class) {
        result = new char[length];
      }
      else if (type == boolean.class) {
        result = new boolean[length];
      }
      else {
        throw new ProlCriticalError("Unsupported primitive type [" + type.getCanonicalName() + ']');
      }
    }
    else {
      result = Array.newInstance(type, length);
    }
    return result;
  }

  /**
   * Complex inside function to automate conversion of a term into compatible
   * representation which can be used as an object of a class type
   *
   * @param context the prol context which will be used for convertation, must
   * not be null
   * @param argclass the class of the result, must not be null
   * @param term the term to be converted, must not be null
   * @return an Object which is compatible with the argclass
   * @throws ProlCriticalError will be thrown if there is an inside error during
   * operation
   */
  @SuppressWarnings("unchecked")
  private static Object term2obj(final ProlContext context, final Class<?> argclass, final Term term) {
    Object result = null;
    if (argclass.isArray()) {
      final Class<?> arrayclass = argclass.getComponentType();
      switch (term.getTermType()) {
        case Term.TYPE_LIST: {
          final TermList list = (TermList) term;
          if (list.isNullList()) {
            result = newArray(arrayclass, 0);
          }
          else {
            final int len = list.calculateLength();

            final Object resultarr = newArray(arrayclass, len);
            TermList lst = list;
            int index = 0;
            while (true) {
              final Term head = lst.getHead();
              setObjectToArrayElement(resultarr, index, term2obj(context, arrayclass, head));
              index++;
              final Term tail = lst.getTail();
              if (tail.getTermType() == Term.TYPE_LIST) {
                if (((TermList) tail).isNullList()) {
                  break;
                }
                else {
                  lst = (TermList) tail;
                }
              }
              else {
                setObjectToArrayElement(resultarr, index, term2obj(context, arrayclass, tail));
                index++;
                break;
              }
            }
            if (len != index) {
              throw new ProlCriticalError("Wrong converted array length detected [" + len + "!=" + index + ']');
            }
            result = resultarr;
          }
        }
        break;
        case Term.TYPE_STRUCT: {
          final TermStruct struct = (TermStruct) term;
          final int arity = struct.getArity();
          final Object resultarr = newArray(arrayclass, arity + 1);
          setObjectToArrayElement(resultarr, 0, term2obj(context, arrayclass, struct.getFunctor()));

          for (int li = 0; li < arity; li++) {
            setObjectToArrayElement(resultarr, li + 1, term2obj(context, arrayclass, struct.getElement(li)));
          }
          result = resultarr;
        }
        break;
        default: {
          final Object resultarr = newArray(arrayclass, 1);
          setObjectToArrayElement(resultarr, 0, term2obj(context, arrayclass, term));
          result = resultarr;
        }
        break;
      }
    }
    else {
      if (argclass.isPrimitive()) {
        if (argclass == int.class) {
          if (term instanceof TermInteger) {
            result = ((TermInteger) term).getNumericValue();
          }
        }
        else if (argclass == long.class) {
          if (term instanceof TermInteger) {
            result = Long.valueOf(((TermInteger) term).getNumericValue().intValue());
          }
        }
        else if (argclass == double.class) {
          if (term instanceof TermInteger) {
            result = Double.valueOf(((TermInteger) term).getNumericValue().doubleValue());
          }
          else if (term instanceof TermFloat) {
            result = Double.valueOf(((TermFloat) term).getNumericValue().doubleValue());
          }
        }
        else if (argclass == float.class) {
          if (term instanceof TermInteger) {
            result = Float.valueOf(((TermInteger) term).getNumericValue().floatValue());
          }
          else if (term instanceof TermFloat) {
            result = ((TermFloat) term).getNumericValue();
          }
        }
        else if (argclass == boolean.class) {
          if (term instanceof NumericTerm) {
            result = ((NumericTerm) term).getNumericValue().intValue() == 0 ? false : true;
          }
          else {
            if ("true".equalsIgnoreCase(term.getText())) {
              result = true;
            }
            else {
              result = false;
            }
          }
        }
        else if (argclass == byte.class) {
          if (term instanceof NumericTerm) {
            result = Byte.valueOf(((NumericTerm) term).getNumericValue().byteValue());
          }
        }
        else if (argclass == char.class) {
          if (term instanceof NumericTerm) {
            result = new Character((char) ((NumericTerm) term).getNumericValue().shortValue());
          }
          else if (term.getTermType() == Term.TYPE_ATOM) {
            final String text = term.getText();
            if (text.length() == 1) {
              result = text.charAt(0);
            }
          }
        }
        else if (argclass == short.class) {
          if (term instanceof NumericTerm) {
            result = Short.valueOf(((NumericTerm) term).getNumericValue().shortValue());
          }
        }
        else {
          throw new ProlCriticalError("Unsupported primitive type [" + argclass.getCanonicalName() + ']');
        }
      }
      else {
        if (argclass == Object.class) {
          result = context.termAsObject(term);
        }
        else if (argclass == String.class) {
          result = term.getText();
        }
        else if (argclass == List.class) {
          result = Arrays.asList((Object[]) term2obj(context, (new Object[0]).getClass(), term));
        }
        else if (argclass == Set.class) {
          final Object[] asarray = (Object[]) term2obj(context, (new Object[0]).getClass(), term);
          final Set<Object> resultset = new HashSet<Object>();
          resultset.addAll(Arrays.asList(asarray));
          result = resultset;

        }
        else {
          throw new ProlCriticalError("Unsupported type [" + argclass.getCanonicalName() + ']');
        }

      }
    }
    if (result == null) {
      throw new ProlCriticalError("Can't convert \'" + term.toString() + "\' to \'" + argclass.getCanonicalName() + "\' compatible representation");
    }

    return result;
  }
}
