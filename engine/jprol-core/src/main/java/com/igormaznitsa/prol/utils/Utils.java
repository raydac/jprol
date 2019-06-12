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

package com.igormaznitsa.prol.utils;

import com.igormaznitsa.prol.annotations.*;
import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prol.libraries.AbstractProlLibrary;
import com.igormaznitsa.prol.logic.ProlContext;

import java.io.*;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Comparator;

import static com.igormaznitsa.prol.data.TermType.ATOM;

public final class Utils {

  public static final Comparator<Term> TERM_COMPARATOR = Term::compareTermTo;

  public static final Operator SIGNATURE_OPERATOR = new Operator(400, Operator.OPTYPE_YFX, "/");

  // hide the constructor
  private Utils() {
  }

  public static void doSilently(final RunnableWithException runnable) {
    try {
      runnable.run();
    } catch (Throwable e) {
    }
  }

  public static void writeAsUtf8(final File file, final CharSequence seq) throws IOException {
    try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file, false), StandardCharsets.UTF_8))) {
      writer.write(seq.toString());
      writer.flush();
    }
  }

  public static String readAsUtf8(final File file) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
      final StringBuilder buffer = new StringBuilder((int) file.length() < 0 ? 16384 : (int) file.length());
      while (!Thread.currentThread().isInterrupted()) {
        final int chr = reader.read();
        if (chr < 0) {
          break;
        }
        buffer.append((char) chr);
      }
      return buffer.toString();
    }
  }

  public static TermList createOrAppendToList(final TermList nullableList, final Term term) {
    final TermList newList = Terms.newList(term);

    if (nullableList != null && !nullableList.isNullList()) {
      newList.setTail(nullableList.getTail());
      nullableList.setTail(newList);
    }

    return newList;
  }

  public static Term getListAsAtom(final ProlContext context, final TermList list) {
    if (list == TermList.NULLLIST) {
      return new Term("<empty>");
    }
    if (list.getTail() == TermList.NULLLIST) {
      return list.getHead();
    } else {
      final int length = list.calculateLength();
      if (length == 3) {
        // may be it is a list
        if (list.getHead().getText().equals(".")) {
          final TermList secondElement = list.getTail();
          final TermList thirdElement = secondElement.getTail();

          if (thirdElement.getHead().getTermType() == LIST) {
            // it's a list
            return new TermList(secondElement.getHead(), thirdElement.getHead());
          }
        }
      }

      final TermStruct result;
      if (length == 1) {
        result = new TermStruct(list.getHead());
      } else {
        Term[] elements = new Term[length - 1];
        TermList lst = list.getTail();
        int index = 0;
        while (lst != TermList.NULLLIST) {
          elements[index++] = lst.getHead();
          lst = lst.getTail();
        }
        result = new TermStruct(list.getHead(), elements);
      }

      result.setPredicateProcessor(context.findProcessor(result));

      return result;
    }
  }

  public static TermList arrayToList(final Term... array) {
    if (array == null || array.length == 0) {
      return TermList.NULLLIST;
    }

    final TermList result = new TermList(array[0]);
    TermList next = result;
    final int length = array.length;
    for (int li = 1; li < length; li++) {
      next = createOrAppendToList(next, array[li]);
    }

    return result;
  }

  public static Term[] listToArray(final TermList list) {
    if (list.isNullList()) {
      return new Term[0];
    }

    final ArrayList<Term> arraylist = new ArrayList<>();
    TermList curlist = list;
    while (!Thread.currentThread().isInterrupted()) {
      if (curlist.isNullList()) {
        break;
      }
      arraylist.add(curlist.getHead());
      final Term nextList = curlist.getTail();
      if (nextList.getTermType() == LIST) {
        curlist = (TermList) nextList;
      } else {
        arraylist.add(nextList);
        break;
      }
    }

    return arraylist.toArray(new Term[0]);
  }

  public static TermList unrollTermIntoList(final Term element) {
    switch (element.getTermType()) {
      case LIST:
        return (TermList) element;
      case STRUCT: {
        final TermStruct struct = (TermStruct) element;
        final TermList result = new TermList(struct.getFunctor());
        TermList curResult = result;
        final int arity = struct.getArity();
        for (int li = 0; li < arity; li++) {
          curResult = createOrAppendToList(curResult, struct.getElement(li));
        }
        return result;
      }
      default:
        return new TermList(element);
    }
  }

  private static void processTermForArrangeVariables(final Term term, final Map<String, Var> variables) {
    switch (term.getTermType()) {
      case LIST: {
        TermList list = (TermList) term;
        while (!list.isNullList()) {
          processTermForArrangeVariables(list.getHead(), variables);
          final Term tail = list.getTail();
          if (tail.getTermType() == LIST) {
            list = (TermList) tail;
          } else {
            processTermForArrangeVariables(tail, variables);
            break;
          }
        }
      }
      break;
      case STRUCT: {
        final TermStruct struct = (TermStruct) term;
        final int arity = struct.getArity();

        for (int li = 0; li < arity; li++) {
          final Term element = struct.getElement(li);
          if (element.getTermType() == VAR) {
            final Var thatVar = (Var) element;
            final String varname = thatVar.getText();
            if (!thatVar.isAnonymous() && thatVar.isFree()) {
              final Var var = variables.get(varname);
              if (var == null) {
                variables.put(varname, (Var) element);
              } else {
                struct.setElement(li, var);
              }
            }
          } else {
            processTermForArrangeVariables(element, variables);
          }
        }
      }
      break;
      case VAR: {
        final String name = term.getText();
        if (variables.containsKey(name)) {
          final Var var = variables.get(name);
          term.unifyTo(var);
        } else {
          variables.put(name, (Var) term);
        }
      }
      break;
    }
  }

  public static void arrangeVariablesInsideTerms(final Term termOne, final Term termTwo) {
    final Map<String, Var> varMap = new HashMap<>();
    processTermForArrangeVariables(termOne, varMap);
    processTermForArrangeVariables(termTwo, varMap);
  }

  public static void printPredicatesForLibrary(final PrintStream out, final Class<?> libraryClass) {
    if (!AbstractProlLibrary.class.isAssignableFrom(libraryClass)) {
      out.println(libraryClass.getCanonicalName() + " is not an AbstractLibrary class");
      return;
    }

    final Method[] methods = libraryClass.getMethods();
    out.println(libraryClass.getCanonicalName());
    out.println("===============================================");

    final ProlOperators operators = libraryClass.getAnnotation(ProlOperators.class);
    if (operators != null) {
      // there is defined operators
      final ProlOperator[] ops = operators.Operators();
      if (ops.length > 0) {
        out.println("Operators\n-----------------------");
        for (final ProlOperator oper : ops) {
          if (oper.Priority() > 0) {
            out.println(":-op(" + oper.Priority() + "," + Operator.getTypeFromIndex(oper.Type()) + ",\'" + oper.Name() + "\').");
          }
        }
        out.println("-----------------------");
      }
    }

    for (final Method method : methods) {
      final Predicate predicate = method.getAnnotation(Predicate.class);
      if (predicate != null) {
        final boolean determined = method.getAnnotation(Determined.class) != null;
        final PredicateSynonyms predicateSynonims = method.getAnnotation(PredicateSynonyms.class);
        out.print(predicate.Signature());
        if (predicateSynonims != null) {
          out.print(" {");
          final String[] signatures = predicateSynonims.Signatures();
          for (int ls = 0; ls < signatures.length; ls++) {
            if (ls > 0) {
              out.print(", ");
            }
            out.print(signatures[ls]);
          }
          out.print("}");
        }
        if (determined) {
          out.print(" [DETERMINED]");
        }
        out.println();

        final String[] templates = predicate.Template();
        for (String template : templates) {
          out.println('[' + template + ']');
        }

        final String reference = predicate.Reference();
        if (reference != null && reference.length() > 0) {
          out.println();
          out.println(reference);
        }

        out.println("---------------------\r\n");
      }
    }
  }

  public static void spaces(final PrintStream out, final int number) {
    for (int li = 0; li < number; li++) {
      out.print(' ');
    }
  }

  public static void printTree(final PrintStream out, final int spaces, final Term term) {
    if (term.getTermType() == STRUCT) {
      final TermStruct struct = (TermStruct) term;
      if (struct.getFunctor().getTermType() == OPERATOR) {
        out.println('(' + struct.getFunctor().toString() + ')');
        final int spaces2 = spaces + 1;
        spaces(out, spaces2);
        out.println('|');
        for (int li = 0; li < struct.getArity(); li++) {
          spaces(out, spaces2);
          out.print("\\-");
          printTree(out, spaces2 + 2, struct.getElement(li));
        }
      } else {
        out.println(term.toString());
      }
    } else {
      out.println(term);
    }
  }

  public static String escapeSrc(final String string) {

    if (string.length() == 0) {
      return string;
    }

    final StringBuilder builder = new StringBuilder(string.length());

    for (int li = 0; li < string.length(); li++) {
      final char curChar = string.charAt(li);

      switch (curChar) {
        case '\\':
          builder.append("\\\\");
          break;
        case '\'':
          builder.append("\\\'");
          break;
        case '\"':
          builder.append("\\\"");
          break;
        case '\n':
          builder.append("\\n");
          break;
        case '\f':
          builder.append("\\f");
          break;
        case '\r':
          builder.append("\\r");
          break;
        case '\t':
          builder.append("\\t");
          break;
        case '_': {
          builder.append('_');
        }
        break;
        case '%':
        case '.':
        default: {
          builder.append(curChar);
        }
      }
    }

    return builder.toString();
  }

  public static String extractPredicateSignatureFromStructure(final Term term) {
    final TermStruct struct = term.findNonVarOrSame();

    if (struct.getArity() != 2) {
      return null;
    }
    final Term left = struct.getElement(0).findNonVarOrDefault(null);
    final Term right = struct.getElement(1).findNonVarOrDefault(null);

    if (right instanceof TermInteger && left.getTermType() == ATOM) {
      return left.getText() + '/' + right.getText();
    }
    return null;
  }

  public static String normalizeSignature(final String signature) {
    if (signature == null) {
      return null;
    }
    String sig = signature.trim();

    if (sig.length() > 0 && sig.charAt(0) == '\'') {
      sig = sig.substring(1);
      final int lastIndex = sig.lastIndexOf('/');
      if (lastIndex < 0) {
        sig = null;
      } else {
        final String arity = sig.substring(lastIndex + 1).trim();
        String name = sig.substring(0, lastIndex - 1).trim();
        if (name.length() > 0 && name.charAt(name.length() - 1) == '\'') {
          name = name.substring(0, name.length() - 1);
          sig = name + '/' + arity;
        } else {
          sig = null;
        }
      }
    }
    return sig;
  }

  public static String validateSignature(final String signature) {
    if (signature == null) {
      throw new NullPointerException("Null signature detected");
    }
    final String[] parsed = signature.split("/");
    if (parsed.length == 2) {
      String str = parsed[0].trim();
      boolean quoted = false;
      if (str.length() != 0) {
        if (str.charAt(0) == '\'') {
          if (str.length() > 1 && str.charAt(str.length() - 1) == '\'') {
            str = str.substring(1, str.length() - 1);
            if (str.length() == 0) {
              return null;
            }
            quoted = true;
          } else {
            // wrong name, it must not contain '\'' as the only symbol
            return null;
          }
        }

        final char firstChar = str.charAt(0);
        if (!quoted && (Character.isDigit(firstChar) || Character.isUpperCase(firstChar) || Character.isWhitespace(firstChar) || firstChar == '.')) {
          return null;
        }

        // ok. the first part is ok, check the second part
        final int arity;
        try {
          arity = Integer.parseInt(parsed[1].trim());
          if (arity < 0) {
            throw new NumberFormatException("Negate number is not supported as arity");
          }
        } catch (NumberFormatException ex) {
          return null;
        }

        final StringBuilder builder = new StringBuilder(signature.length());

        if (quoted) {
          builder.append('\'').append(str).append('\'').append('/').append(arity);
        } else {
          builder.append(str).append('/').append(arity);
        }

        return builder.toString();
      }
    }
    return null;
  }

  public static Term obj2term(final Object object) {
    Term result = null;

    if (object == null) {
      // return NULLLIST
      result = TermList.NULLLIST;
    } else if (object instanceof Term) {
      result = (Term) object;
    } else if (object instanceof ConvertableToTerm) {
      final ConvertableToTerm cterm = (ConvertableToTerm) object;
      result = cterm.asProlTerm();
      if (result == null) {
        throw new NullPointerException("asProlTerm() returned null [" + object.toString() + ']');
      }
    } else if (object instanceof String) {
      // atom or mapped object
      result = new Term((String) object);
    } else if (object instanceof Number) {
      if (object instanceof Integer) {
        result = new TermInteger(((Integer) object));
      } else if (object instanceof Float) {
        result = new TermFloat(((Float) object));
      } else {
        throw new IllegalArgumentException("Unsupported number format.");
      }
    } else if (object instanceof Collection) {
      // list
      final Collection<?> lst = (Collection) object;

      if (lst.isEmpty()) {
        result = TermList.NULLLIST;
      } else {
        TermList accumulator = null;
        // fill the list
        for (Object item : lst) {
          if (accumulator == null) {
            accumulator = new TermList(obj2term(item));
            result = accumulator; // the first list
          } else {
            accumulator = createOrAppendToList(accumulator, obj2term(item));
          }
        }
      }
    } else if (object instanceof Object[]) {
      // struct
      final Object[] array = (Object[]) object;
      final int arrlen = array.length;
      if (arrlen == 0) {
        // as null list
        result = TermList.NULLLIST;
      } else {
        final Term functor = new Term(array[0].toString());
        if (arrlen == 1) {
          result = new TermStruct(functor);
        } else {
          final Term[] terms = new Term[arrlen - 1];
          for (int li = 1; li < arrlen; li++) {
            terms[li - 1] = obj2term(array[li]);
          }
          result = new TermStruct(functor, terms);
        }
      }
    } else {
      throw new IllegalArgumentException("Unsupported object to be represented as a Term");
    }
    return result;
  }

  @FunctionalInterface
  public interface RunnableWithException {

    void run() throws Throwable;
  }
}
