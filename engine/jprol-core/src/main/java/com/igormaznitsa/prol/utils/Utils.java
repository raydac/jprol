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
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.prol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.prol.libraries.AbstractProlLibrary;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLConnection;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public final class Utils {

    @FunctionalInterface
    public interface RunnableWithException {

        void run() throws Throwable;
    }

    /**
     * The constant contains a comparator which can compare two terms.
     */
    public static final Comparator<Term> TERM_COMPARATOR = Term::termComparsion;
    /**
     * Inside logger, the canonical class name is used as the logger identifier
     * (Utils.class.getCanonicalName())
     */
    protected static final Logger LOG = Logger.getLogger(Utils.class.getCanonicalName());

    // hide the constructor
    private Utils() {
    }

    public static void doSilently(final RunnableWithException runnable) {
        try {
            runnable.run();
        } catch (Throwable e) {
        }
    }

    public static boolean isPrologVariable(final String name) {
        boolean result = false;
        if (name.length()>0) {
            final char firstChar = name.charAt(0);
            result = firstChar == '_' || (Character.isAlphabetic(firstChar) && Character.isUpperCase(firstChar));
        }
        return result;
    }
    
    public static void writeFileAsUTF8Str(final File file, final CharSequence seq) throws IOException {
        try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file, false), StandardCharsets.UTF_8))) {
            writer.write(seq.toString());
            writer.flush();
        }
    }

    public static String readFileAsUTF8Str(final File file) throws IOException {
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

    /**
     * Get a hash-map filled by variables found in a term, the variables can be
     * accessed by their string names.
     * <b>NB! Variables are mutable object and the operation doesn't make any
     * copy of them, their state will be changed during any next JProl
     * operation, thus consider the map as temporary data storage which values
     * you should process as soon as possible!</b>
     *
     * @param term the term which variables should be returned, must not be null
     * @return a HashMap object contains pairs name-variables extracted from the
     * term
     * @see #fillTableWithVarValues(com.igormaznitsa.prol.data.Term)
     */
    public static Map<String, Var> fillTableWithVars(final Term term) {
        final Map<String, Var> vars = new HashMap<>();
        term.fillVarables(vars);
        return vars;
    }

    /**
     * Extract content of all instantiated non-anonymous variables from the
     * provided term.
     * <b>NB! Both Non-instantiated and anonymous variables will be ignored in
     * the result.</b>
     *
     * @param term term to be processes, must not be null
     * @param map a map to be filled by pairs, it can be null and in the case it
     * will be created
     * @return a name-value map for all instantiated variables detected in the
     * provided term
     */
    public static Map<String, Term> fillTableWithFoundVarContent(final Term term, final Map<String, Term> map) {
        final Map<String, Var> vars = fillTableWithVars(term);
        final Map<String, Term> result;
        if (map == null) {
            result = new HashMap<>();
        } else {
            map.clear();
            result = map;
        }

        vars.entrySet().forEach((e) -> {
            final String name = e.getKey();
            final Var value = e.getValue();
            if (!(value.isAnonymous() || value.isUndefined())) {
                result.put(name, value.getValue());
            }
        });
        return result;
    }

    /**
     * Return a Number object from a Term if it is possible.
     *
     * @param term the term which should be converted into a number
     * @return a Number object if it is possible
     * @throws com.igormaznitsa.prol.exceptions.ProlTypeErrorException it will
     * be thrown if the term is not a numeric term
     */
    public static Number getNumberFromElement(Term term) {
        if (term.getTermType() == Term.TYPE_VAR) {
            final Term origTerm = term;
            term = ((Var) term).getValue();
            if (term == null) {
                throw new ProlInstantiationErrorException("NonInstantiated variable", origTerm);
            }
        }

        if (term instanceof NumericTerm) {
            return ((NumericTerm) term).getNumericValue();
        } else {
            throw new ProlTypeErrorException("numeric", "NonNumeric term", term);
        }
    }

    /**
     * Convert the term into a String.If the term is not instantiated the
     * ProlCriticalError will be thrown.
     *
     * @param term the term to be converted into String.
     * @return the term text representation as a String (the same value which
     * will be written by write/1 for the term)
     */
    public static String getStringFromElement(Term term) {
        if (term.getTermType() == Term.TYPE_VAR) {
            final Term origTerm = term;
            term = ((Var) term).getValue();
            if (term == null) {
                throw new ProlInstantiationErrorException("NonInstantiated variable", origTerm);
            }
        }

        return term.forWrite();
    }

    /**
     * Find the root throwable object in an excepotion
     *
     * @param ex a trhowable object to find the root, must not be null
     * @return a throwable object which is the root for the cause chain of the
     * exception or the same exception if there is not any cause
     */
    public static Throwable getRootThrowable(Throwable ex) {
        while (!Thread.currentThread().isInterrupted()) {
            if (ex.getCause() == null) {
                break;
            }
            ex = ex.getCause();
        }

        return ex;
    }

    /**
     * If the term is a instantiated variable thane the function will return the
     * variable value else the same term
     *
     * @param element the term which shold be processed, must not be null
     * @return the value of a variable if the term is an instantiated variable
     * else the same term
     */
    public static Term getTermFromElement(final Term element) {
        if (element.getTermType() == Term.TYPE_VAR) {
            final Term val = ((Var) element).getValue();
            if (val == null) {
                return element;
            } else {
                return val;
            }
        } else {
            return element;
        }
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
                    final TermList secondElement = (TermList) list.getTail();
                    final TermList thirdElement = (TermList) secondElement.getTail();

                    if (thirdElement.getHead().getTermType() == Term.TYPE_LIST) {
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
                TermList lst = (TermList) list.getTail();
                int index = 0;
                while (lst != TermList.NULLLIST) {
                    elements[index++] = lst.getHead();
                    lst = (TermList) lst.getTail();
                }
                result = new TermStruct(list.getHead(), elements);
            }

            result.setPredicateProcessor(context.findProcessor(result));

            return result;
        }
    }

    public static TermList arrayToList(final Term[] array) {
        if (array == null || array.length == 0) {
            return TermList.NULLLIST;
        }

        final TermList result = new TermList(array[0]);
        TermList next = result;
        final int length = array.length;
        for (int li = 1; li < length; li++) {
            next = TermList.appendItem(next, array[li]);
        }

        return result;
    }

    /**
     * Convert a term list into a term array
     *
     * @param list the list to be converted into an array, must not be null
     * @return a term array contains all elements from the list, if the list is
     * a NULLLIST then zero length array will be returned
     */
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
            if (nextList.getTermType() == Term.TYPE_LIST) {
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
            case Term.TYPE_LIST:
                return (TermList) element;
            case Term.TYPE_STRUCT: {
                final TermStruct struct = (TermStruct) element;
                final TermList result = new TermList(struct.getFunctor());
                TermList curResult = result;
                final int arity = struct.getArity();
                for (int li = 0; li < arity; li++) {
                    curResult = TermList.appendItem(curResult, struct.getElement(li));
                }
                return result;
            }
            default:
                return new TermList(element);
        }
    }

    private static void processTermForArrangeVariables(final Term term, final Map<String, Var> variables) {
        switch (term.getTermType()) {
            case Term.TYPE_LIST: {
                TermList list = (TermList) term;
                while (!list.isNullList()) {
                    processTermForArrangeVariables(list.getHead(), variables);
                    final Term tail = list.getTail();
                    if (tail.getTermType() == Term.TYPE_LIST) {
                        list = (TermList) tail;
                    } else {
                        processTermForArrangeVariables(tail, variables);
                        break;
                    }
                }
            }
            break;
            case Term.TYPE_STRUCT: {
                final TermStruct struct = (TermStruct) term;
                final int arity = struct.getArity();

                for (int li = 0; li < arity; li++) {
                    final Term element = struct.getElement(li);
                    if (element.getTermType() == Term.TYPE_VAR) {
                        final String varname = element.getText();
                        if (((Var) element).isUndefined()) {
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
            case Term.TYPE_VAR: {
                final String name = term.getText();
                if (variables.containsKey(name)) {
                    final Var var = variables.get(name);
                    term.Equ(var);
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
        switch (term.getTermType()) {
            case Term.TYPE_STRUCT: {
                final TermStruct struct = (TermStruct) term;
                if (struct.getFunctor().getTermType() == Term.TYPE_OPERATOR) {
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
            }
            break;
            default: {
                out.println(term);
            }
        }
    }

    public static void printTermState(final PrintStream out, final Term term) {
        out.println(term.getSourceLikeRepresentation());

        final Map<String, Var> vars = Utils.fillTableWithVars(term);

        for (Var variable : vars.values()) {
            if (variable.isAnonymous()) {
                continue;
            }
            out.print(variable.getText());
            out.print("{uid=" + variable.getVarUID() + '}');
            out.print('=');
            if (variable.isUndefined()) {
                out.println("???");
            } else {
                out.println(variable.toString());
            }
        }
    }

    public static String encodeTextSourceLike(final String string) {

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

    public static void consultFromURLConnection(final String url, final ProlContext context) throws IOException {
        if (url == null || context == null) {
            throw new IllegalArgumentException("There is a null as an argument");
        }

        final InputStream inStream;
        if (url.startsWith("this://")) {
            final String purePath = url.substring(7); // remove the prefix
            inStream = Thread.currentThread().getContextClassLoader().getResourceAsStream(purePath);
            if (inStream == null) {
                throw new IOException("Can't find the resource addressed as \"" + purePath + "\" through the class loader");
            }
        } else {
            final URL parsed = new URL(url);
            final URLConnection connection = parsed.openConnection();
            connection.setDoInput(true);
            connection.setDoOutput(false);
            connection.setAllowUserInteraction(false);
            connection.setUseCaches(true);
            inStream = connection.getInputStream();
        }

        try {
            new ProlConsult(inStream, context).consult();
        } finally {
            if (inStream != null) {
                try {
                    inStream.close();
                } catch (Exception thr) {
                    LOG.log(Level.WARNING, "consultFromURLConnection().close", thr);
                }
            }
        }
    }

    public static String extractPredicateSignatureFromStructure(final Term term) {
        final TermStruct struct = (TermStruct) Utils.getTermFromElement(term);

        if (struct.getArity() != 2) {
            return null;
        }
        final Term left = getTermFromElement(struct.getElement(0));
        final Term right = getTermFromElement(struct.getElement(1));

        if (right instanceof TermInteger && left.getTermType() == Term.TYPE_ATOM) {
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

    public static Var findVarInsideTerm(final Term term, final String name) {
        if (term == null || name == null) {
            throw new NullPointerException();
        }

        Var result = null;
        switch (term.getTermType()) {
            case Term.TYPE_STRUCT: {
                final TermStruct struct = (TermStruct) term;
                final Term[] elements = struct.getElementsAsArray();
                for (Term element : elements) {
                    result = findVarInsideTerm(element, name);
                    if (result != null) {
                        break;
                    }
                }
            }
            break;
            case Term.TYPE_LIST: {
                final TermList list = (TermList) term;
                if (!list.isNullList()) {
                    final Term headterm = list.getHead();
                    if (headterm != null) {
                        result = findVarInsideTerm(headterm, name);
                    }
                    if (result == null) {
                        final Term tailterm = list.getTail();
                        if (tailterm != null) {
                            result = findVarInsideTerm(tailterm, name);
                        }
                    }
                }
            }
            break;
            case Term.TYPE_VAR: {
                final Var varTerm = (Var) term;
                if (varTerm.getText().equals(name)) {
                    result = varTerm;
                } else {
                    final Term value = varTerm.getThisValue();
                    if (value != null) {
                        result = findVarInsideTerm(value, name);
                    }
                }

            }
            break;
            default: {
            }
            break;
        }
        return result;
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
    
    public static Object term2obj(final ProlContext context, final Term term) {
        final Term cterm = Utils.getTermFromElement(term);
        Object result = null;
        switch (cterm.getTermType()) {
            case Term.TYPE_ATOM: {
                if (cterm instanceof NumericTerm) {
                    // as numeric value
                    result = ((TermInteger) cterm).getNumericValue();
                } else {
                    // find mapped object
                    final String termtext = cterm.getText();
                    result = context.findMappedObjectForName(termtext);
                    if (result == null) {
                        // there is not any mapped object, so return the text
                        result = termtext;
                    }
                }
            }
            break;
            case Term.TYPE_LIST: {
                // make List<Object>
                final List<Object> list = new ArrayList<>();
                TermList tlist = (TermList) cterm;
                while (tlist.isNullList()) {
                    list.add(term2obj(context, tlist.getHead()));
                    final Term tail = tlist.getTail();
                    if (tail.getTermType() == Term.TYPE_LIST) {
                        tlist = (TermList) tail;
                    } else {
                        list.add(term2obj(context, tail));
                        break;
                    }
                }
                result = list;
            }
            break;
            case Term.TYPE_OPERATORS:
            case Term.TYPE_OPERATOR: {
                // just as text
                result = cterm.getText();
            }
            break;
            case Term.TYPE_STRUCT: {
                // struct
                final TermStruct sterm = (TermStruct) cterm;
                final int size = sterm.getArity() + 1;
                final Object[] array = new Object[size];

                // the first element is the term
                array[0] = term2obj(context, sterm.getFunctor());

                // other elements
                for (int li = 1; li < size; li++) {
                    array[li] = term2obj(context, sterm.getElement(li - 1));
                }

                result = array;
            }
            break;
            case Term.TYPE_VAR: {
                throw new IllegalArgumentException("It is non instantiate variable \'" + cterm.getText() + "\'");
            }
            default: {
                throw new ProlCriticalError("Unsupported term type");
            }
        }
        return result;
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
                        accumulator = TermList.appendItem(accumulator, obj2term(item));
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
}
