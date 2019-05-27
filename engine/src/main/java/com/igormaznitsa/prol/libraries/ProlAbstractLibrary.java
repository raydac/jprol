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

import com.igormaznitsa.prol.annotations.Predicate;
import com.igormaznitsa.prol.annotations.PredicateSynonyms;
import com.igormaznitsa.prol.annotations.ProlOperator;
import com.igormaznitsa.prol.annotations.ProlOperators;
import com.igormaznitsa.prol.containers.OperatorContainer;
import com.igormaznitsa.prol.data.Operator;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.easygui.MainFrame;
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.utils.Utils;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The core class describes a prolog library contains predicates and operators.
 * Any class which contains predicates msut extend this abstract class. Only
 * classes extend this abstract class can be added as libraries in a prolog
 * engine context
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.containers.KnowledgeBase
 * @see com.igormaznitsa.prol.logic.ProlContext
 */
public abstract class ProlAbstractLibrary {

    /**
     * The table contains all operators which have been defined in the library
     */
    protected final Map<String, OperatorContainer> libraryOperators = new HashMap<>();
    /**
     * The variable contains the text representation of library UID
     */
    protected final String libraryUID;
    /**
     * the table contains all predicate which have been defined in the library
     */
    protected final Map<String, PredicateProcessor> predicateMethodsMap = new HashMap<>();

    /**
     * Set for all zero arity predicate names
     */
    protected final Set<String> zeroArityPredicateNames = new HashSet<>();

    /**
     * The constructor
     *
     * @param libraryID the ID for the library, must not be null
     */
    public ProlAbstractLibrary(final String libraryID) {
        if (libraryID == null) {
            throw new IllegalArgumentException("The library ID must not be null!");
        }

        this.libraryUID = libraryID;

        loadStaticOperators();
        scanThisClassForPredicates();
    }

    /**
     * Check that the library contains a predicate for a signature
     *
     * @param signature a predicate signature ("[FUNCTOR]/[ARITY]")
     * @return true if the library contains such predicate, else false
     */
    public boolean hasPredicateForSignature(final String signature) {
        return this.predicateMethodsMap.containsKey(signature);
    }

    /**
     * Find a processor for a predicate
     *
     * @param predicate the predicate to find a predicate processor for it, must
     *                  not be null
     * @return found processor as a PredicateProcessor object else null
     * @see com.igormaznitsa.prol.libraries.PredicateProcessor
     */
    public PredicateProcessor findProcessorForPredicate(final TermStruct predicate) {
        return this.predicateMethodsMap.get(predicate.getSignature());
    }

    public boolean hasZeroArityPredicateForName(final String name) {
        return this.zeroArityPredicateNames.contains(name);
    }

    /**
     * get the ID of the library
     *
     * @return the library ID as String, must not be null
     */
    public String getLibraryUID() {
        return this.libraryUID;
    }

    /**
     * Release the library. Release and clear resources taken by the library.
     */
    public void release() {
        try {
            this.predicateMethodsMap.clear();
            this.libraryOperators.clear();
            this.zeroArityPredicateNames.clear();
        } catch (Exception thr) {
            Logger.getLogger(this.getClass().getCanonicalName()).log(Level.WARNING, "release()", thr);
        }
    }

    protected static void msgError(final String msg) {
      final MainFrame frame = MainFrame.MAIN_FRAME_INSTANCE.get();
      if (frame!=null) {
        frame.getMessageEditor().addErrorText(msg);
      }
    }
    
    protected static void msgInfo(final String msg) {
      final MainFrame frame = MainFrame.MAIN_FRAME_INSTANCE.get();
      if (frame!=null) {
        frame.getMessageEditor().addInfoText(msg);
      }
    }
    
    protected static void msgWarn(final String msg) {
      final MainFrame frame = MainFrame.MAIN_FRAME_INSTANCE.get();
      if (frame!=null) {
        frame.getMessageEditor().addWarningText(msg);
      }
    }
    
    @Override
    public int hashCode() {
        return this.libraryUID.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (obj instanceof ProlAbstractLibrary) {
            final ProlAbstractLibrary other = (ProlAbstractLibrary) obj;
            return this.libraryUID.equals(other.libraryUID);
        }
        return true;
    }

    /**
     * Inside auxulary function to scan a class for annotations and fill inside
     * tables with found contanent
     *
     * @see com.igormaznitsa.prol.annotations.Predicate
     * @see com.igormaznitsa.prol.annotations.PredicateSynonims
     * @see com.igormaznitsa.prol.annotations.Determined
     * @see com.igormaznitsa.prol.annotations.Evaluable
     */
    private void scanThisClassForPredicates() {
        final Class<?> thisClass = this.getClass();
        final Method[] methods = thisClass.getMethods();

        this.predicateMethodsMap.clear();

        for (final Method method : methods) {
            final Predicate predicateAnnotation = method.getAnnotation(Predicate.class);
            final PredicateSynonyms synonims = method.getAnnotation(PredicateSynonyms.class);

            if (predicateAnnotation != null) {
                final String signature = Utils.normalizeSignature(predicateAnnotation.Signature());

                if (signature == null) {
                    throw new ProlCriticalError("Wrong signature of a predicate method " + method.getName() + " at " + libraryUID);
                }

                if (predicateMethodsMap.containsKey(signature)) {
                    throw new ProlCriticalError("Duplicated predicate method " + signature + " at " + libraryUID);
                }

                PredicateTemplate[][] templates = null;
                final String[] templateStrings = predicateAnnotation.Template();
                if (templateStrings != null && templateStrings.length > 0) {
                    templates = new PredicateTemplate[templateStrings.length][];
                    for (int lt = 0; lt < templateStrings.length; lt++) {
                        final String[] str = templateStrings[lt].split(",");
                        PredicateTemplate[] curtemp = new PredicateTemplate[str.length];
                        for (int ld = 0; ld < str.length; ld++) {
                            curtemp[ld] = new PredicateTemplate(str[ld]);
                        }
                        templates[lt] = curtemp;
                    }
                }

                final PredicateProcessor processor = new PredicateProcessor(this, signature, method, templates);
                this.predicateMethodsMap.put(signature, processor);
                if (signature.endsWith("/0")) {
                    this.zeroArityPredicateNames.add(signature.substring(0, signature.lastIndexOf('/')));
                }

                if (synonims != null) {
                    final String[] synonimSignatures = synonims.Signatures();
                    for (String synonimSignature : synonimSignatures) {
                        final String sig = synonimSignature.trim();
                        this.predicateMethodsMap.put(sig, processor);
                        if (sig.endsWith("/0")) {
                            this.zeroArityPredicateNames.add(sig.substring(0, sig.lastIndexOf('/')));
                        }
                    }
                }
            }
        }
    }

    /**
     * Add a static (system) operator into the library table
     *
     * @param operator an operator which will be added into the operator table as
     *                 a system operator, must not be null
     */
    private void addStaticOperator(final ProlOperator operator) {
        Operator newOperator = new Operator(operator.Priority(), operator.Type(), operator.Name());
        OperatorContainer container = libraryOperators.get(operator.Name());
        if (container == null) {
            container = new OperatorContainer(newOperator, true);
            libraryOperators.put(operator.Name(), container);
        } else {
            container.setOperator(newOperator);
        }
    }

    /**
     * Scan class for operator anotations and place them into local tables
     *
     * @see com.igormaznitsa.prol.annotations.ProlOperator
     * @see com.igormaznitsa.prol.annotations.ProlOperators
     */
    @SuppressWarnings("unchecked")
    private void loadStaticOperators() {
        libraryOperators.clear();
        final Class<?> thisClass = this.getClass();

        // find ProlOperators
        final ProlOperators operators = thisClass.getAnnotation(ProlOperators.class);
        if (operators != null) {
            ProlOperator[] operatorList = operators.Operators();
            for (final ProlOperator lst : operatorList) {
                addStaticOperator(lst);
            }
        }

        // find ProlOperator
        final ProlOperator operator = thisClass.getAnnotation(ProlOperator.class);
        if (operator != null) {
            addStaticOperator(operator);
        }
    }

    /**
     * Check that the string is the name of a static (system) operator from the
     * library
     *
     * @param nameToBeChecked the name to be checked
     * @return if there is an operator with such name then true else false
     */
    public boolean isSystemOperator(final String nameToBeChecked) {
        if (nameToBeChecked == null) {
            return false;
        }
        return libraryOperators.containsKey(nameToBeChecked);
    }

    /**
     * Check that the library has a system operator whose name starts with a
     * substring
     *
     * @param startSubstring the substring to be checked, must not be null
     * @return true if there is an operator whose name has start with the
     * substring, else null
     */
    public boolean hasSyatemOperatorStartsWith(final String startSubstring) {
        for (String s : libraryOperators.keySet()) {
            if (s.startsWith(startSubstring)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Find an operator for full name
     *
     * @param operatorName the name of needed operator, must not be null
     * @return found operator if it has been found else null
     */
    public OperatorContainer findSystemOperatorForName(final String operatorName) {
        return libraryOperators.get(operatorName);
    }

    /**
     * The notification that the context (which is the owner for the library
     * instance) is being halted
     *
     * @param context the prol context must not be null
     */
    public void contextHasBeenHalted(final ProlContext context) {
    }
}
