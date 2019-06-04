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
import com.igormaznitsa.prol.exceptions.ProlCriticalError;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.utils.Utils;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public abstract class AbstractProlLibrary {
    private final String libraryUid;
    private final Map<String, OperatorContainer> libraryOperators;
    private final Map<String, PredicateProcessor> predicateMethodsMap;
    private final Set<String> zeroArityPredicateNames;
    private final IoActionProvider ioActionProvider;
    
    public AbstractProlLibrary(final String libraryUid, final IoActionProvider actionProvider) {
        if (libraryUid == null) {
            throw new IllegalArgumentException("Library UID must not be null!");
        }

        if (actionProvider == null) {
            throw new IllegalArgumentException("Action provider must not be null!");
        }

        this.ioActionProvider = actionProvider;
        this.libraryUid = libraryUid;

        this.libraryOperators = Collections.unmodifiableMap(loadStaticOperators(this.getClass()));
        final Set<String> zeroArityPredicates = new HashSet<>();
        this.predicateMethodsMap = Collections.unmodifiableMap(extractAnnotatedMethodsAsPredicates(libraryUid, zeroArityPredicates));
        this.zeroArityPredicateNames = Collections.unmodifiableSet(zeroArityPredicates);
    }

    public boolean hasPredicateForSignature(final String signature) {
        return this.predicateMethodsMap.containsKey(signature);
    }

    public PredicateProcessor findProcessorForPredicate(final TermStruct predicate) {
        final String signture = predicate.getSignature();
        PredicateProcessor result = onBeforeFindProcessorForPredicate(signture);
        return result == null ? this.predicateMethodsMap.get(predicate.getSignature()) : result;
    }

    protected PredicateProcessor onBeforeFindProcessorForPredicate(final String signature) {
        return null;
    }
    
    public boolean hasZeroArityPredicate(final String predicateName) {
        return this.onBeforeHasZeroArityPredicate(predicateName) || this.zeroArityPredicateNames.contains(predicateName);
    }

    protected boolean onBeforeHasZeroArityPredicate(final String predicateName) {
        return false;
    }

    public String getLibraryUid() {
        return this.libraryUid;
    }

    public void release() {
    }

    protected void msgError(final String msg) {
        this.ioActionProvider.addErrorText(msg);
    }

    protected void msgInfo(final String msg) {
        this.ioActionProvider.addInfoText(msg);
    }

    protected void msgWarn(final String msg) {
        this.ioActionProvider.addWarnText(msg);
    }

    protected IoActionProvider getIoActionProvider() {
        return this.ioActionProvider;
    }
    
    @Override
    public int hashCode() {
        return this.libraryUid.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        boolean result = false;
        if (obj instanceof AbstractProlLibrary) {
            result = this.libraryUid.equals(((AbstractProlLibrary) obj).libraryUid);
        }
        return result;
    }

    private Map<String, PredicateProcessor> extractAnnotatedMethodsAsPredicates(final String libraryUID, final Set<String> foundZeroArityPredicates) {
        final Map<String, PredicateProcessor> result = new HashMap<>();

        final Method[] methods = this.getClass().getMethods();
        for (final Method method : methods) {
            final Predicate predicateAnnotation = method.getAnnotation(Predicate.class);
            final PredicateSynonyms synonims = method.getAnnotation(PredicateSynonyms.class);

            if (predicateAnnotation != null) {
                final String signature = Utils.normalizeSignature(predicateAnnotation.Signature());

                if (signature == null) {
                    throw new ProlCriticalError("Wrong signature of a predicate method " + method.getName() + " at " + libraryUID);
                }

                if (result.containsKey(signature)) {
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
                result.put(signature, processor);
                if (signature.endsWith("/0")) {
                    foundZeroArityPredicates.add(signature.substring(0, signature.lastIndexOf('/')));
                }

                if (synonims != null) {
                    final String[] synonimSignatures = synonims.Signatures();
                    for (String synonimSignature : synonimSignatures) {
                        final String sig = synonimSignature.trim();
                        result.put(sig, processor);
                        if (sig.endsWith("/0")) {
                            foundZeroArityPredicates.add(sig.substring(0, sig.lastIndexOf('/')));
                        }
                    }
                }
            }
        }

        return result;
    }

    private static void registerStaticOperator(final Map<String, OperatorContainer> operatorMap, final ProlOperator operator) {
        Operator newOperator = new Operator(operator.Priority(), operator.Type(), operator.Name());
        OperatorContainer container = operatorMap.get(operator.Name());
        if (container == null) {
            container = new OperatorContainer(newOperator, true);
            operatorMap.put(operator.Name(), container);
        } else {
            container.setOperator(newOperator);
        }
    }

    private static Map<String, OperatorContainer> loadStaticOperators(final Class<?> klazz) {
        final Map<String, OperatorContainer> result = new HashMap<>();
        final ProlOperators operators = klazz.getAnnotation(ProlOperators.class);
        if (operators != null) {
            ProlOperator[] operatorList = operators.Operators();
            for (final ProlOperator lst : operatorList) {
                registerStaticOperator(result, lst);
            }
        }

        final ProlOperator operator = klazz.getAnnotation(ProlOperator.class);
        if (operator != null) {
            registerStaticOperator(result, operator);
        }
        return result;
    }

    public boolean isSystemOperator(final String nameToBeChecked) {
        return this.libraryOperators.containsKey(nameToBeChecked);
    }

    public boolean hasSyatemOperatorStartsWith(final String startSubstring) {
        return this.libraryOperators.keySet().stream().anyMatch((s) -> (s.startsWith(startSubstring)));
    }

    public OperatorContainer findSystemOperatorForName(final String operatorName) {
        return this.libraryOperators.get(operatorName);
    }

    public void contextHasBeenHalted(final ProlContext context) {
    }
}
