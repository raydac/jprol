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

package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.data.Terms.newLong;
import static com.igormaznitsa.jprol.data.Terms.newStruct;
import static com.igormaznitsa.jprol.utils.Utils.SIGNATURE_OPERATOR;
import static java.lang.Integer.parseInt;

import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlOperators;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermOperatorContainer;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import com.igormaznitsa.jprol.utils.OperatorIterator;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.Utils;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

public abstract class AbstractJProlLibrary {

  private final String libraryUid;
  private final Map<String, TermOperatorContainer> systemOperators;
  private final Map<String, PredicateInvoker> predicateMethodsMap;
  private final Set<String> zeroArityPredicateNames;

  private final Map<JProlContext, Map<String, Object>> contextNamedObjects =
      new ConcurrentHashMap<>();

  public AbstractJProlLibrary(final String libraryUid) {
    if (libraryUid == null) {
      throw new IllegalArgumentException("Library UID must not be null!");
    }

    this.libraryUid = libraryUid;

    this.systemOperators =
        Collections.unmodifiableMap(loadStaticOperators(this.getClass(), SourcePosition.UNKNOWN));
    final Set<String> zeroArityPredicates = new HashSet<>();
    this.predicateMethodsMap = Collections
        .unmodifiableMap(extractAnnotatedMethodsAsPredicates(libraryUid, zeroArityPredicates));
    this.zeroArityPredicateNames = Collections.unmodifiableSet(zeroArityPredicates);
  }

  private static void registerStaticOperator(final Map<String, TermOperatorContainer> operatorMap,
                                             final JProlOperator operator,
                                             final SourcePosition sourcePosition) {
    TermOperator newOperator =
        new TermOperator(operator.priority(), operator.type(), operator.name(), sourcePosition);
    TermOperatorContainer container = operatorMap.get(operator.name());
    if (container == null) {
      container = new TermOperatorContainer(newOperator, sourcePosition);
      operatorMap.put(operator.name(), container);
    } else {
      container.setOperator(newOperator);
    }
  }

  private static Map<String, TermOperatorContainer> loadStaticOperators(final Class<?> targetClass,
                                                                        final SourcePosition sourcePosition) {
    final Map<String, TermOperatorContainer> result = new HashMap<>();
    final JProlOperators operators = targetClass.getAnnotation(JProlOperators.class);
    if (operators != null) {
      JProlOperator[] operatorList = operators.value();
      for (final JProlOperator lst : operatorList) {
        registerStaticOperator(result, lst, sourcePosition);
      }
    }

    final JProlOperator operator = targetClass.getAnnotation(JProlOperator.class);
    if (operator != null) {
      registerStaticOperator(result, operator, sourcePosition);
    }
    return result;
  }

  protected static boolean assertUnify(final Term a, final Term b) {
    if (!a.unifyTo(b)) {
      throw new ProlCriticalError(
          "Can't unify terms, it's critical unexpected error, contact developer!");
    }
    return true;
  }

  @SuppressWarnings("EmptyMethod")
  public void onRegisteredInContext(final JProlContext context) {

  }

  @SuppressWarnings("EmptyMethod")
  public void onLibraryRemove(final JProlContext context) {

  }

  protected static NumericTerm calcEvaluable(final JProlChoicePoint choicePoint, final Term term) {
    try {
      if (term.getTermType() == VAR) {
        throw new ProlInstantiationErrorException("Non-instantiated var: " + term, term);
      }

      final NumericTerm result;

      switch (term.getTermType()) {
        case ATOM: {
          ProlAssertions.assertNumber(term);
          result = (NumericTerm) term;
        }
        break;
        case STRUCT: {
          final PredicateInvoker processor = ((TermStruct) term).getPredicateProcessor();
          if (processor.isEvaluable()) {
            result = (NumericTerm) processor.executeEvaluable(choicePoint, (TermStruct) term);
          } else {
            throw new ProlTypeErrorException("evaluable", "Non-evaluable item found: " + term,
                term);
          }
        }
        break;
        default:
          throw new ProlTypeErrorException("evaluable", "Can't evaluate item: " + term, term);
      }
      return result;
    } catch (final ArithmeticException ex) {
      throw new ProlEvaluationErrorException(ex.getMessage(), "Arithmetic exception", term, ex);
    }
  }

  public CloseableIterator<TermOperator> makeOperatorIterator() {
    return new OperatorIterator(this.systemOperators.values().iterator());
  }

  @SuppressWarnings("unchecked")
  protected <T> T findContextObject(final JProlContext context, final String objectId,
                                    final Function<String, T> defaultSupplier) {
    return (T) this.contextNamedObjects
        .computeIfAbsent(context, ctx -> new ConcurrentHashMap<>())
        .computeIfAbsent(objectId, defaultSupplier);
  }

  protected void putContextObject(final JProlContext context, final String objectId,
                                  final Object obj) {
    final Map<String, Object> contextMap = this.contextNamedObjects
        .computeIfAbsent(context, ctx -> new ConcurrentHashMap<>());

    if (obj == null) {
      contextMap.remove(objectId);
    } else {
      contextMap.put(objectId, obj);
    }
  }

  protected Map<String, Object> getContextNamedObjects(final JProlContext context) {
    return this.contextNamedObjects.computeIfAbsent(context, ctx -> new ConcurrentHashMap<>());
  }

  public List<PredicateInvoker> findAllPredicateInvokersForSignature(final String signature) {
    return this.predicateMethodsMap.entrySet().stream()
        .filter(e -> e.getKey().equals(signature))
        .map(Map.Entry::getValue)
        .collect(Collectors.toList());
  }

  public List<TermStruct> findAllForPredicateIndicator(final Term predicateIndicator) {
    return this.predicateMethodsMap.keySet()
        .stream()
        .map(key -> {
          final int index = key.lastIndexOf('/');
          return newStruct(SIGNATURE_OPERATOR,
              new Term[] {
                  newAtom(key.substring(0, index), predicateIndicator.getSourcePosition()),
                  newLong(parseInt(key.substring(index + 1)),
                      predicateIndicator.getSourcePosition())
              });
        })
        .filter(predicateIndicator::dryUnifyTo)
        .collect(Collectors.toList());
  }

  public boolean hasPredicateForSignature(final String signature) {
    return this.predicateMethodsMap.containsKey(signature);
  }

  public PredicateInvoker findProcessorForPredicate(final TermStruct predicate) {
    final String signature = predicate.getSignature();
    PredicateInvoker result = onBeforeFindProcessorForPredicate(signature);
    return result == null ? this.predicateMethodsMap.get(predicate.getSignature()) : result;
  }

  public boolean hasZeroArityPredicate(final String predicateName) {
    return this.onBeforeHasZeroArityPredicate(predicateName) ||
        this.zeroArityPredicateNames.contains(predicateName);
  }

  protected boolean onBeforeHasZeroArityPredicate(final String predicateName) {
    return false;
  }

  public String getLibraryUid() {
    return this.libraryUid;
  }

  public void release() {
    this.contextNamedObjects.clear();
  }

  @Override
  public int hashCode() {
    return this.libraryUid.hashCode();
  }

  protected PredicateInvoker onBeforeFindProcessorForPredicate(final String signature) {
    return null;
  }

  @Override
  public boolean equals(Object obj) {
    boolean result = false;
    if (obj instanceof AbstractJProlLibrary) {
      result = this.libraryUid.equals(((AbstractJProlLibrary) obj).libraryUid);
    }
    return result;
  }

  public boolean isSystemOperator(final String nameToBeChecked) {
    return this.systemOperators.containsKey(nameToBeChecked);
  }

  public boolean isSystemOperatorStartsWith(final String startSubstring) {
    return this.systemOperators.keySet().stream().anyMatch((s) -> (s.startsWith(startSubstring)));
  }

  public TermOperatorContainer findSystemOperatorForName(final String operatorName) {
    return this.systemOperators.get(operatorName);
  }

  public void onContextDispose(final JProlContext context) {
    this.contextNamedObjects.remove(context);
  }

  private Map<String, PredicateInvoker> extractAnnotatedMethodsAsPredicates(final String libraryUID,
                                                                            final Set<String> foundZeroArityPredicates) {
    final Map<String, PredicateInvoker> result = new HashMap<>();

    final Method[] methods = this.getClass().getMethods();
    for (final Method method : methods) {
      final JProlPredicate predicateAnnotation = method.getAnnotation(JProlPredicate.class);

      if (predicateAnnotation != null) {
        final String signature = Utils.normalizeSignature(predicateAnnotation.signature());

        if (signature == null) {
          throw new ProlCriticalError(
              "Wrong signature of a predicate method " + method.getName() + " at " + libraryUID);
        }

        if (result.containsKey(signature)) {
          throw new ProlCriticalError(
              "Duplicated predicate method " + signature + " at " + libraryUID);
        }

        final PredicateInvoker invoker =
            new PredicateInvoker(this, predicateAnnotation.determined(),
                predicateAnnotation.evaluable(), predicateAnnotation.changesChooseChain(),
                signature, method);
        result.put(signature, invoker);
        if (signature.endsWith("/0")) {
          foundZeroArityPredicates.add(signature.substring(0, signature.lastIndexOf('/')));
        }

        final String[] synonymSignatures = predicateAnnotation.synonyms();
        for (final String s : synonymSignatures) {
          final String trimmed = s.trim();
          result.put(trimmed, invoker);
          if (trimmed.endsWith("/0")) {
            foundZeroArityPredicates.add(trimmed.substring(0, trimmed.lastIndexOf('/')));
          }
        }
      }
    }

    return result;
  }
}
