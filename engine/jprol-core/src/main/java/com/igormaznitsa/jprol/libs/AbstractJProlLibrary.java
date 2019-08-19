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

import com.igormaznitsa.jprol.annotations.Predicate;
import com.igormaznitsa.jprol.annotations.PredicateSynonyms;
import com.igormaznitsa.jprol.annotations.ProlOperator;
import com.igormaznitsa.jprol.annotations.ProlOperators;
import com.igormaznitsa.jprol.data.*;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.CheckingTemplate;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import com.igormaznitsa.jprol.utils.OperatorIterator;
import com.igormaznitsa.jprol.utils.Utils;

import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.igormaznitsa.jprol.data.TermType.VAR;
import static com.igormaznitsa.jprol.data.Terms.*;
import static com.igormaznitsa.jprol.utils.Utils.SIGNATURE_OPERATOR;
import static java.lang.Integer.parseInt;

public abstract class AbstractJProlLibrary {

  private final String libraryUid;
  private final Map<String, TermOperatorContainer> systemOperators;
  private final Map<String, PredicateInvoker> predicateMethodsMap;
  private final Set<String> zeroArityPredicateNames;

  private final Map<JProlContext, Map<String, Object>> contextNamedObjects = new ConcurrentHashMap<>();

  public AbstractJProlLibrary(final String libraryUid) {
    if (libraryUid == null) {
      throw new IllegalArgumentException("Library UID must not be null!");
    }

    this.libraryUid = libraryUid;

    this.systemOperators = Collections.unmodifiableMap(loadStaticOperators(this.getClass()));
    final Set<String> zeroArityPredicates = new HashSet<>();
    this.predicateMethodsMap = Collections.unmodifiableMap(extractAnnotatedMethodsAsPredicates(libraryUid, zeroArityPredicates));
    this.zeroArityPredicateNames = Collections.unmodifiableSet(zeroArityPredicates);
  }

  protected static boolean assertUnify(final Term a, final Term b) {
    if (!a.unifyTo(b)) {
      throw new ProlCriticalError("Can't unify terms, it's critical unexpected error, contact developer!");
    }
    return true;
  }

  public CloseableIterator<TermOperator> makeOperatorIterator() {
    return new OperatorIterator(this.systemOperators.values().iterator());
  }

  @SuppressWarnings("unchecked")
  protected <T> T findContextObject(final JProlContext context, final String objectId, final Function<String, T> defaultSupplier) {
    return (T) this.contextNamedObjects
        .computeIfAbsent(context, ctx -> new ConcurrentHashMap<>())
        .computeIfAbsent(objectId, defaultSupplier);
  }

  protected void putContextObject(final JProlContext context, final String objectId, final Object obj) {
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

  private static void registerStaticOperator(final Map<String, TermOperatorContainer> operatorMap, final ProlOperator operator) {
    TermOperator newOperator = new TermOperator(operator.priority(), operator.type(), operator.name());
    TermOperatorContainer container = operatorMap.get(operator.name());
    if (container == null) {
      container = new TermOperatorContainer(newOperator);
      operatorMap.put(operator.name(), container);
    } else {
      container.setOperator(newOperator);
    }
  }

  private static Map<String, TermOperatorContainer> loadStaticOperators(final Class<?> klazz) {
    final Map<String, TermOperatorContainer> result = new HashMap<>();
    final ProlOperators operators = klazz.getAnnotation(ProlOperators.class);
    if (operators != null) {
      ProlOperator[] operatorList = operators.operators();
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

  public List<TermStruct> findAllForPredicateIndicator(final Term predicateIndicator) {
    return this.predicateMethodsMap.keySet()
        .stream()
        .map(key -> {
          final int index = key.lastIndexOf('/');
          return newStruct(SIGNATURE_OPERATOR,
              new Term[] {
                  newAtom(key.substring(0, index)),
                  newLong(parseInt(key.substring(index + 1)))
              });
        })
        .filter(predicateIndicator::dryUnifyTo)
        .collect(Collectors.toList());
  }

  public boolean hasPredicateForSignature(final String signature) {
    return this.predicateMethodsMap.containsKey(signature);
  }

  protected static NumericTerm calculatEvaluable(final ChoicePoint goal, Term term) {
    try {
      if (term.getTermType() == VAR) {
        term = term.findNonVarOrSame();
        if (term.getTermType() == VAR) {
          throw new ProlInstantiationErrorException("Non-instantiated variable", term);
        }
      }

      final NumericTerm result;

      switch (term.getTermType()) {
        case ATOM: {
          if (term instanceof NumericTerm) {
            result = (NumericTerm) term;
          } else {
            throw new ProlTypeErrorException("number", "Not a numeric atom +[" + term + "] found at goal [" + goal + ']', term);
          }
        }
        break;
        case STRUCT: {
          final PredicateInvoker processor = ((TermStruct) term).getPredicateProcessor();
          if (processor.isEvaluable()) {
            result = (NumericTerm) processor.executeEvaluable(goal, (TermStruct) term);
          } else {
            throw new ProlTypeErrorException("evaluable", "Not an arithmetic operator found [" + goal.toString() + ']', term);
          }
        }
        break;
        default:
          throw new ProlTypeErrorException("evaluable", "Unsupported atom at an arithmetic expression [" + goal.toString() + ']', term);
      }
      return result;
    } catch (final ArithmeticException ex) {
      throw new ProlEvaluationErrorException(ex.getMessage(), "Arithmetic exception", term, ex);
    }
  }

  public PredicateInvoker findProcessorForPredicate(final TermStruct predicate) {
    final String signture = predicate.getSignature();
    PredicateInvoker result = onBeforeFindProcessorForPredicate(signture);
    return result == null ? this.predicateMethodsMap.get(predicate.getSignature()) : result;
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

  public boolean hasSyatemOperatorStartsWith(final String startSubstring) {
    return this.systemOperators.keySet().stream().anyMatch((s) -> (s.startsWith(startSubstring)));
  }

  public TermOperatorContainer findSystemOperatorForName(final String operatorName) {
    return this.systemOperators.get(operatorName);
  }

  public void onContextDispose(final JProlContext context) {
    this.contextNamedObjects.remove(context);
  }

  private Map<String, PredicateInvoker> extractAnnotatedMethodsAsPredicates(final String libraryUID, final Set<String> foundZeroArityPredicates) {
    final Map<String, PredicateInvoker> result = new HashMap<>();

    final Method[] methods = this.getClass().getMethods();
    for (final Method method : methods) {
      final Predicate predicateAnnotation = method.getAnnotation(Predicate.class);
      final PredicateSynonyms synonims = method.getAnnotation(PredicateSynonyms.class);

      if (predicateAnnotation != null) {
        final String signature = Utils.normalizeSignature(predicateAnnotation.signature());

        if (signature == null) {
          throw new ProlCriticalError("Wrong signature of a predicate method " + method.getName() + " at " + libraryUID);
        }

        if (result.containsKey(signature)) {
          throw new ProlCriticalError("Duplicated predicate method " + signature + " at " + libraryUID);
        }

        CheckingTemplate[][] templates = null;
        final String[] templateStrings = predicateAnnotation.template();
        if (templateStrings.length > 0) {
          templates = new CheckingTemplate[templateStrings.length][];
          for (int lt = 0; lt < templateStrings.length; lt++) {
            final String[] str = templateStrings[lt].split(",");
            CheckingTemplate[] curtemp = new CheckingTemplate[str.length];
            for (int ld = 0; ld < str.length; ld++) {
              curtemp[ld] = new CheckingTemplate(str[ld]);
            }
            templates[lt] = curtemp;
          }
        }

        final PredicateInvoker invoker = new PredicateInvoker(this, signature, method, templates);
        result.put(signature, invoker);
        if (signature.endsWith("/0")) {
          foundZeroArityPredicates.add(signature.substring(0, signature.lastIndexOf('/')));
        }

        if (synonims != null) {
          final String[] synonimSignatures = synonims.signatures();
          for (String synonimSignature : synonimSignatures) {
            final String sig = synonimSignature.trim();
            result.put(sig, invoker);
            if (sig.endsWith("/0")) {
              foundZeroArityPredicates.add(sig.substring(0, sig.lastIndexOf('/')));
            }
          }
        }
      }
    }

    return result;
  }
}
