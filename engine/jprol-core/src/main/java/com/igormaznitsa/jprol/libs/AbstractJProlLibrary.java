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
import static com.igormaznitsa.jprol.utils.ProlUtils.SIGNATURE_OPERATOR;
import static java.lang.Integer.parseInt;
import static java.util.Collections.unmodifiableMap;
import static java.util.Collections.unmodifiableSet;

import com.igormaznitsa.jprol.annotations.JProlConsultFile;
import com.igormaznitsa.jprol.annotations.JProlConsultResource;
import com.igormaznitsa.jprol.annotations.JProlConsultText;
import com.igormaznitsa.jprol.annotations.JProlConsultUrl;
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
import com.igormaznitsa.jprol.logic.AutoArgumentValidator;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.PredicateInvoker;
import com.igormaznitsa.jprol.utils.CloseableIterator;
import com.igormaznitsa.jprol.utils.OperatorIterator;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.ProlPair;
import com.igormaznitsa.jprol.utils.ProlUtils;
import com.igormaznitsa.jprol.utils.lazy.LazyMap;
import com.igormaznitsa.jprol.utils.lazy.LazySet;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class AbstractJProlLibrary {

  private final String libraryUid;
  private final Map<String, TermOperatorContainer> systemOperators;
  private final Map<String, PredicateInvoker> predicateMethodsMap;
  private final Set<String> zeroArityPredicateNames;

  private final Map<JProlContext, Map<String, Object>> contextNamedObjects =
      new ConcurrentHashMap<>();

  @SuppressWarnings("this-escape")
  public AbstractJProlLibrary(final String libraryUid) {
    if (libraryUid == null) {
      throw new IllegalArgumentException("Library UID must not be null!");
    }

    this.libraryUid = libraryUid;

    this.systemOperators =
        unmodifiableMap(loadStaticOperators(this.getClass(), SourcePosition.UNKNOWN));
    final Set<String> zeroArityPredicates = new LazySet<>();
    this.predicateMethodsMap =
        unmodifiableMap(extractAnnotatedMethodsAsPredicates(libraryUid, zeroArityPredicates));
    this.zeroArityPredicateNames = unmodifiableSet(zeroArityPredicates);
  }

  protected static NumericTerm calcEvaluable(final JProlChoicePoint choicePoint, final Term term) {
    try {
      final Term thatTerm = term.tryGround();
      if (thatTerm.getTermType() == VAR) {
        throw new ProlInstantiationErrorException("Non-instantiated var: " + term,
            choicePoint.getGoalTerm());
      }

      final NumericTerm result;

      switch (thatTerm.getTermType()) {
        case ATOM: {
          ProlAssertions.assertNumber(thatTerm);
          result = (NumericTerm) thatTerm;
        }
        break;
        case STRUCT: {
          final PredicateInvoker processor = ((TermStruct) thatTerm).getPredicateProcessor();
          if (processor.isEvaluable()) {
            result = (NumericTerm) processor.executeEvaluable(choicePoint, (TermStruct) thatTerm);
          } else {
            throw new ProlTypeErrorException("evaluable", "Non-evaluable item found: " + thatTerm,
                choicePoint.getGoalTerm());
          }
        }
        break;
        default:
          throw new ProlTypeErrorException("evaluable", "Can't evaluate item: " + term,
              choicePoint.getGoalTerm());
      }
      return result;
    } catch (final ArithmeticException ex) {
      throw new ProlEvaluationErrorException(ex.getMessage(), "Arithmetic exception",
          choicePoint.getGoalTerm(), ex);
    }
  }

  private static void assertProlOperator(final JProlOperator operator) {
    if (operator.priority() < TermOperator.PRIORITY_MAX ||
        operator.priority() > TermOperator.PRIORITY_MIN) {
      throw new IllegalArgumentException(
          "Operator priority must be in " + TermOperator.PRIORITY_MAX + "..." +
              TermOperator.PRIORITY_MIN + " but " + operator.priority());
    }
    if (operator.name().isEmpty()) {
      throw new IllegalArgumentException("Operator name must not be empty");
    }
    if (Character.isUpperCase(operator.name().charAt(0))) {
      throw new IllegalArgumentException("Operator name must not start with upper cased char");
    }
    if (operator.name().charAt(0) == '_') {
      throw new IllegalArgumentException("Operator name must not start with '_'");
    }
    if (Character.isDigit(operator.name().charAt(0))) {
      throw new IllegalArgumentException("Operator name must not start with a digit");
    }
    if (operator.name().split("\\s").length > 1) {
      throw new IllegalArgumentException(
          "Operator name must not contain whitespaces: " + operator.name());
    }
  }

  private static void registerStaticOperator(final Map<String, TermOperatorContainer> operatorMap,
                                             final JProlOperator operator,
                                             final SourcePosition sourcePosition) {
    assertProlOperator(operator);
    TermOperator newOperator =
        new TermOperator(operator.priority(), operator.type(), operator.name(), sourcePosition);
    TermOperatorContainer container = operatorMap.get(operator.name());
    if (container == null) {
      container = TermOperatorContainer.makeFor(newOperator, sourcePosition);
      operatorMap.put(operator.name(), container);
    } else {
      final TermOperatorContainer newContainer = container.makeFor(newOperator);
      if (newContainer == null) {
        throw new IllegalStateException("Detected duplicated type of operator: " + operator);
      } else {
        operatorMap.put(operator.name(), newContainer);
      }
    }
  }

  private static Map<String, TermOperatorContainer> loadStaticOperators(final Class<?> targetClass,
                                                                        final SourcePosition sourcePosition) {
    final Map<String, TermOperatorContainer> result = new LazyMap<>();
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
    if (!a.unifyWith(b)) {
      throw new ProlCriticalError(
          "Can't unify terms, it's critical unexpected error, contact developer!");
    }
    return true;
  }

  public List<JProlPredicate> findAllJProlPredicates() {
    final List<JProlPredicate> allLibraryPredicates = new ArrayList<>();

    final JProlConsultText consultText = this.getClass().getAnnotation(JProlConsultText.class);
    final JProlConsultResource consultResource =
        this.getClass().getAnnotation(JProlConsultResource.class);
    final JProlConsultFile consultFile = this.getClass().getAnnotation(JProlConsultFile.class);
    final JProlConsultUrl consultUrl = this.getClass().getAnnotation(JProlConsultUrl.class);

    if (consultText != null) {
      allLibraryPredicates.addAll(Arrays.asList(consultText.declaredPredicates()));
    }

    if (consultFile != null) {
      allLibraryPredicates.addAll(Arrays.asList(consultFile.declaredPredicates()));
    }

    if (consultResource != null) {
      allLibraryPredicates.addAll(Arrays.asList(consultResource.declaredPredicates()));
    }

    if (consultUrl != null) {
      allLibraryPredicates.addAll(Arrays.asList(consultUrl.declaredPredicates()));
    }

    for (final Method method : this.getClass().getMethods()) {
      final JProlPredicate predicate = method.getAnnotation(JProlPredicate.class);
      if (predicate != null) {
        allLibraryPredicates.add(predicate);
      }
    }

    return allLibraryPredicates;
  }

  @SuppressWarnings("EmptyMethod")
  public void onRegisteredInContext(final JProlContext context) {

  }

  @SuppressWarnings("EmptyMethod")
  public void onLibraryRemove(final JProlContext context) {

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
        .filter(predicateIndicator::isUnifiableWith)
        .collect(Collectors.toList());
  }

  public boolean hasPredicateForSignature(final String signature) {
    return this.predicateMethodsMap.containsKey(signature);
  }

  public PredicateInvoker findPredicateProcessor(final TermStruct predicate) {
    final String signature = predicate.getSignature();
    PredicateInvoker result = this.onBeforeFindPredicateProcessor(signature);
    if (result == null) {
      {
        result = this.predicateMethodsMap.get(predicate.getSignature());
      }
    }
    return result;
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

  protected PredicateInvoker onBeforeFindPredicateProcessor(final String signature) {
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

  public final void onContextDispose(final JProlContext context) {
    if (context.isRootContext()) {
      // make disposing notification only for root context
      this.onCallContextDispose(context, this.contextNamedObjects.remove(context));
    }
  }

  protected void onCallContextDispose(final JProlContext context,
                                      final Map<String, Object> contextNamedObjects) {
    if (contextNamedObjects != null) {
      contextNamedObjects.values().stream()
          .filter(x -> x instanceof AutoCloseable)
          .map(x -> (AutoCloseable) x)
          .forEach(x -> {
            try {
              x.close();
            } catch (Exception ex) {
              // ignore
            }
          });
    }
  }

  private Map<String, PredicateInvoker> extractAnnotatedMethodsAsPredicates(final String libraryUID,
                                                                            final Set<String> foundZeroArityPredicates) {
    final Map<String, PredicateInvoker> result = new HashMap<>();

    final Method[] methods = this.getClass().getMethods();
    for (final Method method : methods) {
      final JProlPredicate predicateAnnotation = method.getAnnotation(JProlPredicate.class);
      if (predicateAnnotation == null) {
        continue;
      }

      try {
        final List<String> signatures =
            Stream.concat(
                    Stream.of(Objects.requireNonNull(predicateAnnotation.signature(),
                        "Signature must not be null")),
                    Stream.of(predicateAnnotation.synonyms()))
                .collect(Collectors.toList());

        final List<List<AutoArgumentValidator.ModificatorArgument>> validateTypes =
            AutoArgumentValidator.parse(predicateAnnotation.validate());

        for (final String s : signatures) {
          final ProlPair<String, Integer> signaturePair = ProlUtils.parseSignaturePair(s);
          final String reassembledSignature =
              signaturePair.getLeft() + '/' + signaturePair.getRight();

          if (result.containsKey(reassembledSignature)) {
            throw new ProlCriticalError(
                "Duplicated predicate method " + reassembledSignature + " at " + libraryUID);
          }

          final Predicate<TermStruct> termValidator =
              AutoArgumentValidator.makeFor(signaturePair.getRight(), validateTypes, true);

          final PredicateInvoker invoker =
              new PredicateInvoker(this,
                  predicateAnnotation.guarded(),
                  predicateAnnotation.determined(),
                  predicateAnnotation.evaluable(),
                  predicateAnnotation.changesChooseChain(),
                  termValidator,
                  reassembledSignature,
                  method);

          result.put(reassembledSignature, invoker);
          if (signaturePair.getRight() == 0) {
            foundZeroArityPredicates.add(signaturePair.getLeft());
          }
        }
      } catch (Exception ex) {
        throw new IllegalArgumentException(
            String.format("Can't register predicate %s from library %s for error",
                predicateAnnotation, libraryUID), ex);
      }

    }

    return result;
  }
}
