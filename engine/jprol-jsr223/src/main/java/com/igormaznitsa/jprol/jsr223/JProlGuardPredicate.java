package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

/**
 * Interface describing an object which can disable execution of predicates marked as critical.
 */
@FunctionalInterface
public interface JProlGuardPredicate {
  /**
   * Check a guarded predicate
   *
   * @param sourceLibraryClass the source library class for the predicate, must not be null
   * @param choicePoint        the calling choice point, must not be null
   * @param predicateIndicator the checking predicate indicator like 'consult/1', must not be null
   * @return true if predicate allowed, false otherwise
   * @see JProlPredicate#guarded()
   */
  boolean isGuardedPredicateAllowed(
      Class<? extends AbstractJProlLibrary> sourceLibraryClass,
      JProlChoicePoint choicePoint,
      String predicateIndicator);
}
