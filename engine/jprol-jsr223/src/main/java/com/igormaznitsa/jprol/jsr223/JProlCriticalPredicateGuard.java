package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

/**
 * Interface describing an object which can disable execution of predicates marked as critical.
 */
@FunctionalInterface
public interface JProlCriticalPredicateGuard {
  boolean isCriticalPredicateAllowed(
      Class<? extends AbstractJProlLibrary> sourceLibrary,
      JProlChoicePoint choicePoint,
      String predicateIndicator);
}
