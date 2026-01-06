package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;

/**
 * Constants of special attributes in Bindings for JProl JSR 223.
 *
 * @see javax.script.Bindings
 */
public interface JProlBindingsConstants {
  /**
   * Checker to allow guarded predicates which guarded flag is true.
   *
   * @see JProlGuardPredicate
   * @see JProlPredicate#guarded()
   */
  String JPROL_GLOBAL_GUARD_PREDICATE = "jprol.global.guard.predicate";

  /**
   * Executor service to be used for all child JProl engines to start async processes. Can be defined only in global scope.
   */
  String JPROL_GLOBAL_EXECUTOR_SERVICE = "jprol.global.executor.service";

  /**
   * Knowledge base to be used for all child JProl engines. Can be defined only in global scope.
   */
  String JPROL_GLOBAL_KNOWLEDGE_BASE = "jprol.global.knowledge.base";

  /**
   * Array of JProl libraries which will be applied to JProl engine context during create. Can be defined in both global and engine context.
   */
  String JPROL_LIBRARIES = "jprol.libraries";

  /**
   * Map of JProl flag names as String and their values as objects to be applied during JProl engine initialization. Can be defined in both global and engine context.
   */
  String JPROL_CONTEXT_FLAGS = "jprol.context.flags";

  /**
   * Instance of global variables store to be used in created contexts.
   *
   * @see com.igormaznitsa.jprol.logic.KeyValueTermStore
   */
  String JPROL_VARIABLES_STORE = "jprol.context.variables.store";
}
