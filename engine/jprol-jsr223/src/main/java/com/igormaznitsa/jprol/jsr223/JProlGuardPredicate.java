package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

/**
 * Policy hook for predicates declared with {@link JProlPredicate#guarded()} set to {@code true} on a library
 * (typically I/O, {@code consult/1}, reflection-like builtins, or other sensitive operations).
 * <p>
 * When a guarded predicate is about to run, the engine calls {@link #isGuardedPredicateAllowed(Class, JProlChoicePoint, String)}.
 * If it returns {@code false}, execution fails with a permission error ({@code access/prohibited_predicate}).
 * If it returns {@code true}, the predicate runs normally.
 * <p>
 * <b>Installation:</b> put an instance under the key {@value JProlBindingsConstants#JPROL_GLOBAL_GUARD_PREDICATE} in
 * {@link javax.script.ScriptContext#GLOBAL_SCOPE global} {@link javax.script.Bindings} (for example via
 * {@link javax.script.ScriptEngine#setBindings(javax.script.Bindings, int)}). The value is read when a
 * {@linkplain com.igormaznitsa.jprol.logic.JProlContext JProl context} is first created for a thread inside
 * {@link JProlScriptEngineContext}. If no predicate is bound, all guarded predicates are allowed (same as always returning
 * {@code true}).
 * <p>
 * <b>{@code predicateIndicator}:</b> functor and arity as a single string, e.g. {@code "write/1"}, {@code "consult/1"},
 * {@code "clause/2"} — suitable for simple allow-lists or deny-lists.
 * <p>
 * Example: keep normal I/O but block {@code clause/2} introspection (see tests in {@code SimpleJsr223Test}):
 * <pre>{@code
 * Bindings global = new SimpleBindings();
 * global.put(JProlBindingsConstants.JPROL_GLOBAL_GUARD_PREDICATE,
 *     (JProlGuardPredicate) (libraryClass, choicePoint, indicator) ->
 *         !"clause/2".equals(indicator));
 * engine.setBindings(global, javax.script.ScriptContext.GLOBAL_SCOPE);
 * }</pre>
 *
 * @see JProlBindingsConstants#JPROL_GLOBAL_GUARD_PREDICATE
 * @see JProlPredicate#guarded()
 */
@FunctionalInterface
public interface JProlGuardPredicate {
  /**
   * Whether the guarded predicate identified by {@code predicateIndicator} may run in this invocation.
   * <p>
   * Implementations may inspect {@code sourceLibraryClass} (which library owns the builtin), {@code choicePoint}
   * (current goal stack / context), or {@code predicateIndicator} (e.g. {@code "see/1"}). Heavy work should be avoided:
   * this runs on the hot path before every guarded predicate call.
   *
   * @param sourceLibraryClass class of the {@link com.igormaznitsa.jprol.libs.AbstractJProlLibrary} that registered the predicate
   * @param choicePoint        active choice point for the call, must not be null
   * @param predicateIndicator signature {@code "name/arity"}, must not be null
   * @return {@code true} to allow execution, {@code false} to reject with a permission error
   * @see JProlPredicate#guarded()
   */
  boolean isGuardedPredicateAllowed(
      Class<? extends AbstractJProlLibrary> sourceLibraryClass,
      JProlChoicePoint choicePoint,
      String predicateIndicator);
}
