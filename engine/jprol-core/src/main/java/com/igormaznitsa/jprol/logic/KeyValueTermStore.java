package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import java.util.Optional;
import java.util.function.BiPredicate;

/**
 * Name-to-term map used as the global store of a context.
 *
 * <p>Pass an instance to the {@link JProlContext} constructor. The root context and its child
 * contexts share that instance. Prolog {@code nb_setval/2}, {@code nb_getval/2}, and
 * {@code nb_delete/1} read and write it. The store is absent when the constructor was given
 * {@code null}; {@link JProlContext#getGlobalVariablesStore()} is then empty and those predicates
 * have no place to keep a value.
 *
 * <p>Keys are plain strings. Values are terms. An implementation stores and returns clones, so a
 * later unification of the caller's term cannot change the stored term, and a term read from the
 * store cannot change the stored term either. The default implementation is
 * {@link com.igormaznitsa.jprol.utils.DefaultKeyValueTermStore}.
 *
 * <pre>{@code
 * KeyValueTermStore store = new DefaultKeyValueTermStore("globals");
 * JProlContext context = new JProlContext("demo", new File("."), store);
 * store.setValue("answer", Terms.newLong(42));
 * Optional<Term> answer = store.findValue("answer");
 * }</pre>
 *
 * @see JProlContext#getGlobalVariablesStore()
 * @since 3.0.0
 */
public interface KeyValueTermStore {

  /**
   * Remove every entry for which {@code predicate} returns {@code true}.
   *
   * <p>The predicate receives the key and the stored term. It must not modify the store. An entry
   * for which the predicate returns {@code false} stays. The removal happens after the test, so a
   * predicate sees a stable snapshot of the entries it is asked about.
   *
   * @param predicate test of {@code (key, storedTerm)}; {@code true} marks the entry for removal,
   *     must not be {@code null}
   * @throws NullPointerException when {@code predicate} is {@code null}
   * @throws RuntimeException when {@code predicate} throws
   */
  void removeif(final BiPredicate<String, Term> predicate);

  /**
   * Read the term stored under {@code name}.
   *
   * <p>The returned term is a clone of the stored term. An unknown name is an empty optional,
   * which is the normal miss.
   *
   * @param name key, must not be {@code null}
   * @return a clone of the stored term, or an empty optional when {@code name} is absent
   * @throws NullPointerException when {@code name} is {@code null}
   */
  Optional<Term> findValue(final String name);

  /**
   * Store a clone of {@code value} under {@code name}.
   *
   * <p>A previous term for the same name is replaced. The implementation must keep a clone, so
   * later changes to {@code value} stay outside the store.
   *
   * @param name key, must not be {@code null}
   * @param value term to store, must not be {@code null}
   * @throws NullPointerException when {@code name} or {@code value} is {@code null}
   */
  void setValue(final String name, final Term value);

  /**
   * Drop the term stored under {@code name}.
   *
   * <p>An unknown name leaves the store unchanged.
   *
   * @param name key, must not be {@code null}
   * @throws NullPointerException when {@code name} is {@code null}
   */
  void remove(final String name);

  /**
   * Number of stored entries.
   *
   * @return the number of keys, zero when the store is empty
   */
  long size();

  /**
   * Report whether the store has no entries.
   *
   * @return {@code true} when {@link #size()} is zero
   */
  boolean isEmpty();

}
