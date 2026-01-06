package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import java.util.Optional;
import java.util.function.BiPredicate;

/**
 * Interface of a Key value store which can play role of global variables store in context..
 *
 * @see JProlContext#getGlobalVariablesStore()
 * @since 3.0.0
 */
public interface KeyValueTermStore {

  /**
   * Remove all values marked by predicate.
   *
   * @param predicate it gets key and value as input and if true then the value will be removed from store.
   */
  void removeif(final BiPredicate<String, Term> predicate);

  /**
   * Find value for its name.
   *
   * @param name value name, must not be null
   * @return found value or empty optional
   */
  Optional<Term> findValue(final String name);

  /**
   * Set value for its name.
   *
   * @param name  name of value, must not be null
   * @param value term as mapped value, must not be null, copy must be saved.
   */
  void setValue(final String name, final Term value);

  /**
   * Remove value for its name if presented.
   *
   * @param name name of value, must not be null
   */
  void remove(final String name);

  /**
   * Size of store.
   *
   * @return size
   */
  long size();

  /**
   * Check that store is empty.
   *
   * @return true if store is empty, false otherwise
   */
  boolean isEmpty();

}
