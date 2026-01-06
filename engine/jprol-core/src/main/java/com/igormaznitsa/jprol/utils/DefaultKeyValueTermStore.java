package com.igormaznitsa.jprol.utils;

import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.logic.KeyValueTermStore;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiPredicate;
import java.util.function.Supplier;

/**
 * Default implementation of KeyValueTermStore. By default, it is thread safe, if custom store then it depends on provided map.
 *
 * @see JProlContext#getGlobalVariablesStore()
 * @since 3.0.0
 */
public class DefaultKeyValueTermStore implements KeyValueTermStore {

  private final String name;
  private final Map<String, Term> store;

  /**
   * Create instance. By default it uses thread safe internal store.
   *
   * @param name name of the instance, must not be null
   */
  public DefaultKeyValueTermStore(final String name) {
    this(name, ConcurrentHashMap::new);
  }

  /**
   * Create instance.
   *
   * @param name          name of the instance, must not be null
   * @param storeSupplier supplier of custom internal map based store.
   */
  public DefaultKeyValueTermStore(final String name,
                                  final Supplier<Map<String, Term>> storeSupplier) {
    this.name = requireNonNull(name);
    this.store = storeSupplier.get();
  }

  @Override
  public void removeif(final BiPredicate<String, Term> predicate) {
    final Set<String> forRemove = new HashSet<>();
    this.store.forEach((s, t) -> {
      if (predicate.test(s, t)) {
        forRemove.add(s);
      }
    });
    forRemove.forEach(this.store::remove);
  }

  public String getName() {
    return this.name;
  }

  @Override
  public Optional<Term> findValue(final String name) {
    final Term result = this.store.get(name);
    if (result == null) {
      return Optional.empty();
    }
    return Optional.of(result.makeClone());
  }

  @Override
  public void setValue(final String name, final Term value) {
    this.store.put(requireNonNull(name, "Name must not be null"),
        requireNonNull(value, "Value must not be null").makeClone());
  }

  @Override
  public void remove(final String name) {
    this.store.remove(requireNonNull(name, "Name must not be null"));
  }

  @Override
  public long size() {
    return this.store.size();
  }

  @Override
  public boolean isEmpty() {
    return this.store.isEmpty();
  }

  public Map<String, Term> getMap() {
    return this.store;
  }

}
