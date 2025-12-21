package com.igormaznitsa.jprol.utils;

import static java.util.Collections.emptyList;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.function.Supplier;

/**
 * The container allows to organize a lazy set collection where the main set object will not be allocated if there is not write operations.
 * <b>NOT THREADSAFE!</b>
 *
 * @param <V> value type
 * @since 2.3.0
 */
public final class LazySet<V> implements Set<V> {

  private static final Object[] EMPTY_ARRAY = new Object[0];
  private final Supplier<Set<V>> supplier;
  private Set<V> set;

  public LazySet() {
    this(HashSet::new);
  }

  public LazySet(final Supplier<Set<V>> supplier) {
    this.supplier = supplier;
  }

  @Override
  public int size() {
    return this.set == null ? 0 : this.set.size();
  }

  @Override
  public boolean isEmpty() {
    return this.set == null || this.set.isEmpty();
  }

  @Override
  public boolean contains(final Object o) {
    return this.set != null && this.set.contains(o);
  }

  @Override
  public Iterator<V> iterator() {
    return this.set == null ? Collections.emptyIterator() : this.set.iterator();
  }

  @Override
  public Object[] toArray() {
    return this.set == null ? EMPTY_ARRAY : this.set.toArray();
  }

  @Override
  public <T> T[] toArray(T[] a) {
    return this.set == null ? emptyList().toArray(a) : this.set.toArray(a);
  }

  @Override
  public boolean add(V v) {
    if (this.set == null) {
      this.set = this.supplier.get();
    }
    return this.set.add(v);
  }

  @Override
  public boolean remove(Object o) {
    return this.set != null && this.set.remove(o);
  }

  @Override
  public boolean containsAll(final Collection<?> c) {
    return this.set == null ? c.isEmpty() : this.set.contains(c);
  }

  @Override
  public boolean addAll(Collection<? extends V> c) {
    if (this.set == null) {
      this.set = this.supplier.get();
    }
    return this.set.addAll(c);
  }

  @Override
  public boolean retainAll(Collection<?> c) {
    return this.set != null && this.set.retainAll(c);
  }

  @Override
  public boolean removeAll(Collection<?> c) {
    return this.set != null && this.set.removeAll(c);
  }

  @Override
  public void clear() {
    if (this.set != null) {
      this.set.clear();
    }
  }

  public boolean isInstantiated() {
    return this.set.isEmpty();
  }

  public Set<V> getSet(final boolean forceSupply) {
    if (this.set == null) {
      if (forceSupply) {
        this.set = this.supplier.get();
      }
    }
    return this.set == null ? Set.of() : this.set;
  }

  @Override
  public String toString() {
    return this.set == null ? Set.of().toString() : this.set.toString();
  }

}
