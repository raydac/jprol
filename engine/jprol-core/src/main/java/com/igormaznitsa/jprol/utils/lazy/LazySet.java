package com.igormaznitsa.jprol.utils.lazy;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * The container allows to organize a lazy set collection where the main set object will not be allocated if there is not write operations.
 * <b>NOT THREADSAFE!</b>
 *
 * @param <V> value type
 * @since 3.0.0
 */
public final class LazySet<V> implements Set<V> {

  private static final Object[] EMPTY_ARRAY = new Object[0];

  private final Supplier<Set<V>> supplier;
  private Set<V> set;

  public LazySet() {
    this(HashSet::new);
  }

  public LazySet(final Collection<V> collection) {
    this(HashSet::new);
    if (!collection.isEmpty()) {
      this.set = this.supplier.get();
      this.set.addAll(collection);
    }
  }

  public LazySet(final Supplier<Set<V>> supplier) {
    this.supplier = supplier;
  }

  @Override
  public Spliterator<V> spliterator() {
    return this.set == null ? Spliterators.emptySpliterator() : this.set.spliterator();
  }

  public <T> T[] toArray(final IntFunction<T[]> generator) {
    // such approach for Android 32 which has non-correct API implementation
    if (this.set == null) {
      return generator.apply(0);
    } else {
      return this.set.toArray(generator.apply(this.set.size()));
    }
  }

  @Override
  public boolean removeIf(Predicate<? super V> filter) {
    return this.set != null && this.set.removeIf(filter);
  }

  @Override
  public Stream<V> stream() {
    return this.set == null ? Stream.empty() : this.set.stream();
  }

  @Override
  public Stream<V> parallelStream() {
    return this.set == null ? Stream.empty() : this.set.parallelStream();
  }

  @Override
  public void forEach(Consumer<? super V> action) {
    if (this.set != null) {
      this.set.forEach(action);
    }
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

  @SuppressWarnings("unchecked")
  @Override
  public <T> T[] toArray(final T[] a) {
    if (set == null) {
      if (a.length == 0) {
        return a;
      }
      return Arrays.copyOf(a, 0);
    } else {
      return this.set.toArray(a);
    }
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
      this.set = null;
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
