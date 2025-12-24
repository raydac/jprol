package com.igormaznitsa.jprol.utils.lazy;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.function.IntFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

/**
 * The container allows to organize a lazy list collection where the main list object will not be allocated if there is not write operations.
 * <b>NOT THREADSAFE!</b>
 *
 * @param <V> value type
 * @since 3.0.0
 */
public final class LazyList<V> implements List<V> {

  private static final Object[] EMPTY_ARRAY = new Object[0];

  private final Supplier<List<V>> supplier;
  private List<V> list;

  public LazyList() {
    this(ArrayList::new);
  }

  public LazyList(final Collection<V> collection) {
    this(ArrayList::new);
    if (!collection.isEmpty()) {
      this.list = this.supplier.get();
      this.list.addAll(collection);
    }
  }

  public LazyList(final Supplier<List<V>> supplier) {
    this.supplier = supplier;
  }

  private static void rangeCheckForZero(final int index) {
    if (index != 0) {
      throwIndexErrorForEmpty(index);
    }
  }

  private static void throwIndexErrorForEmpty(final int index) {
    throw new IndexOutOfBoundsException("Index: " + index + ", Size: 0");
  }

  @Override
  public boolean addAll(final int index, final Collection<? extends V> c) {
    if (this.list == null) {
      rangeCheckForZero(index);
      if (c.isEmpty()) {
        return false;
      }
      this.list = this.supplier.get();
    }
    return this.list.addAll(index, c);
  }

  @Override
  public V get(final int index) {
    if (this.list == null) {
      throwIndexErrorForEmpty(index);
    }
    return this.list.get(index);
  }

  @Override
  public V set(final int index, final V element) {
    if (this.list == null) {
      this.list = this.supplier.get();
    }
    return this.list.set(index, element);
  }

  @Override
  public void add(int index, V element) {
    if (this.list == null) {
      this.list = this.supplier.get();
    }
    this.list.add(index, element);
  }

  @Override
  public V remove(final int index) {
    if (this.list == null) {
      throwIndexErrorForEmpty(index);
    }
    return this.list.remove(index);
  }

  @Override
  public int indexOf(final Object o) {
    if (this.list == null) {
      Objects.requireNonNull(o);
      return -1;
    }
    return this.list.indexOf(o);
  }

  @Override
  public int lastIndexOf(Object o) {
    if (this.list == null) {
      Objects.requireNonNull(o);
      return -1;
    }
    return this.list.lastIndexOf(o);
  }

  @Override
  public ListIterator<V> listIterator() {
    if (this.list == null) {
      return Collections.emptyListIterator();
    }
    return this.list.listIterator();
  }

  @Override
  public ListIterator<V> listIterator(final int index) {
    if (this.list == null) {
      rangeCheckForZero(index);
      return Collections.emptyListIterator();
    }
    return this.list.listIterator(index);
  }

  @Override
  public List<V> subList(int fromIndex, int toIndex) {
    if (this.list == null) {
      if (fromIndex != 0) {
        throwIndexErrorForEmpty(fromIndex);
      }
      if (toIndex != 0) {
        throwIndexErrorForEmpty(fromIndex);
      }
      return List.of();
    }
    return this.subList(fromIndex, toIndex);
  }

  @Override
  public Spliterator<V> spliterator() {
    return this.list == null ? Spliterators.emptySpliterator() : this.list.spliterator();
  }

  public <T> T[] toArray(final IntFunction<T[]> generator) {
    // such approach for Android 32 which has non-correct API implementation
    if (this.list == null) {
      return generator.apply(0);
    } else {
      return this.list.toArray(generator.apply(this.list.size()));
    }
  }

  @Override
  public boolean removeIf(Predicate<? super V> filter) {
    return this.list != null && this.list.removeIf(filter);
  }

  @Override
  public Stream<V> stream() {
    return this.list == null ? Stream.empty() : this.list.stream();
  }

  @Override
  public Stream<V> parallelStream() {
    return this.list == null ? Stream.empty() : this.list.parallelStream();
  }

  @Override
  public void forEach(Consumer<? super V> action) {
    if (this.list != null) {
      this.list.forEach(action);
    }
  }

  @Override
  public int size() {
    return this.list == null ? 0 : this.list.size();
  }

  @Override
  public boolean isEmpty() {
    return this.list == null || this.list.isEmpty();
  }

  @Override
  public boolean contains(final Object o) {
    return this.list != null && this.list.contains(o);
  }

  @Override
  public Iterator<V> iterator() {
    return this.list == null ? Collections.emptyIterator() : this.list.iterator();
  }

  @Override
  public Object[] toArray() {
    return this.list == null ? EMPTY_ARRAY : this.list.toArray();
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T[] toArray(final T[] a) {
    if (list == null) {
      if (a.length == 0) {
        return a;
      }
      return Arrays.copyOf(a, 0);
    } else {
      return this.list.toArray(a);
    }
  }

  @Override
  public boolean add(V v) {
    if (this.list == null) {
      this.list = this.supplier.get();
    }
    return this.list.add(v);
  }

  @Override
  public boolean remove(Object o) {
    return this.list != null && this.list.remove(o);
  }

  @Override
  public boolean containsAll(final Collection<?> c) {
    return this.list == null ? c.isEmpty() : this.list.contains(c);
  }

  @Override
  public boolean addAll(Collection<? extends V> c) {
    if (this.list == null) {
      this.list = this.supplier.get();
    }
    return this.list.addAll(c);
  }

  @Override
  public boolean retainAll(Collection<?> c) {
    return this.list != null && this.list.retainAll(c);
  }

  @Override
  public boolean removeAll(Collection<?> c) {
    return this.list != null && this.list.removeAll(c);
  }

  @Override
  public void clear() {
    if (this.list != null) {
      this.list.clear();
      this.list = null;
    }
  }

  public boolean isInstantiated() {
    return this.list.isEmpty();
  }

  public List<V> getList(final boolean forceSupply) {
    if (this.list == null) {
      if (forceSupply) {
        this.list = this.supplier.get();
      }
    }
    return this.list == null ? List.of() : this.list;
  }

  @Override
  public String toString() {
    return this.list == null ? List.of().toString() : this.list.toString();
  }

  @Override
  public void replaceAll(final UnaryOperator<V> operator) {
    if (this.list != null) {
      this.list.replaceAll(operator);
    }
  }

  @Override
  public void sort(Comparator<? super V> c) {
    if (this.list != null) {
      this.list.sort(c);
    }
  }

}
