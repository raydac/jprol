package com.igormaznitsa.jprol.utils.lazy;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.stream.LongStream;

/**
 * Special case of long value set where on start we use just linear array of elements.
 * Also it allows to avoid some boxing on small sets of long values.
 */
public final class LinearLongSet implements Set<Long> {

  public static final int LINEAR_CAPACITY = 16;

  private final long[] array = new long[LINEAR_CAPACITY];
  private int size = 0;
  private Set<Long> longSet;

  public LinearLongSet() {

  }

  public boolean contains(final long value) {
    if (this.longSet == null) {
      for (int i = 0; i < this.size; i++) {
        if (this.array[i] == value) {
          return true;
        }
      }
      return false;
    } else {
      return this.longSet.contains(value);
    }
  }

  @Override
  public int size() {
    if (this.longSet == null) {
      return this.size;
    } else {
      return this.longSet.size();
    }
  }

  @Override
  public boolean isEmpty() {
    if (this.longSet == null) {
      return this.size == 0;
    } else {
      return this.longSet.isEmpty();
    }
  }

  @Override
  public boolean contains(Object o) {
    if (o instanceof Number) {
      return this.contains(((Number) o).longValue());
    }
    return false;
  }

  @Override
  public Iterator<Long> iterator() {
    if (this.longSet == null) {
      return LongStream.of(this.array).limit(this.size).boxed().iterator();
    } else {
      return this.longSet.iterator();
    }
  }

  @Override
  public Object[] toArray() {
    if (this.longSet == null) {
      return LongStream.of(this.array).limit(this.size).boxed().toArray();
    } else {
      return this.longSet.toArray();
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T[] toArray(T[] a) {
    if (this.longSet == null) {
      return (T[]) LongStream.of(this.array).limit(this.size).boxed().toArray();
    } else {
      return (T[]) this.longSet.toArray();
    }
  }

  public boolean add(final long value) {
    if (this.longSet == null) {
      final int index = this.findIndex(value);
      if (index < 0) {
        if (this.size == LINEAR_CAPACITY) {
          this.array2set();
          return this.longSet.add(value);
        } else {
          this.array[this.size++] = value;
          return true;
        }
      } else {
        return false;
      }
    } else {
      return this.longSet.add(value);
    }
  }

  @Override
  public boolean add(final Long aLong) {
    return this.add(aLong.longValue());
  }

  private int findIndex(final long value) {
    int index = -1;
    for (int i = 0; i < this.size; i++) {
      if (this.array[i] == value) {
        index = i;
        break;
      }
    }
    return index;
  }

  private void set2array() {
    this.size = 0;
    this.longSet
        .forEach(x -> {
          this.array[this.size++] = x;
        });
    this.longSet = null;
  }

  private void array2set() {
    this.longSet = new HashSet<>(LINEAR_CAPACITY * 2);
    for (int i = 0; i < this.size; i++) {
      this.longSet.add(this.array[i]);
    }
    this.size = 0;
  }

  public boolean remove(final long value) {
    if (this.longSet == null) {
      final int index = this.findIndex(value);
      if (index >= 0) {
        int numMoved = size - index - 1;
        if (numMoved > 0) {
          System.arraycopy(array, index + 1, this.array, index, numMoved);
        }
        this.size--;
        return true;
      } else {
        return false;
      }
    } else {
      final boolean removed = this.longSet.remove(value);
      if (this.longSet.size() <= LINEAR_CAPACITY) {
        this.set2array();
      }
      return removed;
    }
  }

  @Override
  public boolean remove(final Object o) {
    if (o instanceof Number) {
      return this.remove(((Number) o).longValue());
    } else {
      return false;
    }
  }

  @Override
  public boolean containsAll(final Collection<?> c) {
    if (c.isEmpty()) {
      return true;
    }
    boolean contains = true;
    for (Object j : c) {
      contains &= this.contains(j);
    }
    return contains;
  }

  @Override
  public boolean addAll(final Collection<? extends Long> c) {
    boolean changed = false;
    for (Long i : c) {
      changed |= this.add(i);
    }
    return changed;
  }

  @Override
  public boolean retainAll(final Collection<?> c) {
    return false;
  }

  @Override
  public boolean removeAll(final Collection<?> c) {
    boolean changed = false;
    for (Object j : c) {
      changed |= this.remove(j);
    }
    return changed;
  }

  @Override
  public void clear() {
    this.longSet = null;
    this.size = 0;
  }
}
