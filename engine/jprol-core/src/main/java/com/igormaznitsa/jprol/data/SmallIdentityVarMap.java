package com.igormaznitsa.jprol.data;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

final class SmallIdentityVarMap extends AbstractMap<TermVar, TermVar> {

  private static final int INITIAL_PAIRS_LENGTH = 16;
  private static final int MAX_PAIRS_LENGTH = 32;

  private TermVar[] pairs;
  private int size;
  private IdentityHashMap<TermVar, TermVar> overflow;

  @Override
  public int size() {
    return this.overflow != null ? this.overflow.size() : this.size;
  }

  @Override
  public boolean isEmpty() {
    return this.size() == 0;
  }

  @Override
  public boolean containsKey(final Object key) {
    return this.get(key) != null;
  }

  @Override
  public TermVar get(final Object key) {
    if (this.overflow != null) {
      return this.overflow.get(key);
    }
    final TermVar[] localPairs = this.pairs;
    if (localPairs == null) {
      return null;
    }
    final int limit = this.size << 1;
    for (int i = 0; i < limit; i += 2) {
      if (localPairs[i] == key) {
        return localPairs[i + 1];
      }
    }
    return null;
  }

  @Override
  public TermVar put(final TermVar key, final TermVar value) {
    if (this.overflow != null) {
      return this.overflow.put(key, value);
    }
    TermVar[] localPairs = this.pairs;
    if (localPairs == null) {
      localPairs = new TermVar[INITIAL_PAIRS_LENGTH];
      localPairs[0] = key;
      localPairs[1] = value;
      this.pairs = localPairs;
      this.size = 1;
      return null;
    }

    final int limit = this.size << 1;
    for (int i = 0; i < limit; i += 2) {
      if (localPairs[i] == key) {
        final TermVar previous = localPairs[i + 1];
        localPairs[i + 1] = value;
        return previous;
      }
    }

    if (limit == localPairs.length) {
      if (localPairs.length < MAX_PAIRS_LENGTH) {
        localPairs = Arrays.copyOf(localPairs, MAX_PAIRS_LENGTH);
        this.pairs = localPairs;
      } else {
        final IdentityHashMap<TermVar, TermVar> promoted = new IdentityHashMap<>(32);
        for (int i = 0; i < limit; i += 2) {
          promoted.put(localPairs[i], localPairs[i + 1]);
        }
        this.pairs = null;
        this.overflow = promoted;
        return promoted.put(key, value);
      }
    }

    localPairs[limit] = key;
    localPairs[limit + 1] = value;
    this.size++;
    return null;
  }

  @Override
  public void clear() {
    if (this.overflow != null) {
      this.overflow.clear();
      this.overflow = null;
    }
    this.pairs = null;
    this.size = 0;
  }

  @Override
  public Set<Entry<TermVar, TermVar>> entrySet() {
    if (this.overflow != null) {
      return this.overflow.entrySet();
    }
    return new FlatEntrySet(this);
  }

  private static final class FlatEntrySet extends AbstractSet<Entry<TermVar, TermVar>> {

    private final SmallIdentityVarMap owner;

    private FlatEntrySet(final SmallIdentityVarMap owner) {
      this.owner = owner;
    }

    @Override
    public Iterator<Entry<TermVar, TermVar>> iterator() {
      return new FlatEntryIterator(this.owner);
    }

    @Override
    public int size() {
      return this.owner.size;
    }
  }

  private static final class FlatEntryIterator implements Iterator<Entry<TermVar, TermVar>> {

    private final SmallIdentityVarMap owner;
    private int cursor;

    private FlatEntryIterator(final SmallIdentityVarMap owner) {
      this.owner = owner;
    }

    @Override
    public boolean hasNext() {
      return this.cursor < this.owner.size;
    }

    @Override
    public Entry<TermVar, TermVar> next() {
      final TermVar[] activePairs = this.owner.pairs;
      if (activePairs == null || this.cursor >= this.owner.size) {
        throw new NoSuchElementException();
      }
      final int offset = this.cursor++ << 1;
      return new SimpleImmutableEntry<>(activePairs[offset], activePairs[offset + 1]);
    }
  }
}
