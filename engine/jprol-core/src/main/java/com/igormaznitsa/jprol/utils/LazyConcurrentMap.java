package com.igormaznitsa.jprol.utils;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;

/**
 * The container allows to organize a lazy map collection where the main map object will not be allocated if there is not write operations.
 * It is thread safe and appropriate to wrap a concurrent maps.
 *
 * @param <K> key type
 * @param <V> value type
 * @since 2.3.0
 */
public class LazyConcurrentMap<K, V> implements Map<K, V> {
  private final Supplier<Map<K, V>> supplier;
  private final ReentrantLock locker = new ReentrantLock();
  private Map<K, V> map;

  public LazyConcurrentMap() {
    this(HashMap::new);
  }

  public LazyConcurrentMap(final Supplier<Map<K, V>> supplier) {
    this.supplier = supplier;
  }

  @Override
  public int size() {
    this.locker.lock();
    try {
      return this.map == null ? 0 : this.map.size();
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public boolean isEmpty() {
    this.locker.lock();
    try {
      return this.map == null || this.map.isEmpty();
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public boolean containsKey(final Object key) {
    this.locker.lock();
    try {
      return this.map != null && this.map.containsKey(key);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public boolean containsValue(final Object value) {
    this.locker.lock();
    try {
      return this.map != null && this.map.containsValue(value);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V get(final Object key) {
    this.locker.lock();
    try {
      return this.map == null ? null : this.map.get(key);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V put(K key, V value) {
    this.locker.lock();
    try {
      if (this.map == null) {
        this.map = this.supplier.get();
      }
      return this.map.put(key, value);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V remove(Object key) {
    this.locker.lock();
    try {
      return this.map == null ? null : this.map.remove(key);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public void putAll(Map<? extends K, ? extends V> m) {
    this.locker.lock();
    try {
      if (this.map == null) {
        this.map = this.supplier.get();
      }
      this.map.putAll(m);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public void clear() {
    this.locker.lock();
    try {
      if (this.map != null) {
        this.map.clear();
      }
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public Set<K> keySet() {
    this.locker.lock();
    try {
      return this.map == null ? Set.of() : this.map.keySet();
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public Collection<V> values() {
    this.locker.lock();
    try {
      return this.map == null ? List.of() : this.map.values();
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public Set<Map.Entry<K, V>> entrySet() {
    this.locker.lock();
    try {
      return this.map == null ? Set.of() : this.map.entrySet();
    } finally {
      this.locker.unlock();
    }
  }

  public boolean isInstantiated() {
    this.locker.lock();
    try {
      return this.map.isEmpty();
    } finally {
      this.locker.unlock();
    }
  }

  public Map<K, V> getMap(final boolean forceSupply) {
    this.locker.lock();
    try {
      if (this.map == null) {
        if (forceSupply) {
          this.map = this.supplier.get();
        }
      }
      return this.map == null ? Map.of() : this.map;
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public String toString() {
    this.locker.lock();
    try {
      return this.map == null ? Map.of().toString() : this.map.toString();
    } finally {
      this.locker.unlock();
    }
  }

}
