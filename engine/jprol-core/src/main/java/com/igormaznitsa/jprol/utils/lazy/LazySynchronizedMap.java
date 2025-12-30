package com.igormaznitsa.jprol.utils.lazy;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * The container allows to organize a lazy map collection where the main map object will not be allocated if there is not write operations.
 * It is thread safe and appropriate to wrap a concurrent maps.
 *
 * @param <K> key type
 * @param <V> value type
 * @since 3.0.0
 */
public class LazySynchronizedMap<K, V> implements Map<K, V> {
  private final Supplier<Map<K, V>> supplier;
  private final ReentrantLock locker = new ReentrantLock();
  private Map<K, V> map;

  public LazySynchronizedMap() {
    this(HashMap::new);
  }

  public LazySynchronizedMap(final Supplier<Map<K, V>> supplier) {
    this.supplier = supplier;
  }

  @Override
  public void forEach(final BiConsumer<? super K, ? super V> action) {
    this.locker.lock();
    try {
      if (this.map != null) {
        this.map.forEach(action);
      }
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public void replaceAll(final BiFunction<? super K, ? super V, ? extends V> function) {
    this.locker.lock();
    try {
      if (this.map != null) {
        this.map.replaceAll(function);
      }
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V getOrDefault(Object key, V defaultValue) {
    this.locker.lock();
    try {
      return this.map == null ? defaultValue : this.map.getOrDefault(key, defaultValue);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V putIfAbsent(final K key, final V value) {
    this.locker.lock();
    try {
      if (this.map == null) {
        this.map = this.supplier.get();
      }
      return this.map.putIfAbsent(key, value);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public boolean remove(Object key, Object value) {
    this.locker.lock();
    try {
      return this.map != null && this.map.remove(key, value);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public boolean replace(K key, V oldValue, V newValue) {
    return Map.super.replace(key, oldValue, newValue);
  }

  @Override
  public V replace(final K key, final V value) {
    this.locker.lock();
    try {
      return this.map == null ? null : this.map.replace(key, value);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V computeIfAbsent(final K key, final Function<? super K, ? extends V> mappingFunction) {
    this.locker.lock();
    try {
      if (this.map == null) {
        this.map = this.supplier.get();
      }
      return this.map.computeIfAbsent(key, mappingFunction);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V computeIfPresent(final K key,
                            final BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
    this.locker.lock();
    try {
      return this.map == null ? null : this.map.computeIfPresent(key, remappingFunction);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V compute(final K key,
                   final BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
    this.locker.lock();
    try {
      if (this.map == null) {
        this.map = this.supplier.get();
      }
      return this.map.compute(key, remappingFunction);
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public V merge(final K key, final V value,
                 final BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
    this.locker.lock();
    try {
      if (this.map == null) {
        this.map = this.supplier.get();
      }
      return this.map.merge(key, value, remappingFunction);
    } finally {
      this.locker.unlock();
    }
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
    if (m.isEmpty()) {
      return;
    }
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
        this.map = null;
      }
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public Set<K> keySet() {
    this.locker.lock();
    try {
      return this.map == null ? Set.of() : new HashSet<>(this.map.keySet());
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public Collection<V> values() {
    this.locker.lock();
    try {
      return this.map == null ? List.of() : new ArrayList<>(this.map.values());
    } finally {
      this.locker.unlock();
    }
  }

  @Override
  public Set<Map.Entry<K, V>> entrySet() {
    this.locker.lock();
    try {
      return this.map == null ? Set.of() : new HashSet<>(this.map.entrySet());
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
