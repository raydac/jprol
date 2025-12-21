package com.igormaznitsa.jprol.utils;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;

/**
 * The container allows to organize a lazy map collection where the main map object will not be allocated if there is not write operations.
 * <b>NOT THREADSAFE!</b>
 *
 * @param <K> key type
 * @param <V> value type
 * @since 2.3.0
 */
public final class LazyMap<K, V> implements Map<K, V> {
  private final Supplier<Map<K, V>> supplier;
  private Map<K, V> map;

  public LazyMap() {
    this(HashMap::new);
  }

  public LazyMap(final Supplier<Map<K, V>> supplier) {
    this.supplier = supplier;
  }

  @Override
  public int size() {
    return this.map == null ? 0 : this.map.size();
  }

  @Override
  public boolean isEmpty() {
    return this.map == null || this.map.isEmpty();
  }

  @Override
  public boolean containsKey(final Object key) {
    return this.map != null && this.map.containsKey(key);
  }

  @Override
  public boolean containsValue(final Object value) {
    return this.map != null && this.map.containsValue(value);
  }

  @Override
  public V get(final Object key) {
    return this.map == null ? null : this.map.get(key);
  }

  @Override
  public V put(K key, V value) {
    if (this.map == null) {
      this.map = this.supplier.get();
    }
    return this.map.put(key, value);
  }

  @Override
  public V remove(Object key) {
    return this.map == null ? null : this.map.remove(key);
  }

  @Override
  public void putAll(Map<? extends K, ? extends V> m) {
    if (this.map == null) {
      this.map = this.supplier.get();
    }
    this.map.putAll(m);
  }

  @Override
  public void clear() {
    if (this.map != null) {
      this.map.clear();
    }
  }

  @Override
  public Set<K> keySet() {
    return this.map == null ? Set.of() : this.map.keySet();
  }

  @Override
  public Collection<V> values() {
    return this.map == null ? List.of() : this.map.values();
  }

  @Override
  public Set<Entry<K, V>> entrySet() {
    return this.map == null ? Set.of() : this.map.entrySet();
  }

  public boolean isInstantiated() {
    return this.map.isEmpty();
  }

  public Map<K, V> getMap(final boolean forceSupply) {
    if (this.map == null) {
      if (forceSupply) {
        this.map = this.supplier.get();
      }
    }
    return this.map == null ? Map.of() : this.map;
  }

  @Override
  public String toString() {
    return this.map == null ? Map.of().toString() : this.map.toString();
  }
}
