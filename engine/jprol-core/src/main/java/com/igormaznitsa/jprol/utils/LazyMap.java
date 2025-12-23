package com.igormaznitsa.jprol.utils;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * The container allows to organize a lazy map collection where the main map object will not be allocated if there is not write operations.
 * <b>NOT THREADSAFE!</b>
 *
 * @param <K> key type
 * @param <V> value type
 * @since 3.0.0
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
  public V computeIfPresent(K key,
                            BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
    return this.map == null ? null : this.map.computeIfPresent(key, remappingFunction);
  }

  @Override
  public V getOrDefault(final Object key, final V defaultValue) {
    return this.map == null ? defaultValue : this.map.getOrDefault(key, defaultValue);
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
  public void forEach(final BiConsumer<? super K, ? super V> action) {
    if (this.map != null) {
      this.map.forEach(action);
    }
  }

  @Override
  public void replaceAll(final BiFunction<? super K, ? super V, ? extends V> function) {
    if (this.map != null) {
      this.map.replaceAll(function);
    }
  }

  @Override
  public V putIfAbsent(final K key, final V value) {
    if (this.map == null) {
      this.map = this.supplier.get();
    }
    return this.map.putIfAbsent(key, value);
  }

  @Override
  public boolean remove(final Object key, final Object value) {
    return this.map != null && this.map.remove(key, value);
  }

  @Override
  public boolean replace(final K key, final V oldValue, final V newValue) {
    return this.map != null && this.map.replace(key, oldValue, newValue);
  }

  @Override
  public V replace(final K key, final V value) {
    return this.map == null ? null : this.map.replace(key, value);
  }

  @Override
  public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
    if (this.map == null) {
      this.map = this.supplier.get();
    }
    return this.map.computeIfAbsent(key, mappingFunction);
  }

  @Override
  public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
    if (this.map == null) {
      this.map = this.supplier.get();
    }
    return this.map.compute(key, remappingFunction);
  }

  @Override
  public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
    if (this.map == null) {
      this.map = this.supplier.get();
    }
    return this.map.merge(key, value, remappingFunction);
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
