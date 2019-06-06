package com.igormaznitsa.prol.script;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.utils.Utils;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import javax.script.Bindings;

final class JProlBindings implements Bindings {

  private final Map<String, Term> internalMap = new HashMap<>();
  private final JProlScriptContext context;

  private static final class ConvertedEntry implements Map.Entry<String, Object> {

    private final String key;
    private final Object value;

    ConvertedEntry(final String key, final Object value) {
      this.key = key;
      this.value = value;
    }

    @Override
    public String getKey() {
      return this.key;
    }

    @Override
    public Object getValue() {
      return this.value;
    }

    @Override
    public Object setValue(final Object value) {
      throw new UnsupportedOperationException("Unsupported operation");
    }
  }

  Map<String, Term> getTermMap() {
    return this.internalMap;
  }

  void fillByValues(final Map<String, Term> instantiatedVariables) {
    instantiatedVariables.entrySet().forEach(e -> {
      this.internalMap.putAll(instantiatedVariables);
    });
  }

  public JProlBindings(final JProlScriptContext context) {
    this.context = context;
  }

  private void assertPrologName(final String name) {
    if (!JProlScriptUtils.isValidVarName(name)) {
      throw new IllegalArgumentException("Must be valid Prolog non-anonymous variable name: " + name);
    }
  }

  @Override
  public Object put(final String name, final Object value) {
    assertPrologName(name);
    final Term converted = value instanceof Term ? (Term) value : Utils.obj2term(value);
    final Term prev = this.internalMap.put(name, converted);
    return prev == null ? null : Utils.term2obj(this.context.getProlContext(), prev);
  }

  @Override
  public void putAll(Map<? extends String, ? extends Object> toMerge) {
    toMerge.entrySet().forEach(e -> {
      this.put(e.getKey(), e.getValue());
    });
  }

  @Override
  public boolean containsKey(final Object key) {
    assertPrologName(String.valueOf(key));
    return this.internalMap.containsKey(key);
  }

  @Override
  public Object get(final Object key) {
    assertPrologName(String.valueOf(key));
    final Term theTerm = this.internalMap.get(key);
    return theTerm == null ? null : Utils.term2obj(this.context.getProlContext(), theTerm);
  }

  @Override
  public Object remove(final Object key) {
    final Object result = this.get(key);
    this.internalMap.remove(key);
    return result;
  }

  @Override
  public int size() {
    return this.internalMap.size();
  }

  @Override
  public boolean isEmpty() {
    return this.internalMap.isEmpty();
  }

  @Override
  public boolean containsValue(final Object value) {
    if (value == null) {
      throw new NullPointerException("Value is null");
    }
    final Term theTerm = value instanceof Term ? (Term) value : Utils.obj2term(value);
    return this.internalMap.containsValue(theTerm);
  }

  @Override
  public void clear() {
    this.internalMap.clear();
  }

  @Override
  public Set<String> keySet() {
    return this.internalMap.keySet();
  }

  @Override
  public Collection<Object> values() {
    return this.internalMap.values().stream().map(x -> Utils.term2obj(this.context.getProlContext(), x)).collect(Collectors.toList());
  }

  @Override
  public Set<Entry<String, Object>> entrySet() {
    return this.internalMap.entrySet()
            .stream()
            .map(x -> new ConvertedEntry(x.getKey(), Utils.term2obj(this.context.getProlContext(), x.getValue())))
            .collect(Collectors.toSet());
  }

}
