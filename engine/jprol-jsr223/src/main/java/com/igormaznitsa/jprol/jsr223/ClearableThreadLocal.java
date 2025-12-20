package com.igormaznitsa.jprol.jsr223;

import static java.util.Objects.requireNonNull;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class ClearableThreadLocal<T> {

  final Map<Long, T> threadMap = new ConcurrentHashMap<>();

  private final Supplier<T> supplier;

  public ClearableThreadLocal(Supplier<T> supplier) {
    this.supplier = Objects.requireNonNull(supplier);
  }

  protected Long getCurrentThreadId() {
    return Thread.currentThread().getId();
  }

  public T set(final T value) {
    return this.threadMap.put(this.getCurrentThreadId(), requireNonNull(value));
  }

  public T remove() {
    return this.threadMap.remove(this.getCurrentThreadId());
  }

  public T get() {
    return this.threadMap.computeIfAbsent(this.getCurrentThreadId(), x -> this.supplier.get());
  }

  public T find() {
    return this.threadMap.get(this.getCurrentThreadId());
  }

  public void removeAll(final Consumer<T> action) {
    if (action != null) {
      this.threadMap.forEach((key, value) -> {
        try {
          action.accept(value);
        } catch (Exception ex) {
          // ignore
        }
      });
    }
    this.threadMap.clear();
  }

  public int size() {
    return this.threadMap.size();
  }
}
