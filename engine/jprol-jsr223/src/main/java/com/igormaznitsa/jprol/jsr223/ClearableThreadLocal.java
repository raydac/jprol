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

  ClearableThreadLocal(Supplier<T> supplier) {
    this.supplier = Objects.requireNonNull(supplier);
  }

  Long getThreadId() {
    return Thread.currentThread().getId();
  }

  T set(final T value) {
    return this.threadMap.put(this.getThreadId(), requireNonNull(value));
  }

  T remove() {
    return this.threadMap.remove(this.getThreadId());
  }

  T get() {
    return this.threadMap.computeIfAbsent(this.getThreadId(), x -> this.supplier.get());
  }

  T find() {
    return this.threadMap.get(this.getThreadId());
  }

  void removeAll(final Consumer<T> action) {
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
}
