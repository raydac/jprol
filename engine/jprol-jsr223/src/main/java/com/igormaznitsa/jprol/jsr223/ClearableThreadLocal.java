package com.igormaznitsa.jprol.jsr223;

import static java.util.Objects.requireNonNull;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class ClearableThreadLocal<T> implements CanGC {

  final Map<Long, Map.Entry<WeakReference<Thread>, T>> threadMap = new ConcurrentHashMap<>();

  private final Supplier<T> supplier;
  private final Consumer<T> removeAction;

  public ClearableThreadLocal(final Supplier<T> supplier, final Consumer<T> removeAction) {
    this.supplier = Objects.requireNonNull(supplier);
    this.removeAction = removeAction;
  }

  @Override
  public void gc() {
    final List<Long> collected = this.threadMap.entrySet().stream()
        .filter(x -> {
          final Thread thread = x.getValue().getKey().get();
          return thread == null || thread.getState() == Thread.State.TERMINATED;
        })
        .map(Map.Entry::getKey)
        .collect(Collectors.toList());
    collected.forEach(x -> {
      final Map.Entry<WeakReference<Thread>, T> removed = this.threadMap.remove(x);
      if (removed != null) {
        if (this.removeAction != null) {
          try {
            this.removeAction.accept(removed.getValue());
          } catch (Exception ex) {
            // ignore
          }
        }
      }
    });
  }

  protected Long getCurrentThreadId() {
    return Thread.currentThread().getId();
  }

  public T set(final T value) {
    final Map.Entry<WeakReference<Thread>, T> result = this.threadMap.put(this.getCurrentThreadId(),
        Map.entry(new WeakReference<>(Thread.currentThread()), requireNonNull(value)));
    return result == null ? null : result.getValue();
  }

  public T remove() {
    final Map.Entry<WeakReference<Thread>, T> result =
        this.threadMap.remove(this.getCurrentThreadId());
    if (result != null && this.removeAction != null) {
      try {
        this.removeAction.accept(result.getValue());
      } catch (Exception ex) {
        // ignore
      }
    }
    return result == null ? null : result.getValue();
  }

  public T get() {
    final Map.Entry<WeakReference<Thread>, T> result =
        this.threadMap.computeIfAbsent(this.getCurrentThreadId(),
            x -> Map.entry(new WeakReference<>(Thread.currentThread()), this.supplier.get()));
    return result.getValue();
  }

  public T find() {
    final Map.Entry<WeakReference<Thread>, T> result =
        this.threadMap.get(this.getCurrentThreadId());
    return result == null ? null : result.getValue();
  }

  public void removeAll() {
    Map<Long, Map.Entry<WeakReference<Thread>, T>> copy =
        this.removeAction == null ? Map.of() : Map.copyOf(this.threadMap);
    this.threadMap.clear();
    if (this.removeAction != null) {
      copy.forEach((key, pair) -> {
        try {
          this.removeAction.accept(pair.getValue());
        } catch (Exception ex) {
          // ignore
        }
      });
    }
  }

  public int size() {
    return this.threadMap.size();
  }
}
