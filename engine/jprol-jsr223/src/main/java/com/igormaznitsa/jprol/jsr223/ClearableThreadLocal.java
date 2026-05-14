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

/**
 * Thread-associated values keyed by {@linkplain Thread#getId() thread id}, with optional cleanup when threads end.
 * Used internally by {@link JProlScriptEngineContext} for per-thread {@link javax.script.Bindings} and {@link com.igormaznitsa.jprol.logic.JProlContext}.
 *
 * @param <T> value type stored per thread
 */
public class ClearableThreadLocal<T> implements CanGC {

  /**
   * Internal map from {@linkplain #getCurrentThreadId() thread id} to weak thread reference and value.
   */
  final Map<Long, Map.Entry<WeakReference<Thread>, T>> threadMap = new ConcurrentHashMap<>();

  private final Supplier<T> supplier;
  private final Consumer<T> removeAction;

  /**
   * @param supplier     factory for new values when {@link #get()} is first called on a thread
   * @param removeAction applied when a value is {@linkplain #remove() removed}, {@linkplain #removeAll() cleared}, or
   *                     collected during {@link #gc()}; may be {@code null}
   */
  public ClearableThreadLocal(final Supplier<T> supplier, final Consumer<T> removeAction) {
    this.supplier = Objects.requireNonNull(supplier);
    this.removeAction = removeAction;
  }

  /**
   * Removes entries for dead or terminated threads and runs {@code removeAction} on evicted values.
   */
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

  /**
   * Thread id used as map key; override only for tests or custom threading.
   *
   * @return {@link Thread#getId()} for the current thread
   */
  protected Long getCurrentThreadId() {
    return Thread.currentThread().getId();
  }

  /**
   * Associates {@code value} with the current thread, replacing any previous value.
   *
   * @return the previous value or {@code null}
   */
  public T set(final T value) {
    final Map.Entry<WeakReference<Thread>, T> result = this.threadMap.put(this.getCurrentThreadId(),
        Map.entry(new WeakReference<>(Thread.currentThread()), requireNonNull(value)));
    return result == null ? null : result.getValue();
  }

  /**
   * Removes the current thread's value and runs {@code removeAction} if configured.
   *
   * @return the removed value or {@code null}
   */
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

  /**
   * Returns the current thread's value, creating it with {@code supplier} if absent.
   */
  public T get() {
    final Map.Entry<WeakReference<Thread>, T> result =
        this.threadMap.computeIfAbsent(this.getCurrentThreadId(),
            x -> Map.entry(new WeakReference<>(Thread.currentThread()), this.supplier.get()));
    return result.getValue();
  }

  /**
   * Returns the current thread's value without creating one.
   *
   * @return value or {@code null}
   */
  public T find() {
    final Map.Entry<WeakReference<Thread>, T> result =
        this.threadMap.get(this.getCurrentThreadId());
    return result == null ? null : result.getValue();
  }

  /**
   * Clears all thread entries and runs {@code removeAction} on each removed value (if configured).
   */
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

  /**
   * @return number of threads currently holding a value
   */
  public int size() {
    return this.threadMap.size();
  }
}
