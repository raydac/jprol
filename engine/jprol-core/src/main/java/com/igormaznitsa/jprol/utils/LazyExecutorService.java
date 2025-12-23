package com.igormaznitsa.jprol.utils;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;

/**
 * Allows to create a deferred executor service which will be init only during first task.
 * It is a thread safe one.
 *
 * @since 3.0.0
 */
public final class LazyExecutorService implements ExecutorService {

  private final Supplier<ExecutorService> supplier;
  private final ReentrantLock reentrantLock = new ReentrantLock();
  private ExecutorService executor;

  public LazyExecutorService() {
    this(ForkJoinPool::commonPool);
  }

  public LazyExecutorService(final Supplier<ExecutorService> supplier) {
    this.supplier = supplier;
  }

  @Override
  public void shutdown() {
    this.reentrantLock.lock();
    try {
      if (this.executor != null) {
        this.executor.shutdown();
      }
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public List<Runnable> shutdownNow() {
    this.reentrantLock.lock();
    try {
      return this.executor == null ? List.of() : this.executor.shutdownNow();
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public boolean isShutdown() {
    this.reentrantLock.lock();
    try {
      return this.executor != null && this.executor.isShutdown();
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public boolean isTerminated() {
    this.reentrantLock.lock();
    try {
      return this.executor != null && this.executor.isTerminated();
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public boolean awaitTermination(long timeout, TimeUnit unit) throws InterruptedException {
    this.reentrantLock.lock();
    try {
      return this.executor == null || this.executor.awaitTermination(timeout, unit);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public <T> Future<T> submit(final Callable<T> task) {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        this.executor = this.supplier.get();
      }
      return this.executor.submit(task);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public <T> Future<T> submit(Runnable task, T result) {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        this.executor = this.supplier.get();
      }
      return this.executor.submit(task, result);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public Future<?> submit(final Runnable task) {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        this.executor = this.supplier.get();
      }
      return this.executor.submit(task);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public <T> List<Future<T>> invokeAll(final Collection<? extends Callable<T>> tasks)
      throws InterruptedException {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        this.executor = this.supplier.get();
      }
      return this.executor.invokeAll(tasks);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public <T> List<Future<T>> invokeAll(
      final Collection<? extends Callable<T>> tasks,
      final long timeout,
      final TimeUnit unit
  ) throws InterruptedException {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        this.executor = this.supplier.get();
      }
      return this.executor.invokeAll(tasks, timeout, unit);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public <T> T invokeAny(Collection<? extends Callable<T>> tasks)
      throws InterruptedException, ExecutionException {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        this.executor = this.supplier.get();
      }
      return this.executor.invokeAny(tasks);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public <T> T invokeAny(
      final Collection<? extends Callable<T>> tasks,
      final long timeout, final TimeUnit unit)
      throws InterruptedException, ExecutionException, TimeoutException {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        this.executor = this.supplier.get();
      }
      return this.executor.invokeAny(tasks, timeout, unit);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  public boolean isInstantiated() {
    this.reentrantLock.lock();
    try {
      return this.executor != null;
    } finally {
      this.reentrantLock.unlock();
    }
  }

  public ExecutorService getExecutor(final boolean forceSupply) {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        if (forceSupply) {
          this.executor = this.supplier.get();
        }
      }
      return this.executor;
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public void execute(final Runnable command) {
    this.reentrantLock.lock();
    try {
      if (this.executor == null) {
        this.executor = this.supplier.get();
      }
      this.executor.execute(command);
    } finally {
      this.reentrantLock.unlock();
    }
  }

  @Override
  public String toString() {
    this.reentrantLock.lock();
    try {
      return this.getClass().getSimpleName() + '(' +
          (this.executor == null ? "not instantiated" : this.executor.toString()) + ')';
    } finally {
      this.reentrantLock.unlock();
    }
  }

  public Supplier<ExecutorService> getSupplier() {
    return this.supplier;
  }
}
