package com.igormaznitsa.jprol.jsr223;

/**
 * Implementation may hold per-thread or auxiliary state that is not reclaimed by the JVM alone; {@link #gc()} requests a sweep.
 */
@FunctionalInterface
public interface CanGC {
  /**
   * Collect stale internal state (for example terminated worker threads).
   */
  void gc();
}
