package com.igormaznitsa.jprol.jsr223;

/**
 * Implementation can keep internal non-used resources which can be recognized as a garbage after time
 * but not in scope of standard GC. Call of the method allows manually clear resources,
 */
@FunctionalInterface
public interface CanGC {
  /**
   * Collect internal garbage and free resources.
   */
  void gc();
}
