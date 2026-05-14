package com.igormaznitsa.jprol.jsr223;

/**
 * Object that can release external or native resources explicitly (in addition to {@code AutoCloseable} where applicable).
 */
public interface Disposable {
  /**
   * Check state.
   *
   * @return if disposed then true, false otherwise
   */
  boolean isDisposed();

  /**
   * Dispose, must not throw any error if already disposed.
   */
  void dispose();
}
