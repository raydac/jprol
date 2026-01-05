package com.igormaznitsa.jprol.jsr223;

/**
 * Object is disposable.
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
