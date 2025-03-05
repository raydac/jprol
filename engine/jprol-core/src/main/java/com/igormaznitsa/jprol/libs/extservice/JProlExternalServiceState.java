package com.igormaznitsa.jprol.libs.extservice;

/**
 * State of service.
 *
 * @since 2.2.2
 */
public enum JProlExternalServiceState {
  /**
   * Service in unknown state.
   */
  UNKNOWN,
  /**
   * Service prepared for work.
   */
  INIT,
  /**
   * Working state.
   */
  RUNNING,
  /**
   * Paused.
   */
  PAUSED,
  /**
   * Disposed.
   */
  DISPOSED
}
