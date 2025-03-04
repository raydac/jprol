package com.igormaznitsa.jprol.libs.extservice;

import com.igormaznitsa.jprol.logic.JProlChoicePoint;

/**
 * Callback from a started service.
 *
 * @since 2.2.2
 */
public interface JProlExternalServiceCallback {
  /**
   * Notification about internal state change.
   *
   * @param source          source service
   * @param prevState       previous service state
   * @param newState        new service state
   * @param optionalObjects some optional objects to be passed to the callback
   */
  void onStateChange(JProlExternalService source, JProlExternalServiceState prevState,
                     JProlExternalServiceState newState, Object... optionalObjects);

  /**
   * Make choice point for the service.
   *
   * @param source source service
   * @param script script to create a choice point.
   * @return created choice point, must not be null
   */
  JProlChoicePoint makeChoicePoint(JProlExternalService source, String script);
}
