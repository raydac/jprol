package com.igormaznitsa.jprol.libs.extservice;


import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import java.util.List;
import java.util.Properties;

/**
 * Interface describes some external service which can communicate with caller through callback.
 *
 * @since 2.3.0
 */
public interface JProlExternalService extends JProlExternalServiceCommonProperties {
  /**
   * Get current state.
   *
   * @return the current state, must not be null
   */
  JProlExternalServiceState getState();

  /**
   * Get the service properties.
   *
   * @return the properties, must not be null
   */
  Properties getProperties();

  List<JProlExternalServiceStartPropertyDescriptor> getStartRequiredProperties();

  /**
   * Find all internal JProl libraries provided by the service.
   *
   * @return list of service JProl library classes.
   */
  List<Class<AbstractJProlLibrary>> findLibraries();

  /**
   * Init the service before use, must be called before any operations with the service.
   */
  void init();

  /**
   * Start execution of the service.
   *
   * @param properties properties to be provided
   * @param callback   the callback object to get signals from service.
   */
  void start(Properties properties, JProlExternalServiceCallback callback);

  /**
   * Pause service work.
   */
  void pause();

  /**
   * Resume of work if paused.
   */
  void resume();

  /**
   * Stop service work.
   */
  void stop();

  /**
   * Dispose service.
   */
  void dispose();
}
