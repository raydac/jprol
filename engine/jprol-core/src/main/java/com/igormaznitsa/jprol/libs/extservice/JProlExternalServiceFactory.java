package com.igormaznitsa.jprol.libs.extservice;

import java.util.Properties;

/**
 * Factory to make new instance of a service.
 *
 * @since 2.2.2
 */
public interface JProlExternalServiceFactory {

  /**
   * Get the properties of the service factory.
   *
   * @return the properties must not be null.
   */
  Properties getProperties();

  /**
   * Create new service instance.
   *
   * @param properties properties to be used during service create. must not be null.
   * @return the new service instance, must not be null.
   */
  JProlExternalService makeService(Properties properties);
}
