package com.igormaznitsa.jprol.libs.extservice;

/**
 * Names of common properties expected by service and service factory user.
 *
 * @since 2.3.0
 */
public interface JProlExternalServiceCommonProperties {
  /**
   * Identifier of service or service factory
   */
  String PROPERTY_JPROL_SERVICE_ID = "jprol.service.id";
  /**
   * Human-readable name of service or service factory,
   */
  String PROPERTY_JPROL_SERVICE_NAME = "jprol.service.name";
  /**
   * Description of service or service factory.
   */
  String PROPERTY_JPROL_SERVICE_DESCRIPTION = "jprol.service.description";
}
