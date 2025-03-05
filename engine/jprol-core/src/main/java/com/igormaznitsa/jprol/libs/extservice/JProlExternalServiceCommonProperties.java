package com.igormaznitsa.jprol.libs.extservice;

/**
 * Names of common properties expected by service and service factory user.
 *
 * @since 2.2.2
 */
public interface JProlExternalServiceCommonProperties {
  /**
   * Identifier of service or service factory
   */
  String ID = "jprol.service.id";
  /**
   * Human-readable name of service or service factory,
   */
  String NAME = "jprol.service.name";
  /**
   * Description of service or service factory.
   */
  String DESCRIPTION = "jprol.service.description";
}
