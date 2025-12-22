package com.igormaznitsa.jprol.libs.extservice;

/**
 * Descriptor of a property required by the service to start.
 *
 * @since 2.3.0
 */
public interface JProlExternalServiceStartPropertyDescriptor {
  /**
   * Human-readable name.
   *
   * @return the human-readable name of property, must not be null.
   */
  String getName();

  /**
   * Expected property name.
   *
   * @return the property name, must not be null.
   */
  String getPropertyName();

  /**
   * The default value for the property.
   *
   * @return the default value for the property, must not be null but can be empty.
   */
  String getDefaultValue();

  /**
   * Description of the property and its functionality.
   *
   * @return the description, can be null.
   */
  String getDescription();

  /**
   * Flag shows that the property is mandatory for service start.
   *
   * @return true if mandatory, false otherwise.
   */
  boolean isMandatory();
}
