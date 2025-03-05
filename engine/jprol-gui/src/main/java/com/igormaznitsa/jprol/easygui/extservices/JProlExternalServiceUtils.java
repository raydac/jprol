package com.igormaznitsa.jprol.easygui.extservices;

import com.igormaznitsa.jprol.libs.extservice.JProlExternalServiceFactory;
import java.util.List;
import java.util.ServiceLoader;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Utility methods to work with external services.
 *
 * @since 2.2.2
 */
public final class JProlExternalServiceUtils {
  private JProlExternalServiceUtils() {

  }

  /**
   * Find all JProl external service factories,
   *
   * @param classLoaders additional class loaders to search factories.
   * @return list of all factories in visibility.
   */
  public static List<JProlExternalServiceFactory> findAllExternalServiceFactories(
      final List<ClassLoader> classLoaders) {
    return Stream.concat(ServiceLoader.load(JProlExternalServiceFactory.class).stream(),
            classLoaders.stream()
                .flatMap(x -> ServiceLoader.load(JProlExternalServiceFactory.class, x).stream()))
        .map(ServiceLoader.Provider::get)
        .collect(Collectors.toList());
  }

}
