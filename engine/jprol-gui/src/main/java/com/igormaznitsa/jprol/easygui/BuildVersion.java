/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.jprol.easygui;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Optional.empty;
import static java.util.Optional.of;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Optional;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

public final class BuildVersion {

  private static final Logger LOGGER = Logger.getLogger(BuildVersion.class.getName());
  private static final String RESOURCE =
      "/com/igormaznitsa/jprol/easygui/build-version.properties";
  private static final Pattern UNRESOLVED_PLACEHOLDER = Pattern.compile("\\$\\{[^}]+}");
  private static final String CURRENT = resolve();

  private BuildVersion() {
  }

  public static String current() {
    return CURRENT;
  }

  private static String resolve() {
    return readStampedVersion().or(BuildVersion::readManifestVersion).orElse("unknown");
  }

  private static Optional<String> readStampedVersion() {
    final InputStream in = BuildVersion.class.getResourceAsStream(RESOURCE);
    if (in == null) {
      return empty();
    }
    try (Reader reader = new InputStreamReader(in, UTF_8)) {
      final Properties properties = new Properties();
      properties.load(reader);
      return accept(properties.getProperty("version"));
    } catch (IOException ex) {
      LOGGER.log(Level.WARNING, "Can't read stamped build version", ex);
      return empty();
    }
  }

  private static Optional<String> readManifestVersion() {
    return accept(BuildVersion.class.getPackage().getImplementationVersion());
  }

  private static Optional<String> accept(final String raw) {
    if (raw == null) {
      return empty();
    }
    final String trimmed = raw.strip();
    if (trimmed.isEmpty() || UNRESOLVED_PLACEHOLDER.matcher(trimmed).find()) {
      return empty();
    }
    return of(trimmed);
  }
}
