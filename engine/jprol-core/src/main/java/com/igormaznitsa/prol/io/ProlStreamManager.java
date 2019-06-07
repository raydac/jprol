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

package com.igormaznitsa.prol.io;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * The interface describes an object which can be used to create reader and
 * writer streams to be used by a prol engine
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface ProlStreamManager {

  /**
   * Get the reader for a resource for it's name.
   *
   * @param resourceName the resource name of a saved object which should be
   * opened for reading. Must not be null
   * @return an opened text reader for the object, must not be null
   * @throws IOException it will be thrown if it is impossible to open the
   * resource successfully
   */
  Reader getReaderForResource(final String resourceName) throws IOException;

  Writer getWriterForResource(final String resourceName, final boolean append) throws IOException;
}
