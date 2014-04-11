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

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;

/**
 * The class implements the ProlStreamManager interface and can be used to
 * read-write files and read resources from JAR
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.io.ProlStreamManager
 */
public class DefaultProlStreamManagerImpl implements ProlStreamManager {

  /**
   * The variable contains only instance to be used
   */
  private static DefaultProlStreamManagerImpl singltone;

  private DefaultProlStreamManagerImpl() {
  }

  /**
   * Get the instance of the manager
   *
   * @return the instance of the class
   */
  public synchronized static final DefaultProlStreamManagerImpl getInstance() {
    if (singltone == null) {
      singltone = new DefaultProlStreamManagerImpl();
    }
    return singltone;
  }

  /**
   * Get the reader for a resource
   *
   * @param resourceName the resource name, must not be null. If the name starts
   * with "@", then it will be used as the name inside JAR resource and will be
   * opened with Class.getResourceAsStream(). The argument must not be null.
   * @return the opened reader as a Reader object.
   * @throws IOException it will be thrown if there will be any transport error.
   */
  @Override
  public synchronized Reader getReaderForResource(final String resourceName) throws IOException {
    if (resourceName == null) {
      throw new IllegalArgumentException("The resource name must not be null");
    }

    if (resourceName.equals("user")) {
      return new InputStreamReader(System.in);
    }
    else {
      if (resourceName.startsWith("@")) {
        String jarName = resourceName.substring(1);
        final InputStream inSream = getClass().getResourceAsStream(jarName);
        if (inSream == null) {
          throw new FileNotFoundException(resourceName);
        }
        return new BufferedReader(new InputStreamReader(inSream));
      }
      else {
        return new FileReader(resourceName);
      }
    }
  }

  @Override
  public synchronized Writer getWriterForResource(final String resourceName, boolean append) throws IOException {
    if (resourceName == null) {
      throw new IllegalArgumentException("The resource name must not be null");
    }

    if (resourceName.equals("user")) {
      return new PrintWriter(System.out);
    }
    else {
      return new FileWriter(resourceName, append);
    }
  }

}
