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

import java.io.*;

public class DefaultProlStreamManagerImpl implements ProlStreamManager {

  private static DefaultProlStreamManagerImpl singltone;

  private DefaultProlStreamManagerImpl() {
  }

  public synchronized static DefaultProlStreamManagerImpl getInstance() {
    if (singltone == null) {
      singltone = new DefaultProlStreamManagerImpl();
    }
    return singltone;
  }

  @Override
  public synchronized Reader getReaderForResource(final String resourceName) throws IOException {
    if (resourceName == null) {
      throw new IllegalArgumentException("The resource name must not be null");
    }

    if (resourceName.equals("user")) {
      return new InputStreamReader(System.in);
    } else {
      if (resourceName.startsWith("@")) {
        String jarName = resourceName.substring(1);
        final InputStream inSream = getClass().getResourceAsStream(jarName);
        if (inSream == null) {
          throw new FileNotFoundException(resourceName);
        }
        return new BufferedReader(new InputStreamReader(inSream));
      } else {
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
    } else {
      return new FileWriter(resourceName, append);
    }
  }

}
