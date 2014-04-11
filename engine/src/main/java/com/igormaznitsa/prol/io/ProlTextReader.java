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

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermInteger;
import java.io.IOException;

/**
 * The interface describea s prol text reader i.e. an object which allows to
 * read tokens, terms and chars from somewhere
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface ProlTextReader {

  /**
   * Read the next token
   *
   * @return the next token or the END_OF_FILE term if the stream end has been
   * reached
   * @throws IOException it will be thrown if there is any transport error
   * during the operation
   */
  public Term readToken() throws IOException;

  /**
   * Read the next term (dot ended)
   *
   * @return the next term or the END_OF_FILE term if the stream end has been
   * reached
   * @throws IOException it will be thrown if there is any transport or logical
   * error during the operation
   */
  public Term readTerm() throws IOException;

  /**
   * Read the next char code as a TermInteger
   *
   * @return the next char code or -1 if the stream end has been reached
   * @throws IOException it will be thrown if there is any transport or logical
   * error during the operation
   */
  public TermInteger readChar() throws IOException;

  /**
   * Get the resource id for the reader
   *
   * @return the resource id as a String object
   */
  public String getResourceId();

  /**
   * Close the reader
   *
   * @throws IOException it will be thrown if there is any error during the
   * operation
   */
  public void close() throws IOException;
}
