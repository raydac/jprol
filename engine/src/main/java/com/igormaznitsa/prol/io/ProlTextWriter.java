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
import java.io.IOException;

/**
 * The interface describes a prol writer allows to output terms
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface ProlTextWriter {

  /**
   * Write a term into the writer
   *
   * @param term the term to be written, must not be null
   * @throws IOException it will be thrown if there is any transport error
   * during the operation
   */
  public void writeTerm(final Term term) throws IOException;

  /**
   * Write a char into the writer, it can be Term (then we write the first
   * symbol of the string representation or a Numeric term so we write the char
   * code)
   *
   * @param term the term describes a char, a Term object or a Numeri term
   * @throws IOException it will be thrown if there is any transport error
   * during the operation
   */
  public void writeChar(final Term term) throws IOException;

  /**
   * Get the writer resource id
   *
   * @return the writer resource id as a String object
   */
  public String getResourceId();

  /**
   * Close the writer
   *
   * @throws IOException it will be thrown if there is any transport error
   * during the operation
   */
  public void close() throws IOException;
}
