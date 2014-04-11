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
import com.igormaznitsa.prol.logic.ProlContext;
import java.io.IOException;

/**
 * The interface describes an abstract stream object which can be used by the
 * prol engine
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface ProlStream {

  /**
   * The string constant which is used as the end stream flag
   */
  public static final String END_OF_FILE_STR = "end_of_file";

  /**
   * The constant describes the term which is used as the flag of the end of a
   * file
   */
  public static final Term END_OF_FILE = new Term(END_OF_FILE_STR);

  /**
   * Get the prol context for the stream
   *
   * @return the prol context for the stream, must not be null
   */
  public ProlContext getContext();

  /**
   * Close the stream
   *
   * @throws IOException it will be thrown if the stream can't be closed
   * successfully
   */
  public void close() throws IOException;

  /**
   * The resource id for the stream
   *
   * @return the resource name for the stream, must not be null
   */
  public String getResourceId();

  /**
   * Get the stream object as a Term
   *
   * @return the stream object as a Term, must not be null
   */
  public Term getAsTerm();
}
