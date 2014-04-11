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
package com.igormaznitsa.prol.exceptions;

/**
 * The exception describes an exception which can be thrown by a prol parser.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.exceptions.ProlException
 */
public class ParserException extends ProlException {
  private static final long serialVersionUID = 6985224567025371152L;

  /**
   * The line number where the exception has been thrown
   */
  private final int Line;

  /**
   * The position at the line where the exception has been thrown
   */
  private final int Pos;

  /**
   * Get the line contains an error
   *
   * @return the line number where the exception has been thrown, 1 is the first
   * line
   */
  public int getLine() {
    return Line;
  }

  /**
   * Get the line position contains an error
   *
   * @return the line position where the exception has been thrown, 1 is the
   * first position.
   */
  public int getPos() {
    return Pos;
  }

  /**
   * A constructor
   *
   * @param line the line number where the exception has been thrown
   * @param pos the string position where the exception has been thrown
   */
  public ParserException(final int line, final int pos) {
    Line = line;
    Pos = pos;
  }

  /**
   * A constructor
   *
   * @param message a text message describing the situation, can be null
   * @param line the line number where the exception has been thrown
   * @param pos the string position where the exception has been thrown
   */
  public ParserException(final String message, final int line, final int pos) {
    super(message);
    Line = line;
    Pos = pos;
  }

  /**
   * A constructor
   *
   * @param message a text message describing the situation, can be null
   * @param line the line number where the exception has been thrown
   * @param pos the string position where the exception has been thrown
   * @param cause the java exception which was the cause for the prol exception
   */
  public ParserException(final String message, final int line, final int pos, final Throwable cause) {
    super(message, cause);
    Line = line;
    Pos = pos;
  }

  /**
   * A constructor
   *
   * @param line the line number where the exception has been thrown
   * @param pos the string position where the exception has been thrown
   * @param cause the java exception which was the cause for the prol exception
   */
  public ParserException(final int line, final int pos, final Throwable cause) {
    super(cause);
    Line = line;
    Pos = pos;
  }

  @Override
  public String getMessage() {
    return super.getMessage() + '[' + Line + ':' + Pos + ']';
  }

}
