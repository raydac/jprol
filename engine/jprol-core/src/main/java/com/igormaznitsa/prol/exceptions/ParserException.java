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

public class ParserException extends ProlException {

  private static final long serialVersionUID = 6985224567025371152L;
  private final int Line;
  private final int Pos;

  public ParserException(final int line, final int pos) {
    Line = line;
    Pos = pos;
  }

  public ParserException(final String message, final int line, final int pos) {
    super(message);
    Line = line;
    Pos = pos;
  }

  public ParserException(final String message, final int line, final int pos, final Throwable cause) {
    super(message, cause);
    Line = line;
    Pos = pos;
  }

  public ParserException(final int line, final int pos, final Throwable cause) {
    super(cause);
    Line = line;
    Pos = pos;
  }

  public int getLine() {
    return Line;
  }

  public int getPos() {
    return Pos;
  }

  @Override
  public String getMessage() {
    return super.getMessage() + '[' + Line + ':' + Pos + ']';
  }

}
