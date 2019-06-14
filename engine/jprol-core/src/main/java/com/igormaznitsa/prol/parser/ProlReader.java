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

package com.igormaznitsa.prol.parser;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

public class ProlReader {

  private static final int INSIDE_CHAR_BUFFER_SIZE = 64;
  private final Reader inReader;
  private final char[] insideCharBuffer = new char[INSIDE_CHAR_BUFFER_SIZE];
  private int insideCharBufferPointer = 0;
  private int strPosPrev;
  private int lineNumPrev;
  private int strPos;
  private int lineNum;

  public ProlReader(final String string) {
    this(new StringReader(string));
  }

  public ProlReader(final Reader reader) {
    inReader = reader;
    strPos = 0;
    lineNum = 1;
    strPosPrev = strPos;
    lineNumPrev = lineNum;
  }

  public synchronized int read() throws IOException {
    int ch;
    if (insideCharBufferPointer > 0) {
      ch = insideCharBuffer[--insideCharBufferPointer];
    } else {
      ch = inReader.read();
    }

    strPosPrev = strPos;
    lineNumPrev = lineNum;
    if (ch == '\n') {
      strPos = 0;
      lineNum++;
    } else {
      if (ch >= 0) {
        strPos++;
      }
    }
    return ch;
  }

  public synchronized void pushBufferDifference(final String etalon, final StringBuilder buffer) {
    int chars = buffer.length() - etalon.length();
    int pos = buffer.length() - 1;
    while (chars > 0) {
      char ch = buffer.charAt(pos--);
      insideCharBuffer[insideCharBufferPointer++] = ch;
      chars--;
      strPos--;
      if (strPos < 1) {
        strPos = 1;
      }
      strPosPrev = strPos;
      if (ch == '\n') {
        lineNum--;
        if (lineNum < 1) {
          lineNum = 1;
        }
        lineNumPrev = lineNum;
      }
    }
  }

  /**
   * Get the previous line number
   *
   * @return the previous line number
   */
  public int getPrevLineNumber() {
    return lineNumPrev;
  }

  /**
   * Get the previous string position
   *
   * @return the previous string position
   */
  public int getPrevStrPos() {
    return strPosPrev;
  }

  /**
   * Get the current line number
   *
   * @return the current line number
   */
  public int getLineNumber() {
    return lineNum;
  }

  /**
   * Get the current string position
   *
   * @return the current string position
   */
  public int getStrPos() {
    return strPos;
  }

  /**
   * Push a char back into the inside buffer to be read into the next read
   * operation
   *
   * @param ch the char to be placed into the inside buffer
   */
  public synchronized void pushCharBack(final char ch) {
    insideCharBuffer[insideCharBufferPointer++] = ch;
    if (ch == '\n') {
      strPos = 1;
      lineNum--;
      if (lineNum <= 0) {
        lineNum = 1;
      }
    } else {
      strPos--;
      if (strPos <= 0) {
        strPos = 1;
      }
    }
  }

  /**
   * Close current reader
   *
   * @throws IOException it will be thrown if there is any error during the
   *                     operation
   */
  public synchronized void close() throws IOException {
    inReader.close();
  }
}
