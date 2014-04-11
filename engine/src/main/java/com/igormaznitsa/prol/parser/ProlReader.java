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
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

/**
 * The class describes a Prol reader which allows to read text data from an
 * input stream and buffer it. As well it supports pushback operation.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlReader {

  /**
   * The constant shows the inside buffer size which is being used for backpush
   * operations
   */
  private static final int INSIDE_CHAR_BUFFER_SIZE = 64;
  /**
   * The text reader which is being used by the prol reader to read incomming
   * text data
   */
  private final Reader inReader;
  /**
   * Inside char buffer to temporary save text data
   */
  private final char[] insideCharBuffer = new char[INSIDE_CHAR_BUFFER_SIZE];
  /**
   * The variable contains the inside char buffer pointer to show the current
   * free position in the buffer
   */
  private int insideCharBufferPointer = 0;
  /**
   * The variable contains the previous value of the string position indicator
   */
  private int strPosPrev;
  /**
   * The variable contains the previous position of the line number indicator
   */
  private int lineNumPrev;
  /**
   * The variable contains current value of the string position indicator
   */
  private int strPos;
  /**
   * The variable contains current value of the line number indicator
   */
  private int lineNum;

  /**
   * A constructor. To make a reader based on a String object.
   *
   * @param string A string object which will be used as the source for the
   * reader, must not be null
   */
  public ProlReader(final String string) {
    this(new StringReader(string));
  }

  /**
   * A constructor. To make a reader based on an input stream.
   *
   * @param inStream an input stream object which will be used as the source for
   * the reader, must not be null
   */
  public ProlReader(final InputStream inStream) {
    this(new InputStreamReader(inStream));
  }

  /**
   * A constructor. To make a reader based on a java reader object.
   *
   * @param reader a java reader object, must not be null
   */
  public ProlReader(final Reader reader) {
    inReader = reader;
    strPos = 0;
    lineNum = 1;
    strPosPrev = strPos;
    lineNumPrev = lineNum;
  }

  /**
   * Read next char code from the reader
   *
   * @return the next char code or -1 if the stream end has been reached
   * @throws IOException it will be thrown if there is any transport error
   * during the operation
   */
  public synchronized int read() throws IOException {
    int ch;
    if (insideCharBufferPointer > 0) {
      ch = insideCharBuffer[--insideCharBufferPointer];
    }
    else {
      ch = inReader.read();
    }

    strPosPrev = strPos;
    lineNumPrev = lineNum;
    if (ch == '\n') {
      strPos = 0;
      lineNum++;
    }
    else {
      if (ch >= 0) {
        strPos++;
      }
    }
    return ch;
  }

  /**
   * Push back the difference between an etalon string and a string buffer
   * content.
   *
   * @param etalon an etalon string must not be null
   * @param buffer a string buffer object, must not be null
   */
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
    }
    else {
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
   * operation
   */
  public synchronized void close() throws IOException {
    inReader.close();
  }
}
