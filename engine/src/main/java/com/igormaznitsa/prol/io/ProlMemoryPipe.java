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
import com.igormaznitsa.prol.logic.ProlContext;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.nio.charset.Charset;

/**
 * The class describes a memory pipe which is the object allows input-output
 * operations in memory buffer. It is based on the java pipe mechanism so you
 * should remember that read operations can be block execution until they get
 * new data, to avoid that case don't forget to call 'told/0' predicate for a
 * pipe for active nenory pipe. If you try read data of the pipe from Java then
 * don't please to call closeForWrittersOnly() method to avoid any blocking for
 * read operations. Also the pipe data can be read only one time because it is a
 * temporary storage. Once you call the 'seen/0' predicate for an active pipe,it
 * will be removed from the inside pool!
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.io.ProlStream
 * @see com.igormaznitsa.prol.io.ProlTextWriter
 * @see com.igormaznitsa.prol.io.ProlTextReader
 */
public class ProlMemoryPipe implements ProlStream, ProlTextReader, ProlTextWriter {

  /**
   * The variable saves the resource ID
   */
  private final String resourceId;
  /**
   * The context is using the pipe
   */
  private final ProlContext context;
  /**
   * The reader used by the pipe to read from itself
   */
  private final ProlTextInputStream reader;
  /**
   * The witer used by the pipe to write into itself
   */
  private final ProlTextOutputStream writer;

  /**
   * The constructor
   *
   * @param resourceId the resource identifier, must not be null
   * @param context the owner context for the pipe, must not be null
   * @throws IOException it will be thrown if there will be any transport errors
   */
  public ProlMemoryPipe(final String resourceId, final ProlContext context) throws IOException {
    if (resourceId == null) {
      throw new IllegalArgumentException("Resource identifier must not be null");
    }
    if (context == null) {
      throw new IllegalArgumentException("Context must not be null");
    }

    this.resourceId = resourceId;
    this.context = context;

    final PipedWriter pipeWriter = new PipedWriter();
    final PipedReader pipeReader = new PipedReader(pipeWriter);

    reader = new ProlTextInputStream(pipeReader, context);
    writer = new ProlTextOutputStream(pipeWriter, context, false);
  }

  /**
   * Get the reader for the pipe
   *
   * @return the pipe reader
   */
  public ProlTextReader getReader() {
    return reader;
  }

  /**
   * Get the reader for the pipe
   *
   * @return the pipe reader
   */
  public ProlTextWriter getWriter() {
    return writer;
  }

  @Override
  public ProlContext getContext() {
    return context;
  }

  @Override
  public void close() throws IOException {
    synchronized (writer) {
      writer.close();
    }

    synchronized (reader) {
      reader.close();
    }
  }

  /**
   * Close only for output data, it will not be possible to write data into the
   * pipe after the operation. From prolog the method will be called when told/0
   * is called for the pipe. If you want to manipulate with the content of the
   * pipe, you have to call the method else your read operations can be blocked
   * until incomming data.
   *
   * @throws IOException it will be thrown if there is any exception
   */
  public void closeForWriteOnly() throws IOException {
    synchronized (writer) {
      writer.close();
    }
  }

  @Override
  public String getResourceId() {
    return resourceId;
  }

  @Override
  public Term getAsTerm() {
    final Term result = new Term(resourceId);
    return result;
  }

  @Override
  public Term readToken() throws IOException {
    synchronized (reader) {
      return reader.readToken();
    }
  }

  @Override
  public Term readTerm() throws IOException {
    synchronized (reader) {
      return reader.readTerm();
    }
  }

  @Override
  public TermInteger readChar() throws IOException {
    synchronized (reader) {
      return reader.readChar();
    }
  }

  @Override
  public void writeTerm(Term term) throws IOException {
    synchronized (writer) {
      writer.writeTerm(term);
    }
  }

  @Override
  public void writeChar(Term term) throws IOException {
    synchronized (writer) {
      writer.writeChar(term);
    }
  }

  /**
   * Get the content written into the pipe as a String object. Remember that the
   * operation will clear the buffer.
   *
   * @param charset the charset which will be used to convert byte data from the
   * pipe into the String
   * @return the pipe data content as a String object
   * @throws IOException it will be thrown if there is any IO error during the
   * operation
   */
  public String getAllDataAsString(final Charset charset) throws IOException {
    return new String(getAllDataAsByteArray(), charset);
  }

  /**
   * Get the content written into the pipe as a byte array
   *
   * @return the data from the pipe as a byte array
   * @throws IOException it will be thrown if there is any IO operation
   */
  public byte[] getAllDataAsByteArray() throws IOException {
    synchronized (reader) {
      final ByteArrayOutputStream buffer = new ByteArrayOutputStream(1024);
      while (true) {
        final int chr = reader.readChar().getNumericValue().intValue();

        if (chr < 0) {
          break;
        }

        buffer.write(chr);
      }
      return buffer.toByteArray();
    }
  }
}
