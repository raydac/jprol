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

public class ProlMemoryPipe implements ProlStream, ProlTextReader, ProlTextWriter {

  private final String resourceId;
  private final ProlContext context;
  private final ProlTextInputStream reader;
  private final ProlTextOutputStream writer;

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

  public ProlTextReader getReader() {
    return reader;
  }

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

  public String getAllDataAsString(final Charset charset) throws IOException {
    return new String(getAllDataAsByteArray(), charset);
  }

  public byte[] getAllDataAsByteArray() throws IOException {
    synchronized (reader) {
      final ByteArrayOutputStream buffer = new ByteArrayOutputStream(1024);
      while (!Thread.currentThread().isInterrupted()) {
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
