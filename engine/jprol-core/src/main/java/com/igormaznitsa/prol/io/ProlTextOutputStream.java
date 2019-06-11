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

import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.logic.ProlContext;

import java.io.IOException;
import java.io.Writer;

public class ProlTextOutputStream implements ProlStream, ProlTextWriter {

  private final String resourceId;
  private final Writer writer;
  private final ProlContext context;

  protected ProlTextOutputStream(final Writer writer, final ProlContext context, final boolean forAppend) throws IOException {
    super();
    this.resourceId = ".local";
    this.context = context;
    this.writer = writer;
  }

  public ProlTextOutputStream(final String resourceId, final ProlContext context, final boolean forAppend) throws IOException {
    super();
    if (resourceId == null) {
      throw new NullPointerException("Resource id must not be null");
    }
    this.resourceId = resourceId;
    this.context = context;

    writer = context.getStreamManager().getWriterForResource(resourceId, forAppend);

    if (writer == null) {
      throw new IOException("Can't open resource \'" + resourceId + "\' for writting");
    }
  }

  @Override
  public ProlContext getContext() {
    return context;
  }

  public synchronized Writer getWriter() {
    return writer;
  }

  @Override
  public String getResourceId() {
    return resourceId;
  }

  @Override
  public synchronized void writeTerm(final Term term) throws IOException {
    writer.write(term.forWrite());
    writer.flush();
  }

  @Override
  public synchronized void writeChar(final Term term) throws IOException {
    char charToWrite = 0;
    if (term instanceof NumericTerm) {
      charToWrite = (char) term.toNumber().intValue();
    } else {
      final String text = term.getText();
      if (text != null) {
        charToWrite = text.charAt(0);
      }
    }
    writer.write(charToWrite);
    writer.flush();
  }

  @Override
  public synchronized void close() throws IOException {
    writer.close();
  }

}
