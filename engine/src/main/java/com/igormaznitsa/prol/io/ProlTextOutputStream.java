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

import com.igormaznitsa.prol.logic.*;
import com.igormaznitsa.prol.data.NumericTerm;
import com.igormaznitsa.prol.data.Term;
import java.io.IOException;
import java.io.Writer;

/**
 * The class describes a text input stream which is used by a prol engine to
 * write texts and chars into an input stream.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.io.ProlStream
 * @see com.igormaznitsa.prol.io.ProlTextWriter
 */
public class ProlTextOutputStream implements ProlStream, ProlTextWriter {

  /**
   * The variable contains the text identifier of the string
   */
  private final String resourceId;

  /**
   * The variable contains the writer which is being used to write in
   */
  private final Writer writer;

  /**
   * The variable contains the owner context for the stream
   */
  private final ProlContext context;

  /**
   * The variable contains the representation of the stream object to be used in
   * a prol engine
   */
  private final Term thisTerm;

  /**
   * A constructor (it is used by memory pipes so it is protected one)
   *
   * @param writer a writer to be used to write chars into, must not be null
   * @param context the owner context for the stream, must not be null
   * @param forAppend usually the flag shows that we need to append data to
   * current context but in the case the flag mainly doesn't play any role
   * @throws IOException it will be thrown if there is any IO error during the
   * operation
   */
  protected ProlTextOutputStream(final Writer writer, final ProlContext context, final boolean forAppend) throws IOException {
    super();
    this.resourceId = ".local";
    this.context = context;
    this.writer = writer;
    thisTerm = new Term(resourceId);
  }

  /**
   * A constructor
   *
   * @param resourceId the resource id describes the resource used by the
   * stream, must not be null
   * @param context the owner context for the stream, must not be null
   * @param forAppend if the flag is true then new data will be append to the
   * current content of the resource, else the resource will be rewritten
   * @throws IOException it will be thrown if there is any transport error
   * during the operation
   */
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

    thisTerm = new Term(resourceId);
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
      charToWrite = (char) ((NumericTerm) term).getNumericValue().intValue();
    }
    else {
      final String text = term.getText();
      if (text != null) {
        charToWrite = text.charAt(0);
      }
    }
    writer.write(charToWrite);
    writer.flush();
  }

  @Override
  public Term getAsTerm() {
    return thisTerm;
  }

  @Override
  public synchronized void close() throws IOException {
    writer.close();
  }

}
