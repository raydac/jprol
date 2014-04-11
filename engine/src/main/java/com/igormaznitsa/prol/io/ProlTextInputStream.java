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
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermInteger;
import com.igormaznitsa.prol.parser.ProlReader;
import com.igormaznitsa.prol.parser.ProlTokenizer;
import com.igormaznitsa.prol.parser.ProlTokenizer.ProlTokenizerResult;
import com.igormaznitsa.prol.parser.ProlTreeBuilder;
import java.io.IOException;
import java.io.Reader;

/**
 * The class implements a text input stream which is used by a prol engine to
 * read from an input stream.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.io.ProlStream
 * @see com.igormaznitsa.prol.io.ProlTextReader
 */
public class ProlTextInputStream implements ProlStream, ProlTextReader {

  /**
   * The variable contains a prol tokenizer which is used to read tokens from an
   * input stream
   */
  private final ProlTokenizer tokenizer;

  /**
   * The variable contains a prol reader to read chars from an input stream
   */
  private final ProlReader reader;

  /**
   * The variable contains a prol tree builder to make tree from read terms
   */
  private ProlTreeBuilder treeBuilder;

  /**
   * The variable contains the context which is the owner for the stream
   */
  private final ProlContext context;

  /**
   * The variable contains the identifier of the stream as a String
   */
  private final String resourceId;

  /**
   * The term describes the stream to be used in a prolog engine
   */
  private final Term thisTerm;

  /**
   * A constructor (it is used by memory pipes so it is protected)
   *
   * @param reader a reader which will be used to read chars, must not be null
   * @param context the owner context for the reader, must not be null
   * @throws IOException it will be thrown if there is any IO error during the
   * operation
   */
  protected ProlTextInputStream(final Reader reader, final ProlContext context) throws IOException {
    super();

    this.resourceId = ".local";
    this.context = context;

    this.reader = new ProlReader(reader);
    this.tokenizer = new ProlTokenizer();

    thisTerm = new Term(resourceId);
  }

  /**
   * A constructor
   *
   * @param resourceId the resource identifier of the stream, must not be null
   * @param context the owner context for the reader, must not be null
   * @throws IOException it will be thrown if there is any IO error during the
   * operation
   */
  public ProlTextInputStream(final String resourceId, final ProlContext context) throws IOException {
    super();

    if (resourceId == null) {
      throw new IllegalArgumentException("The resource ID must not be null");
    }

    this.resourceId = resourceId;
    this.context = context;

    final Reader inReader = context.getStreamManager().getReaderForResource(resourceId);
    if (inReader == null) {
      throw new IOException("Resource \'" + resourceId + "\' has not been found");
    }

    this.reader = new ProlReader(inReader);
    this.tokenizer = new ProlTokenizer();

    thisTerm = new Term(resourceId);
  }

  @Override
  public String getResourceId() {
    return this.resourceId;
  }

  @Override
  public ProlContext getContext() {
    return this.context;
  }

  @Override
  public synchronized void close() throws IOException {
    reader.close();
  }

  @Override
  public synchronized Term readToken() throws IOException {
    ProlTokenizerResult result = tokenizer.nextToken(reader, context.getKnowledgeBase());
    if (result == null) {
      return END_OF_FILE;
    }
    return result.getTerm();
  }

  @Override
  public synchronized Term readTerm() throws IOException {
    if (treeBuilder == null) {
      treeBuilder = new ProlTreeBuilder(context);
    }
    Term term = treeBuilder.readPhraseAndMakeTree(tokenizer, reader);
    if (term == null) {
      return END_OF_FILE;
    }
    else {
      return term;
    }
  }

  @Override
  public synchronized TermInteger readChar() throws IOException {
    int chr = reader.read();
    return new TermInteger(chr);
  }

  @Override
  public Term getAsTerm() {
    return thisTerm;
  }
}
