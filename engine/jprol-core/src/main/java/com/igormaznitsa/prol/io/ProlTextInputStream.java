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
import com.igormaznitsa.prol.data.TermLong;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlReader;
import com.igormaznitsa.prol.parser.ProlTokenizer;
import com.igormaznitsa.prol.parser.ProlTreeBuilder;

import java.io.IOException;
import java.io.Reader;

import static com.igormaznitsa.prol.data.Terms.newLong;

public class ProlTextInputStream implements ProlStream, ProlTextReader {

  private final ProlTokenizer tokenizer;
  private final ProlReader reader;
  private final ProlContext context;
  private final String resourceId;
  private ProlTreeBuilder treeBuilder;

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
  public synchronized Term readTerm() throws IOException {
    if (treeBuilder == null) {
      treeBuilder = new ProlTreeBuilder(context);
    }
    Term term = treeBuilder.readPhraseAndMakeTree(tokenizer, reader);
    if (term == null) {
      return END_OF_FILE;
    } else {
      return term;
    }
  }

  @Override
  public synchronized TermLong readChar() throws IOException {
    int chr = reader.read();
    return newLong(chr);
  }
}
