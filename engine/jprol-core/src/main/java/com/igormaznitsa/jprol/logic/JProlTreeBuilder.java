package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.RuntimeIOException;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import java.io.IOException;
import java.io.Reader;

public class JProlTreeBuilder implements AutoCloseable {

  private final JProlContext context;
  private final PrologParser parser;

  public JProlTreeBuilder(final JProlContext context, final Reader reader,
                          final boolean closeReader) {
    this.context = context;
    this.parser = new GenericPrologParser(reader, this.context.getParserContext()) {
      @Override
      protected boolean isCloseReader() {
        return closeReader;
      }
    };
  }

  public Term readPhraseAndMakeTree() {
    if (this.parser.hasNext()) {
      final PrologTerm result = this.parser.next();
      return Terms.fromParsed(this.context, result);
    } else {
      return null;
    }
  }

  @Override
  public void close() {
    try {
      this.parser.close();
    } catch (IOException ex) {
      throw new RuntimeIOException(ex);
    }
  }
}
