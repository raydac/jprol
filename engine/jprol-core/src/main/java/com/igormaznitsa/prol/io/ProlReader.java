package com.igormaznitsa.prol.io;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.Terms;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.PrologParser;

import java.io.IOException;
import java.io.Reader;

public class ProlReader extends ProlIoResource<Reader> {

  private final PrologParser parser;
  private final ProlContext context;

  public ProlReader(final ProlContext context, final String id, final Reader reader) {
    super(id, reader);
    this.parser = new GenericPrologParser(reader, context);
    this.context = context;
  }

  public int readChar() throws IOException {
    return this.parser.getInternalTokenizer().readChar();
  }

  public Term read() throws IOException {
    return Terms.fromParsed(this.context, this.parser.next());
  }

}
