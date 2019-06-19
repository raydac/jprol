package com.igormaznitsa.prol.io;

import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.PrologParser;

import java.io.Reader;

public class ProlReader extends ProlIoResource<Reader> {

  private final PrologParser parser;
  private final ProlContext context;

  public ProlReader(final ProlContext context, final String id, final Reader reader) {
    super(id, reader);
    this.parser = new GenericPrologParser(reader, context);
    this.context = context;
  }


}
