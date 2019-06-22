package com.igormaznitsa.prol.io;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.Terms;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;

import java.io.Reader;
import java.util.Optional;

public class ProlReader extends ProlIoResource<Reader> {

  private final PrologParser parser;
  private final ProlContext context;

  public ProlReader(final ProlContext context, final String id, final Reader reader) {
    super(id, reader);
    this.parser = new GenericPrologParser(reader, context);
    this.context = context;
  }

  public Term nextSentence() {
    return Terms.fromParsed(this.context, this.parser.next());
  }

  public Optional<Term> nextTerm() {
    final TokenizerResult result = this.parser.getInternalTokenizer().readNextToken();
    return result == null ? Optional.empty() : Optional.ofNullable(Terms.fromParsed(this.context, result.getResult()));
  }

}
