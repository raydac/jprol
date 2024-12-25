package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import java.io.Reader;
import java.util.NoSuchElementException;

public class JProlTreeBuilder {

  private final JProlContext context;
  private final ThreadLocal<PrologParser> parsers = new ThreadLocal<>();

  public JProlTreeBuilder(final JProlContext context) {
    this.context = context;
  }

  public Term readPhraseAndMakeTree(final Reader reader) {
    PrologParser parser = parsers.get();
    if (parser == null) {
      parser = new GenericPrologParser(reader, this.context.getParserContext());
      parsers.set(parser);
    }

    try {
      final PrologTerm result = parser.next();
      return Terms.fromParsed(this.context, result);
    } catch (NoSuchElementException ex) {
      parsers.remove();
      return null;
    }
  }
}
