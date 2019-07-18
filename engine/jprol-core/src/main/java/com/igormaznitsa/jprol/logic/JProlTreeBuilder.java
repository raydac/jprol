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

  public Result readPhraseAndMakeTree(final Reader reader) {
    PrologParser parser = parsers.get();
    if (parser == null) {
      parser = new GenericPrologParser(reader, this.context);
      parsers.set(parser);
    }

    try {
      final PrologTerm result = parser.next();
      return new Result(Terms.fromParsed(this.context, result), result.getLine(), result.getPos());
    } catch (NoSuchElementException ex) {
      parsers.remove();
      return null;
    }
  }

  public static final class Result {
    public final Term term;
    public final int line;
    public final int pos;

    Result(final Term term, final int line, final int pos) {
      this.term = term;
      this.line = line;
      this.pos = pos;
    }
  }
}
