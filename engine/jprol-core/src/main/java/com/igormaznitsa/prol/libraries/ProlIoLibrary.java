package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Predicate;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.io.ProlReader;
import com.igormaznitsa.prol.io.ProlStreamManager;
import com.igormaznitsa.prol.io.ProlWriter;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;

import java.io.IOException;
import java.io.Reader;
import java.util.Optional;

import static com.igormaznitsa.prol.data.Terms.NULL_LIST;
import static com.igormaznitsa.prol.data.Terms.newAtom;

public class ProlIoLibrary extends AbstractProlLibrary {

  public ProlIoLibrary() {
    super("prol-io-lib");
  }

  @Predicate(Signature = "consult/1", Template = {"+atom", "+list"}, Reference = "Take an atom as the file name of the resource to be used for consultation, or a list contains a resource name chain. The resource will be getted through the current ProlStreamManager.")
  @Determined
  @SuppressWarnings("fallthrough")
  public static boolean predicateCONSULT(final ChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();

    final ProlContext ctxt = goal.getContext();
    final ProlStreamManager streamManager = ctxt.getStreamManager();

    return false;

    //TODO

//    switch (term.getTermType()) {
//      case ATOM: {
//        String name = term.getText();
//
//        final Optional<ProlReader> reader = goal.getContext().getStreamManager().findReaderForId(goal.getContext(), term.getText());
//        if (reader.isPresent()) {
//          ctxt.consult();
//        } else {
//          return false;
//        }
//
//
//        return consultFromResource(name, ctxt, streamManager);
//      }
//      case LIST: {
//        TermList list = (TermList) term;
//
//        while (!Thread.currentThread().isInterrupted()) {
//          if (list.isNullList()) {
//            return true;
//          }
//          final Term headterm = list.getHead().findNonVarOrSame();
//          final Term tailTerm = list.getTail().findNonVarOrSame();
//          if (tailTerm.getTermType() == LIST) {
//            list = (TermList) tailTerm;
//          } else {
//            return false;
//          }
//
//          final String name = headterm.getText();
//          if (!consultFromResource(name, ctxt, streamManager)) {
//            return false;
//          }
//        }
//      }
//      default:
//        return false;
//    }
  }
//
//  @Predicate(Signature = "put/1", Template = "+number", Reference = "Write a char for its code into the current output stream.")
//  @Determined
//  public final void predicatePUT(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrSame();
//
//    goal.getContext().getOutWriter().ifPresent(outStream -> {
//      try {
//        outStream.writeChar(arg);
//      } catch (IOException ex) {
//        throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
//      }
//    });
//  }

//  @Predicate(Signature = "get/1", Template = "?number", Reference = "Read next non-blank char code from the current input stream.")
//  @Determined
//  public final boolean predicateGET(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrSame();
//    final Optional<ProlTextReader> getter = goal.getContext().getInReader();
//    getter.orElseThrow(() -> new ProlPermissionErrorException("read", "text_input", predicate));
//
//    final ProlTextReader inStream = getter.get();
//    try {
//      final Term nextchar = inStream.readChar();
//      return arg.unifyTo(nextchar);
//    } catch (IOException ex) {
//      throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
//    }
//  }
//
//  @Predicate(Signature = "get0/1", Template = "?number", Reference = "Read next char code from the current input stream.")
//  @Determined
//  public final boolean predicateGET0(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrSame();
//
//    final Optional<ProlReader> getter = goal.getContext().getInReader();
//    getter.orElseThrow(() -> new ProlPermissionErrorException("read", "text_input", predicate));
//
//    final ProlReader inStream = getter.get();
//    try {
//      Term read = null;
//      do {
//        final int nextChar = inStream.asReader().read();
//        if (nextChar < 0) {
//          read = Terms.newLong(nextChar);
//        } else {
//          final char chr = (char) nextChar;
//          if (!Character.isWhitespace(chr) && !Character.isISOControl(chr)) {
//            read = Terms.newLong(chr);
//          }
//        }
//      } while (read == null);
//      return arg.unifyTo(read);
//    } catch (IOException ex) {
//      throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
//    }
//  }

//  @Predicate(Signature = "read/1", Reference = " Read  the next Prolog term from the current input stream.")
//  @Determined
//  public final boolean predicateRead(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrSame();
//
//    final Optional<ProlTextReader> reader = goal.getContext().getInReader();
//    reader.orElseThrow(() -> new ProlPermissionErrorException("read", "text_input", predicate));
//
//    final ProlTextReader inStream = reader.get();
//    try {
//      Term term = inStream.readTerm();
//      if (term == null) {
//        term = ProlStream.END_OF_FILE;
//      }
//      return arg.unifyTo(term);
//    } catch (IOException ex) {
//      throw new ProlPermissionErrorException("read", "text_input", predicate, ex);
//    }
//  }
//

//  @Predicate(Signature = "readln/1", Reference = " Read  the next line (until NL symbol) from the current input stream as an atom. It sypports backspace to remove last symbol from buffer.")
//  @Determined
//  public final boolean predicateReadLn(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrSame();
//    return arg.unifyTo(newAtom(readFromCurrentInputStreamUntilNL(goal, predicate)));
//  }

//  @Predicate(Signature = "readint/1", Reference = " Read  an integer number (and ignore white space) until NL symbol from the current input stream as an integer atom or the end_of_file atom. It sypports backspace to remove last symbol from buffer. If the input string can't be converted to an integer atom, the predicate will return false.")
//  @Determined
//  public final boolean predicateReadInt(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrSame();
//    final String str = readFromCurrentInputStreamUntilNL(goal, predicate).trim();
//    Term term;
//    if (str.equals(ProlStream.END_OF_FILE_STR)) {
//      term = ProlStream.END_OF_FILE;
//    } else {
//      try {
//        term = newLong(str);
//      } catch (NumberFormatException ex) {
//        return false;
//      }
//    }
//    return arg.unifyTo(term);
//  }

  private static boolean consultFromProlStream(final ProlContext context, final ProlReader reader) {
    try {
      context.consult(new Reader() {
        @Override
        public int read(char[] cbuf, int off, int len) throws IOException {
          int counter = 0;
          int ccc;
          while (len > 0 && (ccc = reader.readChar()) >= 0) {
            cbuf[off++] = (char) ccc;
            counter++;
            len--;
          }
          return counter;
        }

        @Override
        public void close() throws IOException {

        }
      });
      return true;
    } finally {
      try {
        reader.close();
      } catch (IOException ex) {
      }
    }
  }
//
//  @Predicate(Signature = "seen/0", Reference = "Close the current input stream.")
//  @Determined
//  public final void predicateSEEN(final ChoicePoint goal, final TermStruct predicate) {
//    try {
//      goal.getContext().seen();
//    } catch (RuntimeException pex) {
//      final Throwable ex = pex.getCause() == null ? pex : pex.getCause();
//      throw new ProlPermissionErrorException("close", "text_stream", predicate, ex);
//    }
//  }

  @Predicate(Signature = "write/1", Reference = "Write a term into the current output stream.")
  @Determined
  public final void predicateWrite(final ChoicePoint goal, final TermStruct predicate) {
//    goal.getContext().getOutWriter().ifPresent(outStream -> {
//      try {
//        outStream.writeTerm(predicate.getElement(0));
//      } catch (IOException ex) {
//        throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
//      }
//    });
  }

  //  @Predicate(Signature = "readchar/1", Reference = " Read  char from the current input stream as an integer atom or the end_of_file atom")
//  @Determined
//  public final boolean predicateReadChar(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrSame();
//    final TermLong value = readChar(goal, predicate);
//    Term term = value.toNumber().longValue() < 0L ? ProlStream.END_OF_FILE : value;
//    return arg.unifyTo(term);
//  }
//
//  @Predicate(Signature = "readreal/1", Reference = " Read  an real number (and ignore white space) until NL symbol from the current input stream as an real atom or the end_of_file atom. It sypports backspace to remove last symbol from buffer. If the input string can't be converted to a real atom, the predicate will return false.")
//  @Determined
//  public final boolean predicateReadReal(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrSame();
//    final String str = readFromCurrentInputStreamUntilNL(goal, predicate).trim();
//    final Term term;
//    if (str.equals(ProlStream.END_OF_FILE_STR)) {
//      term = ProlStream.END_OF_FILE;
//    } else {
//      try {
//        term = newDouble(str);
//      } catch (NumberFormatException ex) {
//        return false;
//      }
//    }
//    return arg.unifyTo(term);
//  }
//
  @Predicate(Signature = "see/1", Template = "+atom", Reference = "Open SrcDest for reading and make it the current input")
  @Determined
  public final void predicateSEE(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
//    final String name = arg.getText();
//    try {
//      goal.getContext().see(name);
//    } catch (RuntimeException pex) {
//      final Throwable ex = pex.getCause() == null ? pex : pex.getCause();
//      if (ex instanceof FileNotFoundException) {
//        throw new ProlExistenceErrorException("source_sink", predicate, ex);
//      } else {
//        throw new ProlPermissionErrorException("create", "text_stream", predicate, ex);
//      }
//    }
  }

  @Predicate(Signature = "seeing/1", Template = "?term", Reference = "Return the current input stream name.")
  @Determined
  public final boolean predicateSEEING(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
    final Optional<ProlReader> reader = goal.getContext().getInReader();
    final Term result = reader.isPresent() ? newAtom(reader.get().getId()) : NULL_LIST;
    return arg.unifyTo(result);
  }

  @Predicate(Signature = "telling/1", Template = "?term", Reference = "Return the current output stream name.")
  @Determined
  public final boolean predicateTELLING(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
    final Optional<ProlWriter> outStream = goal.getContext().getOutWriter();
    final Term result = outStream.isPresent() ? newAtom(outStream.get().getId()) : NULL_LIST;
    return arg.unifyTo(result);
  }

  @Predicate(Signature = "told/0", Reference = "Close the current output stream.")
  @Determined
  public final void predicateTOLD(final ChoicePoint goal, final TermStruct predicate) {
//    goal.getContext().told();
  }

  @Predicate(Signature = "tell/1", Template = "+atom", Reference = "Open SrcDest for writing and make it the current output")
  @Determined
  public final void predicateTELL(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
//    final String name = arg.getText();
//    try {
//      goal.getContext().tell(name, false);
//    } catch (RuntimeException pex) {
//      final Throwable cause = pex.getCause() == null ? pex : pex.getCause();
//      if (cause instanceof FileNotFoundException) {
//        throw new ProlExistenceErrorException("source_sink", predicate, cause);
//      } else {
//        throw new ProlPermissionErrorException("create", "text_stream", predicate, cause);
//      }
//    }
  }

//  @Predicate(Signature = "append/1", Template = "+atom", Reference = "Open SrcDest to append new data and make it the current input")
//  @Determined
//  public final void predicateAPPEND(final ChoicePoint goal, final TermStruct predicate) {
//    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
//    final String name = arg.getText();
//    try {
//      goal.getContext().tell(name, true);
//    } catch (RuntimeException pex) {
//      final Throwable ex = pex.getCause() == null ? pex : pex.getCause();
//      if (ex instanceof FileNotFoundException) {
//        throw new ProlExistenceErrorException("source_sink", predicate, ex);
//      } else {
//        throw new ProlPermissionErrorException("create", "text_stream", predicate, ex);
//      }
//    }
//  }

}
