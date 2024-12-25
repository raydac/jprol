package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.libs.JProlCoreLibrary.predicateCALL;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;
import java.io.FileNotFoundException;
import java.io.FilterWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
public class JProlIoLibrary extends AbstractJProlLibrary {

  public static final String CURRENT_STREAM_ID = "#current#";
  public static final Term END_OF_FILE = Terms.newAtom("end_of_file");

  private static final String WRITERS_MAP = "_io_writers_map_";
  private static final String READERS_MAP = "_io_readers_map_";

  public JProlIoLibrary() {
    super("jprol-io-lib");
  }

  @JProlPredicate(determined = true, signature = "consult/1", args = {"+atom",
      "+list"}, reference = "Take an atom as the file name of the resource to be used for consultation, or a list contains resource name chain.")
  public boolean predicateCONSULT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    final JProlContext context = goal.getContext();

    switch (term.getTermType()) {
      case ATOM: {
        return consultFromResource(context, term.getText());
      }
      case LIST: {
        TermList list = (TermList) term;

        while (!Thread.currentThread().isInterrupted()) {
          if (list.isNullList()) {
            return true;
          }

          final Term termHead = list.getHead().findNonVarOrSame();
          final Term termTail = list.getTail().findNonVarOrSame();

          if (termTail.getTermType() == LIST) {
            list = (TermList) termTail;
          } else {
            return false;
          }

          if (consultFromResource(context, termHead.getText())) {
            return false;
          }

        }
        return true;
      }
      default:
        return false;
    }
  }

  private boolean consultFromResource(final JProlContext context, final String resourceId) {
    try (final Reader reader = makeResourceReader(context, resourceId)) {
      context.consult(reader);
    } catch (IOException ex) {
      return false;
    }
    return true;
  }

  private InternalReader makeResourceReader(final JProlContext context, final String resourceId)
      throws IOException {
    final Reader reader = context.findResourceReader(resourceId)
        .orElseThrow(() -> new FileNotFoundException(resourceId));

    if (reader != null) {
      return new InternalReader(Terms.newAtom(resourceId), reader, true);
    } else if ("user".equals(resourceId)) {
      return new InternalReader(Terms.newAtom("user"),
          new InputStreamReader(System.in, Charset.defaultCharset()), false);
    } else {
      throw new FileNotFoundException(resourceId);
    }
  }

  private InternalWriter makeResourceWriter(final JProlContext context, final String resourceId,
                                            final boolean append) throws IOException {
    final Writer writer = context.findResourceWriter(resourceId, append).orElse(null);

    if (writer != null) {
      return new InternalWriter(Terms.newAtom(resourceId), writer, true);
    } else if ("user".equals(resourceId)) {
      return new InternalWriter(Terms.newAtom("user"),
          new OutputStreamWriter(System.out, Charset.defaultCharset()),
          false);
    } else if ("error".equals(resourceId)) {
      return new InternalWriter(Terms.newAtom("error"),
          new OutputStreamWriter(System.err, Charset.defaultCharset()), false);
    } else {
      throw new FileNotFoundException(resourceId);
    }
  }

  private Optional<InternalWriter> makeUserAsCurrentOut(final JProlContext context,
                                                        final boolean append) {
    if (getIoWriters(context).containsKey("user")) {
      return Optional.ofNullable(getIoWriters(context).get("user"));
    } else {
      final Optional<Writer> userWriter = context.findResourceWriter("user", append);
      return userWriter.map(writer -> {
        final InternalWriter result = new InternalWriter(Terms.newAtom("user"), writer, false);
        getIoWriters(context).put("user", result);
        return Optional.of(result);
      }).orElse(Optional.empty());
    }
  }

  private Optional<InternalReader> makeUserAsCurrentIn(final JProlContext context) {
    if (getIoReaders(context).containsKey("user")) {
      return Optional.ofNullable(getIoReaders(context).get("user"));
    } else {
      final Optional<Reader> userReader = context.findResourceReader("user");
      return userReader.map(reader -> {
        final InternalReader result = new InternalReader(Terms.newAtom("user"), reader, false);
        getIoReaders(context).put("user", result);
        return Optional.of(result);
      }).orElse(Optional.empty());
    }
  }

  private Optional<InternalReader> findCurrentInput(final JProlContext context, final Term goal) {
    final InternalReader current = getIoReaders(context).get(CURRENT_STREAM_ID);
    return current == null ? makeUserAsCurrentIn(context) : Optional.of(current);
  }

  private Optional<InternalWriter> findCurrentOutput(final JProlContext context, final Term goal) {
    final InternalWriter current = getIoWriters(context).get(CURRENT_STREAM_ID);
    return current == null ? makeUserAsCurrentOut(context, true) : Optional.of(current);
  }

  private Map<String, InternalWriter> getIoWriters(final JProlContext context) {
    return this.findContextObject(context, WRITERS_MAP, id -> new ConcurrentHashMap<>());
  }

  private Map<String, InternalReader> getIoReaders(final JProlContext context) {
    return this.findContextObject(context, READERS_MAP, id -> new ConcurrentHashMap<>());
  }

  @JProlPredicate(determined = true, signature = "put/1", args = "+number", reference = "Write a char for its code into the current output stream.")
  public final void predicatePUT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final Writer current = getIoWriters(goal.getContext()).get(CURRENT_STREAM_ID);

    if (current != null) {
      try {
        current.write(Character.toString((char) arg.toNumber().intValue()));
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
      }
    }
  }

  @JProlPredicate(determined = true, signature = "get/1", args = "?number", reference = "Read next non-blank char code from the current input stream.")
  public final boolean predicateGET(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    return findCurrentInput(goal.getContext(), predicate).map(reader -> {
      try {
        int code = -1;
        do {
          final int nextCode = reader.read();
          if (nextCode < 0) {
            break;
          } else {
            if (!(Character.isWhitespace(nextCode) || Character.isISOControl(nextCode))) {
              code = nextCode;
              break;
            }
          }
        } while (!Thread.currentThread().isInterrupted());
        return arg.unifyTo(Terms.newLong(code));
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("read", "text_input", predicate);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("read", "text_input", predicate));
  }

  @JProlPredicate(determined = true, signature = "get0/1", args = "?number", reference = "Read next char code from the current input stream.")
  public final boolean predicateGET0(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    return findCurrentInput(goal.getContext(), predicate).map(reader -> {
      try {
        int code = -1;
        final int nextCode = reader.read();
        if (nextCode >= 0) {
          code = nextCode;
        }
        return arg.unifyTo(Terms.newLong(code));
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("read", "text_input", predicate);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("read", "text_input", predicate));
  }

  @JProlPredicate(determined = true, signature = "read/1", reference = " Read  the next Prolog term from the current input stream.")
  public final boolean predicateRead(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final Optional<InternalReader> current = findCurrentInput(goal.getContext(), arg);

    if (current.isPresent()) {
      final PrologParser parser =
          new GenericPrologParser(current.get(), goal.getContext().getParserContext());
      PrologTerm nextTerm;
      try {
        nextTerm = parser.next();
      } catch (NoSuchElementException ex) {
        nextTerm = null;
      }

      final Term asTerm =
          nextTerm == null ? END_OF_FILE : Terms.fromParsed(goal.getContext(), nextTerm);

      return arg.unifyTo(asTerm);
    } else {
      throw new ProlPermissionErrorException("read", "text_input", predicate);
    }
  }

  private String readCurrentUntilNl(final JProlContext context, final Term goal) {
    return findCurrentInput(context, goal).map(reader -> {
      final StringBuilderEx result = new StringBuilderEx("");
      try {
        while (Thread.currentThread().isInterrupted()) {
          final int nextChr = reader.read();
          if (nextChr < 0 || nextChr == '\n') {
            break;
          }
          if (nextChr == '\b') {
            if (!result.isEmpty()) {
              result.pop();
            }
          } else {
            result.append((char) nextChr);
          }
        }
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("read", "text_input", goal);
      }
      return result.toString();
    }).orElseThrow(() -> new ProlPermissionErrorException("read", "text_input", goal));
  }

  @JProlPredicate(determined = true, signature = "seen/0", reference = "Close the current input stream.")
  public final boolean predicateSEEN(final JProlChoicePoint goal, final TermStruct predicate) {
    return findCurrentInput(goal.getContext(), predicate).map(reader -> {
      try {
        reader.close();
        return true;
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("close", "text_stream", predicate, ex);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("close", "text_stream", predicate));
  }


  @JProlPredicate(determined = true, signature = "see/1", args = "+atom", reference = "Open source for reading and make it the current input")
  public final void predicateSEE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final String name = arg.getText();

    try {
      final Map<String, InternalReader> readers = getIoReaders(goal.getContext());

      if (readers.containsKey(name)) {
        readers.put(CURRENT_STREAM_ID, readers.get(name));
      } else {
        final InternalReader reader = makeResourceReader(goal.getContext(), name);
        readers.put(CURRENT_STREAM_ID, reader);
      }
    } catch (FileNotFoundException ex) {
      throw new ProlExistenceErrorException("source_sink", predicate, ex);
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("create", "text_stream", predicate, ex);
    }
  }

  @JProlPredicate(determined = true, signature = "tell/1", args = "+atom", reference = "Open SrcDest for writing and make it the current output")
  public final void predicateTELL(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
    final String name = arg.getText();

    try {
      final Map<String, InternalWriter> writers = getIoWriters(goal.getContext());

      if (writers.containsKey(name)) {
        writers.put(CURRENT_STREAM_ID, writers.get(name));
      } else {
        final InternalWriter writer = makeResourceWriter(goal.getContext(), name, true);
        writers.put(CURRENT_STREAM_ID, writer);
      }
    } catch (FileNotFoundException ex) {
      throw new ProlExistenceErrorException("source_sink", predicate, ex);
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("create", "text_stream", predicate, ex);
    }
  }

  @JProlPredicate(determined = true, signature = "write/1", reference = "Write a term into the current output stream.")
  public final boolean predicateWrite(final JProlChoicePoint goal, final TermStruct predicate) {
    return findCurrentOutput(goal.getContext(), predicate).map(writer -> {
      try {
        writer.write(predicate.getElement(0).forWrite());
        return true;
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("write", "text_output", predicate));
  }

  @JProlPredicate(determined = true, signature = "writeln/1", reference = "Write a term and next line into the current output stream.")
  public final boolean predicateWriteln(final JProlChoicePoint goal, final TermStruct predicate) {
    return findCurrentOutput(goal.getContext(), predicate).map(writer -> {
      try {
        writer.write(predicate.getElement(0).forWrite());
        writer.write('\n');
        return true;
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("writeln", "text_output", predicate, ex);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("writeln", "text_output", predicate));
  }

  @JProlPredicate(determined = true, signature = "seeing/1", synonyms = "current_input/1", args = "?term", reference = "Return the current input stream name.")
  public final boolean predicateSEEING(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final InternalReader current = this.getIoReaders(goal.getContext()).get(CURRENT_STREAM_ID);
    final Term result = current == null ? Terms.NULL_LIST : current.getStreamId();
    return arg.unifyTo(result);
  }

  @JProlPredicate(determined = true, signature = "telling/1", synonyms = "current_output/1", args = "?term", reference = "Return the current output stream name.")
  public final boolean predicateTELLING(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final InternalWriter current = this.getIoWriters(goal.getContext()).get(CURRENT_STREAM_ID);
    final Term result = current == null ? Terms.NULL_LIST : current.getStreamId();
    return arg.unifyTo(result);
  }

  @JProlPredicate(determined = true, signature = "told/0", reference = "Close the current output stream.")
  public final boolean predicateTOLD(final JProlChoicePoint goal, final TermStruct predicate) {
    return this.findCurrentOutput(goal.getContext(), predicate).map(writer -> {
      try {
        writer.close();
        return true;
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("close", "text_stream", predicate, ex);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("close", "text_stream", predicate));
  }

  @JProlPredicate(determined = true, signature = "nl/0", reference = "Out the next line char symbol into current output stream")
  public final boolean predicateNL(final JProlChoicePoint goal, final TermStruct predicate) {
    return this.findCurrentOutput(goal.getContext(), predicate).map(writer -> {
      try {
        writer.write('\n');
        return true;
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("write", "text_stream", predicate, ex);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("write", "text_stream", predicate));
  }

  @JProlPredicate(signature = "time/1", args = "+callable", reference = "Execute  Goal just but  print used time, It supports choice point (!) for inside goal.")
  public boolean predicateTime(final JProlChoicePoint goal, final TermStruct predicate) {
    final long time = System.nanoTime();
    final boolean result = predicateCALL(goal, predicate);

    if (!result) {
      goal.cutVariants();
    }

    final long timeInterval = ((System.nanoTime() - time) + 500L) / 1000L; //microseconds

    return findCurrentOutput(goal.getContext(), predicate).map(writer -> {
      try {
        writer.write(String.format("%% %d.%d ms", (timeInterval / 1000), (timeInterval % 1000)));
        writer.flush();
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("write", "text_stream", predicate, ex);
      }
      return result;
    }).orElseThrow(() -> new ProlPermissionErrorException("write", "text_stream", predicate));
  }

  @SuppressWarnings("resource")
  @JProlPredicate(determined = true, signature = "tab/1", args = {
      "+integer"}, reference = "Out a number of space symbols into current output stream")
  public final boolean predicateTAB(final JProlChoicePoint goal, final TermStruct predicate)
      throws IOException {
    final long spaces = predicate.getElement(0).toNumber().longValue();
    final InternalWriter writer = findCurrentOutput(goal.getContext(), predicate)
        .orElseThrow(() -> new ProlPermissionErrorException("write", "text_stream", predicate));
    for (long li = 0; li < spaces; li++) {
      writer.write(" ");
    }
    return true;
  }

  private static class InternalReader extends PushbackReader {
    private final Term streamId;
    private final boolean closeable;

    InternalReader(final Term streamId, final Reader reader, final boolean closeable) {
      super(reader, 512);
      this.streamId = streamId;
      this.closeable = closeable;
    }

    Term getStreamId() {
      return this.streamId;
    }

    @Override
    public void close() throws IOException {
      if (this.closeable) {
        super.close();
      }
    }
  }

  private static class InternalWriter extends FilterWriter {
    private final Term streamId;
    private final boolean closeable;

    InternalWriter(final Term streamId, final Writer writer, final boolean closeable) {
      super(writer);
      this.streamId = streamId;
      this.closeable = closeable;
    }

    Term getStreamId() {
      return this.streamId;
    }

    @Override
    public void close() throws IOException {
      if (this.closeable) {
        super.close();
      }
    }
  }


}
