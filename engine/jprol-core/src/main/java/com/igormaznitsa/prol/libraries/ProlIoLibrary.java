package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.annotations.Determined;
import com.igormaznitsa.prol.annotations.Predicate;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermList;
import com.igormaznitsa.prol.data.TermStruct;
import com.igormaznitsa.prol.data.Terms;
import com.igormaznitsa.prol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.prol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.prol.logic.ChoicePoint;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.tokenizer.TokenizerResult;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;

import java.io.*;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static com.igormaznitsa.prol.data.TermType.LIST;
import static com.igormaznitsa.prol.libraries.ProlCoreLibrary.predicateCALL;

public class ProlIoLibrary extends AbstractProlLibrary {

  private static final String CURRENT_STREAM_ID = "#current#";
  private static final Term END_OF_FILE = Terms.newAtom("end_of_file");
  private static final String WRITERS_MAP = "_io_writers_map_";
  private static final String READERS_MAP = "_io_readers_map_";

  @Predicate(Signature = "consult/1", Template = {"+atom", "+list"}, Reference = "Take an atom as the file name of the resource to be used for consultation, or a list contains resource name chain.")
  @Determined
  @SuppressWarnings("fallthrough")
  public boolean predicateCONSULT(final ChoicePoint goal, final TermStruct predicate) {
    final Term term = predicate.getElement(0).findNonVarOrSame();
    final ProlContext context = goal.getContext();

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

          final Term headterm = list.getHead().findNonVarOrSame();
          final Term tailTerm = list.getTail().findNonVarOrSame();

          if (tailTerm.getTermType() == LIST) {
            list = (TermList) tailTerm;
          } else {
            return false;
          }

          if (consultFromResource(context, headterm.getText())) {
            return false;
          }

        }
        return true;
      }
      default:
        return false;
    }
  }

  private boolean consultFromResource(final ProlContext context, final String resourceId) {
    try (final Reader reader = makeResourceReader(context, resourceId)) {
      context.consult(reader);
    } catch (IOException ex) {
      return false;
    }
    return true;
  }

  public ProlIoLibrary() {
    super("prol-io-lib");
  }

  private InternalReader makeResourceReader(final ProlContext context, final String resourceId) throws IOException {
    final Reader reader = context.findResourceReader(resourceId).orElseThrow(() -> new FileNotFoundException(resourceId));

    if (reader != null) {
      return new InternalReader(Terms.newAtom(resourceId), reader, true);
    } else if ("user".equals(resourceId)) {
      return new InternalReader(Terms.newAtom("user"), new InputStreamReader(System.in, Charset.defaultCharset()), false);
    } else {
      throw new FileNotFoundException(resourceId);
    }
  }

  private InternalWriter makeResourceWriter(final ProlContext context, final String resourceId, final boolean append) throws IOException {
    final Writer writer = context.findResourceWriter(resourceId, append).get();

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

  private InternalReader findCurrentInput(final ProlContext context, final Term goal) {
    final InternalReader current = getIoReaders(context).get(CURRENT_STREAM_ID);
    if (current == null) {
      return getIoReaders(context).computeIfAbsent("user", key -> new InternalReader(Terms.newAtom("user"), new InputStreamReader(System.in, Charset.defaultCharset()), false));
    }
    return current;
  }

  private InternalWriter findCurrentOutput(final ProlContext context, final Term goal) {
    final InternalWriter current = getIoWriters(context).get(CURRENT_STREAM_ID);
    if (current == null) {
      return getIoWriters(context).computeIfAbsent("user", key -> new InternalWriter(Terms.newAtom("user"), new OutputStreamWriter(System.out, Charset.defaultCharset()), false));
    }
    return current;
  }

  private Map<String, InternalWriter> getIoWriters(final ProlContext context) {
    return this.findContextObject(context, WRITERS_MAP, id -> new ConcurrentHashMap<>());
  }

  private Map<String, InternalReader> getIoReaders(final ProlContext context) {
    return this.findContextObject(context, READERS_MAP, id -> new ConcurrentHashMap<>());
  }

  @Predicate(Signature = "put/1", Template = "+number", Reference = "Write a char for its code into the current output stream.")
  @Determined
  public final void predicatePUT(final ChoicePoint goal, final TermStruct predicate) {
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

  @Predicate(Signature = "get/1", Template = "?number", Reference = "Read next non-blank char code from the current input stream.")
  @Determined
  public final boolean predicateGET(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final Reader current = findCurrentInput(goal.getContext(), predicate);

    try {
      int code = -1;
      do {
        final int nextCode = current.read();
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
  }

  @Predicate(Signature = "get0/1", Template = "?number", Reference = "Read next char code from the current input stream.")
  @Determined
  public final boolean predicateGET0(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final Reader current = findCurrentInput(goal.getContext(), predicate);

    try {
      int code = -1;
      do {
        final int nextCode = current.read();
        if (nextCode < 0) {
          break;
        } else {
          code = nextCode;
          break;
        }
      } while (!Thread.currentThread().isInterrupted());
      return arg.unifyTo(Terms.newLong(code));
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("read", "text_input", predicate);
    }
  }

  @Predicate(Signature = "read/1", Reference = " Read  the next Prolog term from the current input stream.")
  @Determined
  public final boolean predicateRead(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrSame();
    final Reader current = getIoReaders(goal.getContext()).get(CURRENT_STREAM_ID);

    if (current == null) {
      throw new ProlPermissionErrorException("read", "text_input", predicate);
    } else {
      final PrologParser parser = new GenericPrologParser(current, goal.getContext());
      final TokenizerResult result = parser.getInternalTokenizer().readNextToken();

      final Term readTerm;
      if (result == null) {
        readTerm = END_OF_FILE;
      } else {
        readTerm = Terms.fromParsed(goal.getContext(), result.getResult());
      }

      return arg.unifyTo(readTerm);
    }
  }

  private String readCurrentUntilNl(final ProlContext context, final Term goal) {
    final Reader current = findCurrentInput(context, goal);
    final StringBuilderEx result = new StringBuilderEx("");
    try {
      while (Thread.currentThread().isInterrupted()) {
        final int nextChr = current.read();
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
  }

  @Predicate(Signature = "seen/0", Reference = "Close the current input stream.")
  @Determined
  public final void predicateSEEN(final ChoicePoint goal, final TermStruct predicate) {
    final Reader current = findCurrentInput(goal.getContext(), predicate);
    try {
      current.close();
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("close", "text_stream", predicate, ex);
    }
  }

  @Predicate(Signature = "see/1", Template = "+atom", Reference = "Open SrcDest for reading and make it the current input")
  @Determined
  public final void predicateSEE(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
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

  @Predicate(Signature = "tell/1", Template = "+atom", Reference = "Open SrcDest for writing and make it the current output")
  @Determined
  public final void predicateTELL(final ChoicePoint goal, final TermStruct predicate) {
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

  @Predicate(Signature = "write/1", Reference = "Write a term into the current output stream.")
  @Determined
  public final void predicateWrite(final ChoicePoint goal, final TermStruct predicate) {
    final Writer writer = findCurrentOutput(goal.getContext(), predicate);
    try {
      writer.write(predicate.getElement(0).forWrite());
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
    }
  }

  @Predicate(Signature = "seeing/1", Template = "?term", Reference = "Return the current input stream name.")
  @Determined
  public final boolean predicateSEEING(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
    final InternalReader current = this.getIoReaders(goal.getContext()).get(CURRENT_STREAM_ID);
    final Term result = current == null ? Terms.NULL_LIST : current.getStreamId();
    return arg.unifyTo(result);
  }

  @Predicate(Signature = "telling/1", Template = "?term", Reference = "Return the current output stream name.")
  @Determined
  public final boolean predicateTELLING(final ChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getElement(0).findNonVarOrDefault(null);
    final InternalWriter current = this.getIoWriters(goal.getContext()).get(CURRENT_STREAM_ID);
    final Term result = current == null ? Terms.NULL_LIST : current.getStreamId();
    return arg.unifyTo(result);
  }

  @Predicate(Signature = "told/0", Reference = "Close the current output stream.")
  @Determined
  public final void predicateTOLD(final ChoicePoint goal, final TermStruct predicate) {
    final InternalWriter current = this.findCurrentOutput(goal.getContext(), predicate);
    try {
      current.close();
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("close", "text_stream", predicate, ex);
    }
  }

  @Predicate(Signature = "nl/0", Reference = "Out the next line char symbol into current output stream")
  @Determined
  public final void predicateNL(final ChoicePoint goal, final TermStruct predicate) {
    try {
      this.findCurrentOutput(goal.getContext(), predicate).write('\n');
    } catch (IOException ex) {

    }
  }

  @Predicate(Signature = "time/1", Template = "+callable_term", Reference = "Execute  Goal just but  print used time, It supports choice point (!) for inside goal.")
  public final boolean predicateTime(final ChoicePoint goal, final TermStruct predicate) {
    final long time = System.nanoTime();
    final boolean result = predicateCALL(goal, predicate);
    final long timeInterval = ((System.nanoTime() - time) + 500L) / 1000L; //microseconds

    final InternalWriter writer = findCurrentOutput(goal.getContext(), predicate);
    try {
      writer.write(String.format("%% %d.%d ms", (timeInterval / 1000), (timeInterval % 1000)));
    } catch (IOException ex) {

    }

    if (!result) {
      goal.resetVariants();
    }
    return result;
  }

  @Predicate(Signature = "tab/1", Template = {"+integer"}, Reference = "Out a number of space symbols into current output stream")
  @Determined
  public final void predicateTAB(final ChoicePoint goal, final TermStruct predicate) {
    final InternalWriter writer = findCurrentOutput(goal.getContext(), predicate);

    final long spaces = predicate.getElement(0).toNumber().longValue();
    try {
      for (long li = 0; li < spaces; li++) {
        writer.write(" ");
      }
    } catch (IOException ex) {

    }
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
