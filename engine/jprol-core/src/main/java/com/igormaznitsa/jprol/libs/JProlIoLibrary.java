package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;
import static com.igormaznitsa.jprol.data.TermType.LIST;
import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static com.igormaznitsa.jprol.libs.JProlCoreLibrary.predicateCALL;
import static com.igormaznitsa.jprol.utils.ProlUtils.fromCharCodeList;
import static com.igormaznitsa.jprol.utils.ProlUtils.readAsString;
import static java.util.Objects.requireNonNullElse;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.exceptions.ProlRepresentationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.jprol.utils.ProlPair;
import com.igormaznitsa.jprol.utils.PrologFormatConverter;
import com.igormaznitsa.prologparser.GenericPrologParser;
import com.igormaznitsa.prologparser.PrologParser;
import com.igormaznitsa.prologparser.terms.PrologTerm;
import com.igormaznitsa.prologparser.utils.StringBuilderEx;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FilterWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.Writer;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
public class JProlIoLibrary extends AbstractJProlLibrary {

  public static final String CURRENT_STREAM_ID = "#current#";
  public static final Term END_OF_FILE = newAtom("end_of_file");

  private static final String WRITERS_MAP = "_io_writers_map_";
  private static final String READERS_MAP = "_io_readers_map_";

  public JProlIoLibrary() {
    super("jprol-io-lib");
  }

  private void consultFromText(final JProlContext prolContext, final String text) {
    prolContext.consult(new StringReader(text));
  }

  private void consultFromUri(final JProlContext context, final URI uri, final Term term) {
    final String scheme = uri.getScheme();
    final String authorityAndPath =
        requireNonNullElse(uri.getAuthority(), "") + requireNonNullElse(uri.getPath(), "");

    if ("file".equalsIgnoreCase(scheme)) {
      final File file = new File(context.getCurrentFolder(), authorityAndPath);
      if (!file.isFile()) {
        throw new ProlExistenceErrorException("file",
            "Can't find file: " + file.getAbsolutePath(), term);
      }
      if (!file.canRead()) {
        throw new ProlPermissionErrorException("read", "image_input", term);
      }
      try {
        consultFromText(context, readAsString(file, StandardCharsets.UTF_8));
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("read", "text_input", term, ex);
      }
    } else if ("resource".equalsIgnoreCase(scheme) || scheme == null) {
      this.consultFromResource(context, authorityAndPath, term);
    } else if ("classpath".equalsIgnoreCase(scheme)) {
      final InputStream stream = context.getClass().getResourceAsStream(authorityAndPath);
      if (stream == null) {
        throw new ProlExistenceErrorException("classpath",
            "Can't find resource: " + authorityAndPath,
            term);
      }
      final StringBuilder buffer = new StringBuilder();
      try (InputStreamReader reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
        while (true) {
          int nextChar = reader.read();
          if (nextChar < 0) {
            break;
          }
          buffer.append((char) nextChar);
        }
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("read", "text_input", term, ex);
      }
      consultFromText(context, buffer.toString());
    } else {
      throw new ProlDomainErrorException("scheme", "Unsupported scheme: " + uri, term);
    }

  }

  private void consultFromResource(final JProlContext context, final String resourceId,
                                   final Term term) {
    try (final Reader reader = makeResourceReader(context, resourceId, term)) {
      context.consult(reader);
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("read", "resource_input", term, ex);
    }
  }

  @JProlPredicate(determined = true, signature = "consult/1", validate = {"+atom",
      "+list"}, guarded = true, reference = "Take an atom as URI of resource, or a list contains resource URIs and load all them as Prolog sources. Allowed schemes 'file:', 'resource:', 'classpath'")
  public void predicateCONSULT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argument = predicate.getArgumentAt(0).tryGround();

    switch (argument.getTermType()) {
      case ATOM: {
        try {
          this.consultFromUri(goal.getContext(), new URI(argument.getText()), predicate);
        } catch (URISyntaxException ex) {
          throw new ProlDomainErrorException("uri", "Malformed URI format: " + argument.getText(),
              predicate);
        }
      }
      break;
      case LIST: {
        TermList list = (TermList) argument;

        while (!goal.getContext().isDisposed()) {
          if (list.isNullList()) {
            break;
          }
          final Term head = list.getHead().tryGround();
          final Term tail = list.getTail().tryGround();
          ProlAssertions.assertAtom(head);
          ProlAssertions.assertList(tail);
          this.consultFromResource(goal.getContext(), head.getText(), predicate);
          list = (TermList) tail;
        }
      }
      break;
      default: {
        throw new ProlTypeErrorException("atom", "Atom or atom list expected: " + predicate,
            predicate);
      }
    }
  }

  private InternalReader makeResourceReader(final JProlContext context, final String resourceId,
                                            final Term term)
      throws IOException {
    final Reader reader = context.findResourceReader(resourceId)
        .orElseThrow(() -> new FileNotFoundException(resourceId));

    if (reader != null) {
      return new InternalReader(newAtom(resourceId), reader, true);
    } else if ("user".equals(resourceId)) {
      return new InternalReader(newAtom("user"),
          new InputStreamReader(System.in, Charset.defaultCharset()), false);
    } else {
      throw new ProlExistenceErrorException("resource", "Can't find resource: " + resourceId, term);
    }
  }

  private InternalWriter makeResourceWriter(final JProlContext context, final String resourceId,
                                            final boolean append) throws IOException {
    final Writer writer = context.findResourceWriter(resourceId, append).orElse(null);

    if (writer != null) {
      return new InternalWriter(newAtom(resourceId), writer, true);
    } else if ("user".equals(resourceId)) {
      return new InternalWriter(newAtom("user"),
          new OutputStreamWriter(System.out, Charset.defaultCharset()),
          false);
    } else if ("error".equals(resourceId)) {
      return new InternalWriter(newAtom("error"),
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
        final InternalWriter result = new InternalWriter(newAtom("user"), writer, false);
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
        final InternalReader result = new InternalReader(newAtom("user"), reader, false);
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

  @JProlPredicate(guarded = true, determined = true, signature = "put/1", validate = "+number", reference = "Write a char for its code into the current output stream.")
  public final void predicatePUT(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    final Writer current = getIoWriters(goal.getContext()).get(CURRENT_STREAM_ID);

    if (current != null) {
      try {
        current.write(Character.toString((char) arg.toNumber().intValue()));
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
      }
    }
  }

  @JProlPredicate(guarded = true, determined = true, signature = "get/1", validate = "?number", reference = "Read next non-blank char code from the current input stream.")
  public final boolean predicateGET(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
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
        } while (!goal.getContext().isDisposed());

        if (goal.getContext().isDisposed()) {
          return false;
        }

        return arg.unifyWith(Terms.newLong(code));
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("read", "text_input", predicate);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("read", "text_input", predicate));
  }

  @JProlPredicate(
      signature = "format/1",
      synonyms = {"format/2", "format/3"},
      validate = {"+format", "+format,+list", "+atom,+format,+list"},
      determined = true,
      reference = "Print the message to the standard output stream"
  )
  public final boolean predicateFORMAT1_2_3(final JProlChoicePoint goal,
                                            final TermStruct predicate) {
    final Term output = predicate.getArity() == 3 ? predicate.getArgumentAt(0).tryGround() : null;
    final Term format = predicate.getArity() == 3 ? predicate.getArgumentAt(1).tryGround() :
        predicate.getArgumentAt(0).tryGround();

    final Term arguments;
    if (predicate.getArity() == 1) {
      arguments = TermList.NULL_LIST;
    } else {
      arguments = predicate.getArity() == 3 ? predicate.getArgumentAt(2).tryGround() :
          predicate.getArgumentAt(1).tryGround();
    }

    final String prologFormat;
    final String javaStringFormat;
    if (goal.getInternalObject() == null) {
      prologFormat =
          format instanceof TermList ? fromCharCodeList((TermList) format) : format.getText();
      javaStringFormat = PrologFormatConverter.convertFormat(prologFormat);
      goal.setInternalObject(ProlPair.makeOf(prologFormat, javaStringFormat));
    } else {
      final ProlPair<String, String> pair = goal.getInternalObject();
      prologFormat = pair.getLeft();
      javaStringFormat = pair.getRight();
    }

    if (arguments.getTermType() != LIST) {
      throw new ProlTypeErrorException("list", "Expected list", arguments);
    }

    final Term[] prepared = ((TermList) arguments).toArray(true);
    final String formatted;
    try {
      final Object[] convertedArgs = PrologFormatConverter.convertArguments(prologFormat, prepared);
      formatted = String.format(javaStringFormat, convertedArgs);
    } catch (Exception ex) {
      throw new ProlRepresentationErrorException("format", "Format error: " + ex.getMessage(),
          predicate);
    }

    return findCurrentOutput(goal.getContext(), predicate).map(writer -> {
      try {
        writer.write(formatted.toCharArray());
        return true;
      } catch (Exception ex) {
        throw new ProlPermissionErrorException("format", "text_output", predicate, ex);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("format", "text_output", predicate));
  }

  @JProlPredicate(guarded = true, determined = true, signature = "get0/1", validate = "?number", reference = "Read next char code from the current input stream.")
  public final boolean predicateGET0(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    return findCurrentInput(goal.getContext(), predicate).map(reader -> {
      try {
        int code = -1;
        final int nextCode = reader.read();
        if (nextCode >= 0) {
          code = nextCode;
        }
        return arg.unifyWith(Terms.newLong(code));
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("read", "text_input", predicate);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("read", "text_input", predicate));
  }

  @JProlPredicate(guarded = true, determined = true, signature = "read/1", validate = "-term",
      reference = "Read  the next Prolog term from the current input stream.")
  public final boolean predicateRead(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
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

      return arg.unifyWith(asTerm);
    } else {
      throw new ProlPermissionErrorException("read", "text_input", predicate);
    }
  }

  @JProlPredicate(guarded = true, determined = true, signature = "read_line_to_string/2", validate = "-term,?string", reference = "Read line of chars from input as atom.")
  public final boolean predicateReadLineToString(final JProlChoicePoint goal,
                                                 final TermStruct predicate) {
    final Term readerId = predicate.getArgumentAt(0).tryGround();
    final Term stringTerm = predicate.getArgumentAt(1).tryGround();

    final Optional<InternalReader> current = this.findCurrentInput(goal.getContext(), readerId);

    if (current.isPresent()) {
      final InternalReader internalReader = current.get();
      final StringBuilder buffer = new StringBuilder();

      while (!goal.getContext().isDisposed()) {
        try {
          final int nextChar = internalReader.read();
          if (nextChar < 0 || nextChar == '\n') {
            break;
          }
          buffer.append((char) nextChar);
        } catch (IOException ex) {
          throw new ProlPermissionErrorException("read", "text_input", goal.getGoalTerm());
        }
      }
      if (goal.getContext().isDisposed()) {
        return false;
      }
      return stringTerm.unifyWith(newAtom(buffer.toString()));
    } else {
      throw new ProlPermissionErrorException("read", "text_input", goal.getGoalTerm());
    }
  }

  private String readCurrentUntilNl(final JProlContext context, final Term goal) {
    return findCurrentInput(context, goal).map(reader -> {
      final StringBuilderEx result = new StringBuilderEx("");
      try {
        while (!context.isDisposed()) {
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


  @JProlPredicate(guarded = true, determined = true, signature = "see/1", validate = "+atom", reference = "Open source for reading and make it the current input")
  public final void predicateSEE(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    final String name = arg.getText();

    try {
      final Map<String, InternalReader> readers = getIoReaders(goal.getContext());

      if (readers.containsKey(name)) {
        readers.put(CURRENT_STREAM_ID, readers.get(name));
      } else {
        final InternalReader reader = makeResourceReader(goal.getContext(), name, predicate);
        readers.put(CURRENT_STREAM_ID, reader);
      }
    } catch (FileNotFoundException ex) {
      throw new ProlExistenceErrorException("source_sink", predicate, ex);
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("create", "text_stream", predicate, ex);
    }
  }

  @JProlPredicate(guarded = true, determined = true, signature = "tell/1", validate = "+atom", reference = "Open SrcDest for writing and make it the current output")
  public final void predicateTELL(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).findGroundOrDefault(null);
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

  @JProlPredicate(guarded = true, determined = true, signature = "write/1", validate = "?term", reference = "Write a term into the current output stream.")
  public final boolean predicateWrite(final JProlChoicePoint goal, final TermStruct predicate) {
    return findCurrentOutput(goal.getContext(), predicate).map(writer -> {
      try {
        writer.write(predicate.getArgumentAt(0).forWrite());
        return true;
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("write", "text_output", predicate));
  }

  @JProlPredicate(guarded = true, determined = true, signature = "writeln/1", validate = "?term", reference = "Write a term and next line into the current output stream.")
  public final boolean predicateWriteln(final JProlChoicePoint goal, final TermStruct predicate) {
    return findCurrentOutput(goal.getContext(), predicate).map(writer -> {
      try {
        writer.write(predicate.getArgumentAt(0).forWrite());
        writer.write('\n');
        return true;
      } catch (IOException ex) {
        throw new ProlPermissionErrorException("writeln", "text_output", predicate, ex);
      }
    }).orElseThrow(() -> new ProlPermissionErrorException("writeln", "text_output", predicate));
  }

  @JProlPredicate(guarded = true, determined = true, signature = "seeing/1", synonyms = "current_input/1", validate = "?term", reference = "Return the current input stream name.")
  public final boolean predicateSEEING(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    final InternalReader current = this.getIoReaders(goal.getContext()).get(CURRENT_STREAM_ID);
    final Term result = current == null ? NULL_LIST : current.getStreamId();
    return arg.unifyWith(result);
  }

  @JProlPredicate(guarded = true, determined = true, signature = "telling/1", synonyms = "current_output/1", validate = "?term", reference = "Return the current output stream name.")
  public final boolean predicateTELLING(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term arg = predicate.getArgumentAt(0).tryGround();
    final InternalWriter current = this.getIoWriters(goal.getContext()).get(CURRENT_STREAM_ID);
    final Term result = current == null ? NULL_LIST : current.getStreamId();
    return arg.unifyWith(result);
  }

  @JProlPredicate(guarded = true, determined = true, signature = "told/0", reference = "Close the current output stream.")
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

  @JProlPredicate(guarded = true, determined = true, signature = "nl/0", reference = "Out the next line char symbol into current output stream")
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

  @JProlPredicate(guarded = true, signature = "time/1", validate = "+callable", reference = "Execute  Goal just but  print used time, It supports choice point (!) for inside goal.")
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
  @JProlPredicate(guarded = true, determined = true, signature = "tab/1", validate = {
      "+integer"}, reference = "Out a number of space symbols into current output stream")
  public final boolean predicateTAB(final JProlChoicePoint goal, final TermStruct predicate)
      throws IOException {
    final long spaces = predicate.getArgumentAt(0).toNumber().longValue();
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
