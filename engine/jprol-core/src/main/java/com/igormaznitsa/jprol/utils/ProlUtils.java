/*
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.igormaznitsa.jprol.utils;

import static com.igormaznitsa.jprol.data.TermType.ATOM;
import static com.igormaznitsa.jprol.data.Terms.newList;
import static com.igormaznitsa.jprol.data.Terms.newLong;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.SourcePosition;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class ProlUtils {

  public static final TermOperator SIGNATURE_OPERATOR =
      new TermOperator(400, OpAssoc.YFX, "/", SourcePosition.UNKNOWN);

  private ProlUtils() {
  }

  public static String readAsString(final File file, final Charset charset) throws IOException {
    final StringBuilder builder = new StringBuilder();
    try (Reader reader = new BufferedReader(
        new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
      while (!Thread.currentThread().isInterrupted()) {
        final int next = reader.read();
        if (next < 0) {
          break;
        }
        builder.append((char) next);
      }
    }
    return builder.toString();
  }

  public static TermList toCharCodeList(final Term term) {
    final String text = term.getText();

    if (text == null || text.isEmpty()) {
      return Terms.NULL_LIST;
    } else {
      final TermList result =
          createOrAppendToList(null, newLong(text.charAt(0), term.getSourcePosition()));
      TermList current = result;
      for (int i = 1; i < text.length(); i++) {
        current = createOrAppendToList(current, newLong(text.charAt(i), term.getSourcePosition()));
      }
      return result;
    }
  }

  public static int getJvmBitness() {
    String data = System.getProperty("sun.arch.data.model", "");
    if (data.contains("32")) {
      return 32;
    }
    return 64;
  }

  public static TermList toCharCodeList(final String text, final SourcePosition sourcePosition) {
    if (text == null || text.isEmpty()) {
      return Terms.newList(sourcePosition);
    }
    final List<Term> codes = new ArrayList<>();
    for (int i = 0; i < text.length(); i++) {
      codes.add(Terms.newLong(text.charAt(i)));
    }
    return TermList.asList(codes, sourcePosition);
  }

  public static TermList toCharList(final Term term, final SourcePosition sourcePosition) {
    final String text = term.getText();
    final int len = text.length();
    if (len == 0) {
      if (sourcePosition == null) {
        return Terms.NULL_LIST;
      } else {
        return Terms.newList(sourcePosition);
      }
    }

    final StringBuilder buff = new StringBuilder(1);

    TermList resultList = null;
    TermList curList = null;

    for (int li = 0; li < len; li++) {
      buff.append(text.charAt(li));
      final Term newAtom = Terms.newAtom(buff.toString(), term.getSourcePosition());
      buff.setLength(0);
      if (li == 0) {
        resultList = newList(newAtom, term.getSourcePosition());
        curList = resultList;
      } else {
        curList = createOrAppendToList(curList, newAtom);
      }
    }

    return resultList;
  }

  public static <T> List<T> listToMappedValues(final TermList list,
                                               final boolean includeNonListTail,
                                               final Function<Term, T> mapper) {
    if (list == null) {
      return List.of();
    }
    return Stream.of(list.toArray(includeNonListTail)).map(mapper).collect(Collectors.toList());
  }

  public static Throwable[] extractErrors(final List<CompletableFuture<Term>> futures) {
    return futures.stream().filter(CompletableFuture::isCompletedExceptionally).map(x -> {
      try {
        x.join();
      } catch (Throwable e) {
        if (!(e instanceof CancellationException)) {
          return e;
        }
      }
      return null;
    }).filter(Objects::nonNull).toArray(Throwable[]::new);
  }

  public static <T> CloseableIterator<T> makeCloseableIterator(final Iterator<T> iterator,
                                                               final Runnable onClose) {
    return new CloseableIterator<>() {
      private final Iterator<T> wrapped = iterator;

      @Override
      public void close() {
        if (onClose != null) {
          onClose.run();
        }
      }

      @Override
      public boolean hasNext() {
        return this.wrapped.hasNext();
      }

      @Override
      public T next() {
        return this.wrapped.next();
      }
    };
  }

  public static void writeAsUtf8(final File file, final CharSequence seq) throws IOException {
    try (Writer writer = new BufferedWriter(
        new OutputStreamWriter(new FileOutputStream(file, false), StandardCharsets.UTF_8))) {
      writer.write(seq.toString());
      writer.flush();
    }
  }

  public static String readAsUtf8(final File file) throws IOException {
    try (BufferedReader reader = new BufferedReader(
        new InputStreamReader(Files.newInputStream(file.toPath()), StandardCharsets.UTF_8))) {
      final StringBuilder buffer =
          new StringBuilder((int) file.length() < 0 ? 16384 : (int) file.length());
      while (!Thread.currentThread().isInterrupted()) {
        final int chr = reader.read();
        if (chr < 0) {
          break;
        }
        buffer.append((char) chr);
      }
      return buffer.toString();
    }
  }

  public static TermList createOrAppendToList(final TermList nullableList, final Term term) {
    return createOrAppendToList(nullableList, term, null);
  }

  public static TermList createOrAppendToList(final TermList nullableList, final Term term,
                                              final Object payload) {
    final TermList newList = Terms.newList(term, payload, term.getSourcePosition());

    if (nullableList != null && !nullableList.isNullList()) {
      newList.setTail(nullableList.getTail());
      nullableList.setTail(newList);
    }

    return newList;
  }

  public static String escapeSrc(final String string) {

    if (string.isEmpty()) {
      return string;
    }

    final StringBuilder builder = new StringBuilder(string.length());

    for (int li = 0; li < string.length(); li++) {
      final char curChar = string.charAt(li);

      switch (curChar) {
        case '\\':
          builder.append("\\\\");
          break;
        case '\'':
          builder.append("\\'");
          break;
        case '\"':
          builder.append("\\\"");
          break;
        case '\n':
          builder.append("\\n");
          break;
        case '\f':
          builder.append("\\f");
          break;
        case '\r':
          builder.append("\\r");
          break;
        case '\t':
          builder.append("\\t");
          break;
        case '_': {
          builder.append('_');
        }
        break;
        case '%':
        case '.':
        default: {
          builder.append(curChar);
        }
        break;
      }
    }

    return builder.toString();
  }

  public static String indicatorAsStringOrNull(final Term term) {
    final TermStruct struct = term.findNonVarOrSame();

    if (struct.getArity() != 2) {
      return null;
    }
    final Term left = struct.getElement(0).findNonVarOrDefault(null);
    final Term right = struct.getElement(1).findNonVarOrDefault(null);

    if (right instanceof TermLong && left.getTermType() == ATOM) {
      return left.getText() + '/' + right.getText();
    }
    return null;
  }

  public static String normalizeSignature(final String signature) {
    if (signature == null) {
      return null;
    }
    String sig = signature.trim();

    if (!sig.isEmpty() && sig.charAt(0) == '\'') {
      sig = sig.substring(1);
      final int lastIndex = sig.lastIndexOf('/');
      if (lastIndex < 0) {
        sig = null;
      } else {
        final String arity = sig.substring(lastIndex + 1).trim();
        String name = sig.substring(0, lastIndex - 1).trim();
        if (!name.isEmpty() && name.charAt(name.length() - 1) == '\'') {
          name = name.substring(0, name.length() - 1);
          sig = name + '/' + arity;
        } else {
          sig = null;
        }
      }
    }
    return sig;
  }

  public static String asTimeString(final Duration duration) {
    if (duration == null) {
      return null;
    }
    final int hours = duration.toHoursPart();
    final int minutes = duration.toMinutesPart();
    final int seconds = duration.toSecondsPart();
    final int milliseconds = duration.toMillisPart();
    return String.format("%02d:%02d:%02d.%03d", hours, minutes, seconds, milliseconds);
  }

  public static ProlPair<String, Integer> parseSignaturePair(final String signature) {
    if (signature == null) {
      throw new NullPointerException("Null signature not allowed");
    }
    final int dividerIndex = signature.lastIndexOf('/');
    String namePart =
        signature.substring(0, dividerIndex < 0 ? signature.length() : dividerIndex).trim();
    final String arityPart = signature.substring(dividerIndex + 1).trim();
    if (namePart.length() > 2
        && namePart.charAt(0) == '\''
        && namePart.charAt(namePart.length() - 1) == '\''
    ) {
      namePart = namePart.substring(1, namePart.length() - 1);
    }
    final Integer arity;
    if (dividerIndex < 0) {
      arity = null;
    } else {
      try {
        arity = Integer.parseInt(arityPart);
      } catch (NumberFormatException ex) {
        throw new NumberFormatException("Arity is not numeric: " + signature);
      }
      if (arity < 0) {
        throw new NumberFormatException("Arity is negative one: " + signature);
      }
    }
    return ProlPair.makeOf(namePart, arity);
  }

  public static String reassembleSignatureOrNull(final String signature) {
    try {
      final ProlPair<String, Integer> parsed = parseSignaturePair(signature);
      final String functor = parsed.getLeft();
      final Integer arity = parsed.getRight();
      if (arity == null) {
        return null;
      }
      final boolean quoted = signature.charAt(0) == '\'';
      final char firstChar = functor.charAt(0);
      if (!quoted && (Character.isDigit(firstChar)
          || Character.isUpperCase(firstChar)
          || Character.isWhitespace(firstChar)
          || firstChar == '.')) {
        return null;
      }
      if (quoted) {
        return "'" + functor + "'/" + arity;
      } else {
        return functor + '/' + arity;
      }
    } catch (IllegalArgumentException ex) {
      return null;
    }
  }

  public static void validateJProlPredicate(final JProlPredicate predicate) {
    if (reassembleSignatureOrNull(predicate.signature()) == null) {
      throw new IllegalArgumentException("Wrong signature: " + predicate.signature());
    }
    final ProlPair<String, Integer> parsedSignature = parseSignaturePair(predicate.signature());
    for (final String s : predicate.synonyms()) {
      if (reassembleSignatureOrNull(s) == null) {
        throw new IllegalArgumentException("Wrong synonym: " + s);
      }
    }

    if (parsedSignature.getRight() > 0 && predicate.args().length == 0) {
      throw new IllegalArgumentException("Expected arguments description for predicate");
    }

    for (final String s : predicate.args()) {
      final String[] parsedArgs = s.split(",");
      if (parsedArgs.length != parsedSignature.getRight()) {
        throw new IllegalArgumentException(
            "Expected " + parsedSignature.getRight() + " arguments: " + s);
      }

      for (final String a : parsedArgs) {
        final String text = extractAndNormalize(s, a);

        switch (text) {
          case "term":
          case "atom":
          case "atomic":
          case "string":
          case "float":
          case "integer":
          case "number":
          case "callable":
          case "evaluable":
          case "list":
          case "goal":
          case "char":
          case "predicate_indicator":
          case "operator_specifier":
          case "compound_term":
          case "compound":
          case "var": {
            // all is ok
          }
          break;
          default:
            throw new IllegalArgumentException("Unexpected type: " + a);
        }
      }
    }
  }

  private static String extractAndNormalize(final String description, final String srcText) {
    final String normalized = srcText.trim().toLowerCase(Locale.ROOT);
    if (normalized.isEmpty()) {
      throw new IllegalArgumentException("Wrong arguments description: " + description);
    }
    final String text;
    if (normalized.startsWith("--") // At call time, the argument must be unbound.
        || normalized.startsWith("++") // At call time, the argument must be ground
    ) {
      text = normalized.substring(2);
    } else if (
        normalized.startsWith("-")
            // Argument is an output argument. It may or may not be bound at call-time.
            || normalized.startsWith("+")
            // At call time, the argument must be instantiated to a term satisfying some (informal) type specification.
            || normalized.startsWith("?")
            // At call time, the argument must be bound to a partial term (a term which may or may not be ground) satisfying some (informal) type specification.
            || normalized.startsWith(":")
            // Argument is a meta-argument, for example a term that can be called as goal. This flag implies +.
            || normalized.startsWith("@")
            // Argument will not be further instantiated than it is at call-time.
            || normalized.startsWith(
            "!") // Argument contains a mutable structure that may be modified
    ) {
      text = normalized.substring(1);
    } else {
      throw new IllegalArgumentException("Unexpected argument mode indicator: " + srcText);
    }
    return text;
  }

}
