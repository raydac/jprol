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
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;

public final class Utils {

  public static final TermOperator SIGNATURE_OPERATOR = new TermOperator(400, OpAssoc.YFX, "/");

  private Utils() {
  }

  public static TermList toCharCodeList(final Term term) {
    final String text = term.getText();

    if (text == null || text.isEmpty()) {
      return Terms.NULL_LIST;
    } else {
      final TermList result = createOrAppendToList(null, newLong(text.charAt(0)));
      TermList current = result;
      for (int i = 1; i < text.length(); i++) {
        current = createOrAppendToList(current, newLong(text.charAt(i)));
      }
      return result;
    }
  }

  public static int getJvmBitness() {
    String data = System.getProperty("sun.arch.data.model","");
    if (data.contains("32")) return 32;
    return 64;
  }
  
  public static TermList toCharList(final Term term) {
    final String text = term.getText();
    final int len = text.length();
    if (len == 0) {
      return Terms.NULL_LIST;
    }

    final StringBuilder buff = new StringBuilder(1);

    TermList resultList = null;
    TermList curList = null;

    for (int li = 0; li < len; li++) {
      buff.append(text.charAt(li));
      final Term newAtom = Terms.newAtom(buff.toString());
      buff.setLength(0);
      if (li == 0) {
        resultList = newList(newAtom);
        curList = resultList;
      } else {
        curList = createOrAppendToList(curList, newAtom);
      }
    }

    return resultList;
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
    return new CloseableIterator<T>() {
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
    final TermList newList = Terms.newList(term);

    if (nullableList != null && !nullableList.isNullList()) {
      newList.setTail(nullableList.getTail());
      nullableList.setTail(newList);
    }

    return newList;
  }

  public static String escapeSrc(final String string) {

    if (string.length() == 0) {
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

  public static String extractPredicateSignatureFromStructure(final Term term) {
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

    if (sig.length() > 0 && sig.charAt(0) == '\'') {
      sig = sig.substring(1);
      final int lastIndex = sig.lastIndexOf('/');
      if (lastIndex < 0) {
        sig = null;
      } else {
        final String arity = sig.substring(lastIndex + 1).trim();
        String name = sig.substring(0, lastIndex - 1).trim();
        if (name.length() > 0 && name.charAt(name.length() - 1) == '\'') {
          name = name.substring(0, name.length() - 1);
          sig = name + '/' + arity;
        } else {
          sig = null;
        }
      }
    }
    return sig;
  }

  public static String validateSignature(final String signature) {
    final String[] parsed =
        Objects.requireNonNull(signature, "Null signature not allowed").split("/");
    if (parsed.length == 2) {
      String str = parsed[0].trim();
      boolean quoted = false;
      if (str.length() != 0) {
        if (str.charAt(0) == '\'') {
          if (str.length() > 1 && str.charAt(str.length() - 1) == '\'') {
            str = str.substring(1, str.length() - 1);
            if (str.length() == 0) {
              return null;
            }
            quoted = true;
          } else {
            // wrong name, it must not contain '\'' as the only symbol
            return null;
          }
        }

        final char firstChar = str.charAt(0);
        if (!quoted && (Character.isDigit(firstChar) || Character.isUpperCase(firstChar) ||
            Character.isWhitespace(firstChar) || firstChar == '.')) {
          return null;
        }

        // ok. the first part is ok, check the second part
        final int arity;
        try {
          arity = Integer.parseInt(parsed[1].trim());
          if (arity < 0) {
            throw new NumberFormatException("Negate number is not supported as arity");
          }
        } catch (NumberFormatException ex) {
          return null;
        }

        final StringBuilder builder = new StringBuilder(signature.length());

        if (quoted) {
          builder.append('\'').append(str).append('\'').append('/').append(arity);
        } else {
          builder.append(str).append('/').append(arity);
        }

        return builder.toString();
      }
    }
    return null;
  }

}
