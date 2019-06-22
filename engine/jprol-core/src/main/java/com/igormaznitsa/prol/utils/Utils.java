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

package com.igormaznitsa.prol.utils;

import com.igormaznitsa.prol.data.*;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Comparator;

import static com.igormaznitsa.prol.data.TermType.ATOM;

public final class Utils {

  public static final Comparator<Term> TERM_COMPARATOR = Term::compareTermTo;

  public static final TermOperator SIGNATURE_OPERATOR = new TermOperator(400, OpAssoc.YFX, "/");

  private Utils() {
  }

  public static void doSilently(final RunnableWithException runnable) {
    try {
      runnable.run();
    } catch (Throwable e) {
    }
  }

  public static void writeAsUtf8(final File file, final CharSequence seq) throws IOException {
    try (Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file, false), StandardCharsets.UTF_8))) {
      writer.write(seq.toString());
      writer.flush();
    }
  }

  public static String readAsUtf8(final File file) throws IOException {
    try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), StandardCharsets.UTF_8))) {
      final StringBuilder buffer = new StringBuilder((int) file.length() < 0 ? 16384 : (int) file.length());
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
          builder.append("\\\'");
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
    if (signature == null) {
      throw new NullPointerException("Null signature detected");
    }
    final String[] parsed = signature.split("/");
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
        if (!quoted && (Character.isDigit(firstChar) || Character.isUpperCase(firstChar) || Character.isWhitespace(firstChar) || firstChar == '.')) {
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

  @FunctionalInterface
  public interface RunnableWithException {

    void run() throws Throwable;
  }
}
