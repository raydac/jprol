package com.igormaznitsa.jprol.utils;

import static com.igormaznitsa.jprol.utils.ProlUtils.fromCharCodeList;
import static java.lang.Character.isDigit;

import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermType;
import java.util.ArrayList;
import java.util.IllegalFormatException;
import java.util.List;

/**
 * Converts ISO Prolog format/2 strings to Java String.format compatible strings.
 * Handles common Prolog format directives and maps them to Java format specifiers.
 */
public final class PrologFormatConverter {

  /**
   * Converts a Prolog format string to Java String.format compatible string.
   *
   * @param prologFormat The Prolog format string with ~directives
   * @return Java-compatible format string with % specifiers
   */
  public static String convertFormat(final String prologFormat) {
    if (prologFormat == null) {
      return null;
    }

    StringBuilder result = new StringBuilder();
    int length = prologFormat.length();
    int i = 0;
    int outputArgIndex = 1; // For positional arguments in Java output

    while (i < length) {
      char c = prologFormat.charAt(i);

      if (c == '~' && i + 1 < length) {
        // Parse numeric modifiers (e.g., ~10, ~2f)
        int modifierStart = i + 1;
        int modifierEnd = modifierStart;

        while (modifierEnd < length &&
            (isDigit(prologFormat.charAt(modifierEnd)) ||
                prologFormat.charAt(modifierEnd) == '.' ||
                prologFormat.charAt(modifierEnd) == '*')) {
          modifierEnd++;
        }

        String modifier = "";
        char directive;

        if (modifierEnd > modifierStart && modifierEnd < length) {
          modifier = prologFormat.substring(modifierStart, modifierEnd);
          directive = prologFormat.charAt(modifierEnd);
        } else {
          directive = prologFormat.charAt(i + 1);
        }

        if (modifier.equals("*")) {
          outputArgIndex++;
        }

        switch (directive) {
          case 'd': // decimal integer
          case 'D': // decimal with separator
          case 'a': {// atom - treat as string
            result.append("%").append(outputArgIndex++).append("$s");
          }
          break;

          case 'p': // any type
          case 'k': // print canonical
          case 'c': // character code
          case 's': // string (list of character codes)
          case 'w': // write term - treat as general string
          case 'q': {// writeq (quoted) - treat as string
            result.append("%").append(outputArgIndex++).append("$s");
          }
          break;

          case 'F':
          case 'f': {// float
            int n = 6;
            if (!modifier.equals("*") && modifier.length() != 0) {
              n = Integer.parseInt(modifier);
              if (n == 0) {
                n = 1;
              }
            }
            result.append("%").append(outputArgIndex++).append("$");
            result.append(".").append(n);
            result.append("f");
          }
          break;

          case 'E':
          case 'e': { // exponential notation
            int n = 6;
            if (!modifier.equals("*") && modifier.length() != 0) {
              n = Integer.parseInt(modifier);
              if (n == 0) {
                n = 1;
              }
            }
            result.append("%").append(outputArgIndex++).append("$");
            result.append(".").append(n);
            result.append(directive == 'e' ? "e" : "E");
          }
          break;

          case 'G':
          case 'g': {// general floating point
            int n = 6;
            if (!modifier.equals("*") && modifier.length() != 0) {
              n = Integer.parseInt(modifier);
              if (n == 0) {
                n = 1;
              }
            }
            result.append("%").append(outputArgIndex++).append("$");
            result.append(".").append(n);
            result.append(directive == 'g' ? "g" : "G");
          }
          break;

          case 'R':
          case 'r': {// radix - needs special handling
            // Prolog ~2r, ~8r, ~16r for binary, octal, hex
            switch (modifier) {
              case "2": {
                // Binary - Java doesn't have native binary format
                result.append("%").append(outputArgIndex++)
                    .append("$s"); // Will need custom conversion
              }
              break;
              case "8": {
                result.append("%").append(outputArgIndex++).append("$o"); // Octal
              }
              break;
              case "16": {
                result.append("%").append(outputArgIndex++).append('$')
                    .append(directive == 'r' ? 'x' : 'X'); // Hex
              }
              break;
              default: {
                result.append("%").append(outputArgIndex++).append("$d"); // Default to decimal
              }
              break;
            }
          }
          break;

          case
              'i': {// ignore argument - don't add to format string, will be handled in convertArguments
            // Don't increment outputArgIndex - the argument is removed from convertedArgs
            // But we still need to move past this directive
          }
          break;

          case 'n': {// newline
            result.append("%n");
          }
          break;

          case 't': {// tab
            result.append("\t");
          }
          break;

          case '~': {// literal tilde
            result.append("~");
          }
          break;

          case '|': {// flush/column alignment - approximate
            // Java doesn't have direct equivalent, use space
            result.append(" ");
          }
          break;

          case '`': {// fill character - complex, approximate
            // Skip fill character directive
            i = skipToEndOfDirective(prologFormat, i);
            continue;
          }
          case '+': {// column position - approximate
            if (!modifier.isEmpty()) {
              int width = Integer.parseInt(modifier);
              result.append(" ".repeat(width));
            }
          }
          break;

          default: {
            // Unknown directive, keep as-is
            result.append("~").append(directive);
          }
          break;
        }

        // Move past the directive
        i = (modifierEnd > modifierStart && modifierEnd < length) ?
            modifierEnd + 1 : i + 2;

      } else if (c == '%') {
        // Escape % for Java format
        result.append("%%");
        i++;
      } else {
        result.append(c);
        i++;
      }
    }

    return result.toString();
  }

  /**
   * Converts arguments for special cases (like binary numbers) and filters ignored arguments.
   *
   * @param prologFormat Original Prolog format string
   * @param args         Arguments to format
   * @return Converted arguments array (with ignored arguments removed)
   */
  public static Object[] convertArguments(String prologFormat, Term[] args) {
    if (args == null) {
      return new Object[0];
    }

    List<Object> converted = new ArrayList<>();
    int argIndex = 0;
    int i = 0;

    final StringBuilder numericBuffer = new StringBuilder();

    while (i < prologFormat.length() && argIndex < args.length) {
      if (prologFormat.charAt(i) == '~' && i + 1 < prologFormat.length()) {
        numericBuffer.setLength(0);

        // Skip numeric modifiers
        int j = i + 1;
        while (j < prologFormat.length() &&
            (isDigit(prologFormat.charAt(j)) ||
                prologFormat.charAt(j) == '.') || prologFormat.charAt(j) == '*') {
          numericBuffer.append(prologFormat.charAt(j));
          j++;
        }

        if (j >= prologFormat.length()) {
          break;
        }

        char directive = prologFormat.charAt(j);

        // Check for binary conversion
        if (j > i + 1) {
          String modifier = prologFormat.substring(i + 1, j);
          if (directive == 'r' && modifier.equals("2")) {
            // Convert number to binary string
            final Term arg = args[argIndex];
            if (arg instanceof NumericTerm) {
              long value = Math.round(arg instanceof TermLong ? arg.toNumber().longValue() :
                  arg.toNumber().doubleValue());
              converted.add(Long.toBinaryString(value));
            } else {
              converted.add(arg.forWrite());
            }
            argIndex++;
            i = j + 1;
            continue;
          }
        }

        // Check for ignore directive - skip the argument entirely
        if (directive == 'i') {
          argIndex++; // Skip this argument in input, don't add to output
          i = j + 1;
          continue;
        }

        // Check if this directive takes an argument
        if ("adDwqscfFeEgGrRkp".indexOf(directive) != -1) {
          Number number = null;
          final String accumulatedNumber = numericBuffer.toString();
          if (accumulatedNumber.length() != 0) {
            if ("*".equals(accumulatedNumber)) {
              final Term arg = args[argIndex++].tryGround();
              number = arg instanceof TermLong ? arg.toNumber().longValue() :
                  arg.toNumber().doubleValue();
            } else {
              try {
                number = Long.parseLong(accumulatedNumber);
              } catch (NumberFormatException ex) {
                number = Double.parseDouble(accumulatedNumber);
              }
            }
          }

          final Term arg = args[argIndex++].tryGround();
          final Object javaObject;
          if (arg instanceof NumericTerm) {
            switch (directive) {
              case 'c': { // char code
                final int repeat = number == null ? 1 : number.intValue();
                javaObject = ("" + (char) arg.toNumber().intValue()).repeat(repeat);
              }
              break;
              case 'R':
              case 'r': { // radix
                javaObject = Math.round(arg instanceof TermLong ? arg.toNumber().longValue() :
                    arg.toNumber().doubleValue());
              }
              break;
              case 'D':
              case 'd': { // integer
                final int afterDot = number == null ? 0 : number.intValue();
                String tempString;
                if (arg instanceof TermLong) {
                  tempString = Long.toString(arg.toNumber().longValue());
                } else if (arg instanceof TermDouble) {
                  tempString = Long.toString(Math.round(arg.toNumber().doubleValue()));
                } else {
                  tempString = arg.forWrite();
                }

                if (afterDot > 0 && afterDot <= tempString.length()) {
                  final int pos = tempString.length() - afterDot;
                  tempString = tempString.substring(0, pos) + '.' +
                      tempString.substring(pos, tempString.length());
                }

                if (directive == 'D') {
                  javaObject = group3FromRight(tempString, afterDot);
                } else {
                  javaObject = tempString;
                }
              }
              break;
              case 'g':
              case 'G':
              case 'E':
              case 'e':
              case 'F':
              case 'f': { // float
                javaObject = arg.toNumber().doubleValue();
              }
              break;
              default: {
                javaObject = arg instanceof TermLong ? arg.toNumber().longValue() :
                    arg.toNumber().doubleValue();
              }
            }
          } else {
            if (directive == 'q') {
              javaObject = arg.toSrcString();
            } else if (directive == 'w') {
              javaObject = arg.forWrite();
            } else {
              if (arg.getTermType() == TermType.LIST) {
                javaObject = fromCharCodeList((TermList) arg);
              } else {
                javaObject = arg.forWrite();
              }
            }
          }
          converted.add(javaObject);
        }

        i = j + 1;
      } else {
        i++;
      }
    }

    return converted.toArray();
  }

  private static String group3FromRight(final String s, final int offsetFromRight) {
    StringBuilder sb = new StringBuilder();
    int count = 0;

    for (int i = s.length() - 1; i >= Math.max(0, s.length() - offsetFromRight - 1); i--) {
      sb.append(s.charAt(i));
    }

    for (int i = s.length() - offsetFromRight - 2; i >= 0; i--) {
      sb.append(s.charAt(i));
      if (++count == 3 && i > 0) {
        sb.append(',');
        count = 0;
      }
    }
    return sb.reverse().toString();
  }

  /**
   * Format string using Prolog-style format string. Format exception will be hidden and replaced by String contains error message.
   *
   * @param prologFormat Prolog format string
   * @param args         Arguments
   * @return Formatted string
   */
  public static String format(final String prologFormat, final Term... args) {
    String javaFormat = convertFormat(prologFormat);
    Object[] convertedArgs = convertArguments(prologFormat, args);

    try {
      return String.format(javaFormat, convertedArgs);
    } catch (IllegalFormatException e) {
      return "Format error: " + e.getMessage() +
          "\nProlog format: " + prologFormat +
          "\nJava format: " + javaFormat +
          "\nArgs count: " + convertedArgs.length;
    }
  }

  private static int skipToEndOfDirective(String format, int start) {
    int i = start + 2; // Skip ~`
    while (i < format.length()) {
      char c = format.charAt(i);
      if (c == '|' || c == '+' || c == 't') {
        return i + 1;
      }
      i++;
    }
    return i;
  }
}