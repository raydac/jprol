package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
@JProlOperator(priority = 1200, type = OpAssoc.FY, name = "@")
public class JProlJsonLibrary extends AbstractJProlLibrary {

  private static final Term JSON_TERM = Terms.newAtom("json");
  private static final Term[] EMPTY_TERM_ARRAY = new Term[0];

  public JProlJsonLibrary() {
    super("jprol-json-lib");
  }

  private static void skipWhitespaces(final String text, final AtomicInteger index) {
    while (index.get() < text.length()) {
      final char c = text.charAt(index.getAndIncrement());
      if (!Character.isWhitespace(c)) {
        index.decrementAndGet();
        break;
      }
    }
  }

  private static boolean isEndOfString(final String text, final AtomicInteger index) {
    return index.get() >= text.length();
  }

  private static char skipWhitespaceAndReadChar(final String text, final AtomicInteger index) {
    skipWhitespaces(text, index);
    if (isEndOfString(text, index)) {
      throw new IllegalArgumentException("Unexpected end of text");
    }
    return text.charAt(index.getAndIncrement());
  }

  private static String readJsonString(final String text, final AtomicInteger index) {
    final StringBuilder result = new StringBuilder();

    final int STATE_CHAR = 0;
    final int STATE_SPEC = 1;
    final int STATE_CODE = 2;

    int state = STATE_CHAR;

    final StringBuilder codeChar = new StringBuilder();

    while (!isEndOfString(text, index)) {
      final char c = text.charAt(index.getAndIncrement());
      switch (state) {
        case STATE_CHAR: {
          switch (c) {
            case '\"':
              return result.toString();
            case '\\':
              state = STATE_SPEC;
              break;
            default: {
              result.append(c);
            }
            break;
          }
        }
        break;
        case STATE_SPEC: {
          switch (c) {
            case 'b': {
              result.append('\b');
              state = STATE_CHAR;
            }
            break;
            case 'f': {
              result.append('\f');
              state = STATE_CHAR;
            }
            break;
            case 'n': {
              result.append('\n');
              state = STATE_CHAR;
            }
            break;
            case 'r': {
              result.append('\r');
              state = STATE_CHAR;
            }
            break;
            case 't': {
              result.append('\t');
              state = STATE_CHAR;
            }
            break;
            case '"': {
              result.append('\"');
              state = STATE_CHAR;
            }
            break;
            case '\\': {
              result.append('\\');
              state = STATE_CHAR;
            }
            break;
            case 'u': {
              state = STATE_CODE;
              codeChar.setLength(0);
            }
            break;
            default:
              throw new IllegalArgumentException("Unexpected special char: " + c);
          }
        }
        break;
        case STATE_CODE: {
          codeChar.append(c);
          if (codeChar.length() == 4) {
            try {
              result.append((char) Integer.parseInt(codeChar.toString(), 16));
            } catch (NumberFormatException ex) {
              throw new IllegalArgumentException(
                  "Wrong control code value: " + codeChar);
            }
            state = STATE_CHAR;
          }
        }
        break;
      }
    }
    throw new IllegalArgumentException("Detected unclosed JSON string");
  }

  private static TermList readJsonArray(
      final TermOperator equalsOperator,
      final TermOperator jsonMarkerOperator, final String json,
      final AtomicInteger index) {
    final List<Term> values = new ArrayList<>();
    boolean endFound = false;
    while (!endFound && !isEndOfString(json, index)) {
      final char nextChar = skipWhitespaceAndReadChar(json, index);
      if (nextChar == ']') {
        endFound = true;
        continue;
      }
      if (nextChar == ',') {
        values.add(extractJsonValue(equalsOperator, jsonMarkerOperator, json, index));
      } else {
        index.decrementAndGet();
        values.add(extractJsonValue(equalsOperator, jsonMarkerOperator, json, index));
      }
    }
    if (endFound) {
      return TermList.asList(values);
    } else {
      throw new IllegalArgumentException("Unclosed JSON array");
    }
  }

  private static Term extractJsonValue(
      final TermOperator equalsOperator,
      final TermOperator jsonMarkerOperator,
      final String json,
      final AtomicInteger index) {
    char nextChar = skipWhitespaceAndReadChar(json, index);
    final Term value;
    switch (nextChar) {
      case '{': {
        value = readJsonStruct(equalsOperator, jsonMarkerOperator, json, index);
      }
      break;
      case '[': {
        value = readJsonArray(equalsOperator, jsonMarkerOperator, json, index);
      }
      break;
      case '\"': {
        value = Terms.newAtom(readJsonString(json, index));
      }
      break;
      default: {
        final StringBuilder valueBuffer = new StringBuilder();
        valueBuffer.append(nextChar);
        boolean anyAlphabetic = false;
        boolean anyDot = false;
        while (!isEndOfString(json, index)) {
          nextChar = json.charAt(index.getAndIncrement());
          if (Character.isDigit(nextChar) || Character.isAlphabetic(nextChar) || nextChar == '.' ||
              nextChar == '-') {
            anyDot |= nextChar == '.';
            anyAlphabetic |= Character.isAlphabetic(nextChar);
            valueBuffer.append(nextChar);
          } else {
            index.decrementAndGet();
            break;
          }
        }
        final String strValue = valueBuffer.toString();
        if (anyAlphabetic) {
          if ("null".equals(strValue) || "true".equals(strValue) || "false".equals(strValue)) {
            value = Terms.newStruct(jsonMarkerOperator, new Term[] {Terms.newAtom(strValue)});
          } else {
            throw new IllegalArgumentException(
                "Unexpected special JSON value, allowed only ['null','true','false']: " + strValue);
          }
        } else if (anyDot) {
          try {
            value = Terms.newDouble(strValue);
          } catch (NumberFormatException ex) {
            throw new IllegalArgumentException("Unexpected double format for JSON: " + strValue);
          }
        } else {
          try {
            value = Terms.newLong(strValue);
          } catch (NumberFormatException ex) {
            throw new IllegalArgumentException("Unexpected long format for JSON: " + strValue);
          }
        }
      }
      break;
    }
    return value;
  }

  private static TermStruct fromJson(final String jsonText,
                                     final TermOperator equalsOperator,
                                     final TermOperator jsonMarkerOperator) {
    final AtomicInteger index = new AtomicInteger(0);
    if (skipWhitespaceAndReadChar(jsonText, index) != '{') {
      throw new IllegalArgumentException("Wrong JSON format");
    }
    return readJsonStruct(equalsOperator, jsonMarkerOperator, jsonText, index);
  }

  private static TermStruct readJsonStruct(final TermOperator equalsOperator,
                                           final TermOperator jsonMarkerOperator, final String json,
                                           final AtomicInteger index) {
    final List<Term> terms = new ArrayList<>();
    int commaCounter = 0;
    while (index.get() < json.length()) {
      char nextChar = skipWhitespaceAndReadChar(json, index);
      if (nextChar == ',') {
        commaCounter++;
        if (commaCounter > 1) {
          throw new IllegalArgumentException("Detected several commas in JSON");
        }
        continue;
      } else {
        commaCounter = 0;
      }

      if (nextChar == '}') {
        return Terms.newStruct(JSON_TERM,
            new Term[] {TermList.asList(terms)});
      } else if (nextChar == '\"') {
        final String fieldName = readJsonString(json, index);
        nextChar = skipWhitespaceAndReadChar(json, index);
        if (nextChar == ':') {
          final TermStruct foundStruct =
              Terms.newStruct(equalsOperator, new Term[] {Terms.newAtom(fieldName),
                  extractJsonValue(equalsOperator, jsonMarkerOperator, json, index)});
          terms.add(foundStruct);
        } else {
          throw new IllegalArgumentException(
              "Unexpected char in JSON instead of field value delimiter: " + nextChar);
        }
      } else {
        throw new IllegalArgumentException("Unexpected char for JSON struct field: " + nextChar);
      }
    }
    throw new IllegalArgumentException("Unexpected end of JSON structure");
  }

  private static String valueConverter(Term value) {
    if (value.getTermType() == TermType.LIST) {
      final StringBuilder buffer = new StringBuilder();
      buffer.append('[');
      String comma = "";
      for (final Term listTerm : ((TermList) value)) {
        buffer.append(comma).append(valueConverter(listTerm));
        comma = ",";
      }
      buffer.append(']');
      return buffer.toString();
    } else if (value.getTermType() == TermType.STRUCT) {
      final TermStruct struct = (TermStruct) value;
      if ("@".equals(struct.getFunctor().getText()) && struct.getArity() == 1) {
        final String text = struct.getArgumentAt(0).tryGround().getText();
        if ("null".equalsIgnoreCase(text)) {
          return "null";
        } else if ("true".equalsIgnoreCase(text)) {
          return "true";
        } else if ("false".equalsIgnoreCase(text)) {
          return "false";
        } else {
          throw new ProlDomainErrorException("[@null,@true,@false]",
              struct.getArgumentAt(0).tryGround());
        }
      } else {
        return toJson((TermStruct) value);
      }
    } else {
      if (value instanceof NumericTerm) {
        return value.getText();
      } else {
        return '\"' + jsonEscape(value.getText()) + '\"';
      }
    }
  }

  static String toJson(final TermStruct struct) {
    final Term functor = struct.getFunctor();
    if ("json".equals(functor.getText()) && struct.getArity() == 1) {
      final Term element = struct.getArgumentAt(0).tryGround();
      if (element.isGround() && element.getTermType() == TermType.LIST) {
        final StringBuilder result = new StringBuilder();
        result.append('{');
        final TermList list = (TermList) element;
        for (final Term term : list) {
          Term grounded = term.tryGround();
          if (grounded.isGround()) {
            if (grounded.getTermType() == TermType.STRUCT) {
              final TermStruct termStruct = (TermStruct) grounded;
              if ("=".equals(termStruct.getFunctor().getText()) && termStruct.getArity() == 2) {
                final String name = termStruct.getArgumentAt(0).tryGround().getText();
                if (result.length() > 1) {
                  result.append(',');
                }
                result.append('\"').append(jsonEscape(name)).append("\":");
                result.append(valueConverter(termStruct.getArgumentAt(1).tryGround()));
              } else {
                throw new ProlDomainErrorException("name=value", term);
              }
            } else {
              throw new ProlDomainErrorException("name=value", term);
            }
          } else {
            throw new ProlTypeErrorException("ground", grounded);
          }
        }
        result.append('}');
        return result.toString();
      } else {
        throw new ProlTypeErrorException("list", element);
      }
    } else {
      throw new ProlDomainErrorException("json([])", struct);
    }
  }


  public static String jsonEscape(final String text) {
    StringBuilder sb = new StringBuilder(text.length() + 16);
    for (char c : text.toCharArray()) {
      switch (c) {
        case '\\':
        case '"':
        case '/':
          sb.append('\\').append(c);
          break;
        case '\b':
          sb.append("\\b");
          break;
        case '\t':
          sb.append("\\t");
          break;
        case '\n':
          sb.append("\\n");
          break;
        case '\f':
          sb.append("\\f");
          break;
        case '\r':
          sb.append("\\r");
          break;
        default:
          if (c < ' ') {
            sb.append(String.format("\\u%04x", (int) c));
          } else {
            sb.append(c);
          }
          break;
      }
    }
    return sb.toString();
  }

  @JProlPredicate(determined = true, signature = "to_json/2", args = {
      "+compound,?term"}, reference = "Convert a structure into JSON text as an atom. Converted structure must be in format json([]).")
  public static boolean predicateTO_JSON(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    try {
      final Term converted = Terms.newAtom(toJson((TermStruct) argLeft));
      return argRight.unifyTo(converted);
    } catch (ProlException ex) {
      throw ex;
    } catch (Exception ex) {
      throw new ProlCriticalError(ex);
    }
  }

  @JProlPredicate(determined = true, signature = "from_json/2", args = {
      "+atom,?term"}, reference = "Create term on base of a JSON string. The result structure is formatted as structure json([..]).")
  public static boolean predicateFROM_JSON(final JProlChoicePoint goal,
                                           final TermStruct predicate) {
    final Term argLeft = predicate.getArgumentAt(0).tryGround();
    final Term argRight = predicate.getArgumentAt(1).tryGround();

    try {
      final TermOperator jsonMarkerOperator =
          goal.getContext().findSystemOperatorForNameAndAssociativity("@", OpAssoc.FY);
      if (jsonMarkerOperator == null) {
        throw new ProlCriticalError("Can't find required operator @/1 FY");
      }
      final TermOperator equalsOperator =
          goal.getContext().findSystemOperatorForNameAndAssociativity("=", OpAssoc.XFX);
      if (equalsOperator == null) {
        throw new ProlCriticalError("Can't find required operator =/2 XFX");
      }

      final Term converted = fromJson(argLeft.getText(), equalsOperator, jsonMarkerOperator);
      return argRight.unifyTo(converted);
    } catch (ProlException ex) {
      throw ex;
    } catch (IllegalArgumentException ex) {
      throw new ProlDomainErrorException("Valid JSON format expected", predicate.getArgumentAt(0),
          ex,
          predicate.getSourcePosition(), "json");
    } catch (Exception ex) {
      throw new ProlCriticalError(ex);
    }
  }
}
