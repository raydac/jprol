package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.annotations.JProlOperator;
import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.utils.ProlAssertions;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;

@JProlOperator(priority = 1200, type = OpAssoc.FY, name = "@")
public class JProlJsonLibrary extends AbstractJProlLibrary {

  public JProlJsonLibrary() {
    super("jprol-json-lib");
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
        final String text = struct.getElement(0).findNonVarOrSame().getText();
        if ("null".equalsIgnoreCase(text)) {
          return "null";
        } else if ("true".equalsIgnoreCase(text)) {
          return "true";
        } else if ("false".equalsIgnoreCase(text)) {
          return "false";
        } else {
          throw new ProlDomainErrorException("[null,true,false]",
              struct.getElement(0).findNonVarOrSame());
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
      final Term element = struct.getElement(0).findNonVarOrSame();
      if (element.isGround() && element.getTermType() == TermType.LIST) {
        final StringBuilder result = new StringBuilder();
        result.append('{');
        final TermList list = (TermList) element;
        for (final Term term : list) {
          Term grounded = term.findNonVarOrSame();
          if (grounded.isGround()) {
            if (grounded.getTermType() == TermType.STRUCT) {
              final TermStruct termStruct = (TermStruct) grounded;
              if ("=".equals(termStruct.getFunctor().getText()) && termStruct.getArity() == 2) {
                final String name = termStruct.getElement(0).findNonVarOrSame().getText();
                if (result.length() > 1) {
                  result.append(',');
                }
                result.append('\"').append(jsonEscape(name)).append("\":");
                result.append(valueConverter(termStruct.getElement(1).findNonVarOrSame()));
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
      "+compound,?atom"}, reference = "Convert a structure into JSON text as an atom. Converted structure must be in format json([]).")
  public static boolean predicateTO_JSON(final JProlChoicePoint goal, final TermStruct predicate) {
    final Term argLeft = predicate.getElement(0).findNonVarOrSame();
    final Term argRight = predicate.getElement(1).findNonVarOrSame();

    if (goal.isArgsValidate()) {
      ProlAssertions.assertStruct(argLeft);
    }

    try {
      final Term converted = Terms.newAtom(toJson((TermStruct) argLeft));
      return argRight.unifyTo(converted);
    } catch (ProlException ex) {
      throw ex;
    } catch (Exception ex) {
      throw new ProlCriticalError(ex);
    }
  }
}
