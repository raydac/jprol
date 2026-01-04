package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.utils.ProlUtils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import javax.script.ScriptContext;

public final class JProlScriptEngineUtils {
  private JProlScriptEngineUtils() {

  }

  public static JProlScriptEngineContext asJProlContext(final ScriptContext scriptContext) {
    if (scriptContext == null) {
      return null;
    }
    if (scriptContext instanceof JProlScriptEngineContext) {
      return (JProlScriptEngineContext) scriptContext;
    } else {
      throw new IllegalArgumentException(
          "Expected " + JProlScriptEngineContext.class.getCanonicalName() + " but " +
              scriptContext.getClass().getCanonicalName());
    }
  }

  /**
   * Check that a string can be recognized as a prolog variable name.
   *
   * @param string source string, can be null
   * @return true if the string can be recognized as a prolog variable name
   */
  public static boolean isValidPrologVariableName(final String string) {
    if (string == null || string.isEmpty()) {
      return false;
    }

    if (string.startsWith("_")) {
      if (string.length() == 1) {
        return false;
      }
    } else {
      if (!Character.isAlphabetic(string.charAt(0)) || !Character.isUpperCase(string.charAt(0))) {
        return false;
      }
    }
    if (string.chars()
        .allMatch(x -> Character.isDigit(x) || Character.isAlphabetic(x) || x == '_')) {
      return true;
    }
    return true;
  }

  /**
   * Convert a Java object into JProl Term. Null will be recognized as empty prolog list.
   *
   * @param obj source object, can be null
   * @return converted object, null will be returned as null list
   * @see NamedList
   */
  public static Term java2term(final Object obj) {
    if (obj == null) {
      return NULL_LIST;
    }
    if (obj instanceof Term) {
      return (Term) obj;
    } else if (obj instanceof Number) {
      if (obj instanceof Float || obj instanceof Double) {
        return Terms.newDouble(((Number) obj).doubleValue());
      }
      return Terms.newLong(((Number) obj).longValue());
    }

    if (obj instanceof Collection) {
      final List<Term> terms = ((Collection<?>) obj).stream().map(JProlScriptEngineUtils::java2term)
          .collect(Collectors.toList());

      if (obj instanceof NamedList) {
        final NamedList namedList = (NamedList) obj;
        return Terms.newStruct(Terms.newAtom(namedList.getName()), terms);
      } else {
        return TermList.listOf(terms);
      }
    }
    return Terms.newAtom(obj.toString());
  }

  /**
   * Convert a term into appropriate Java object.
   *
   * @param term a term, can be null.
   * @return if term is null then null, else converted object.
   */
  public static Object term2java(final Term term) {
    if (term == null) {
      return null;
    }

    if (term instanceof TermVar) {
      final Term value = term.tryGround();
      if (term == value) {
        return term.getText();
      } else {
        return term2java(value);
      }
    }

    if (term instanceof TermLong) {
      return term.toNumber().longValue();
    }

    if (term instanceof TermDouble) {
      return term.toNumber().doubleValue();
    }

    if (term instanceof TermList) {
      return ProlUtils.listToMappedValues((TermList) term, true,
          JProlScriptEngineUtils::term2java);
    }

    if (term instanceof TermStruct) {
      final TermStruct struct = (TermStruct) term;
      final String functor = struct.getFunctor().getText();
      final List<Object> terms;
      switch (struct.getArity()) {
        case 0:
          terms = List.of();
          break;
        case 1:
          terms = List.of(term2java(struct.getArgumentAt(0)));
          break;
        default: {
          terms = new ArrayList<>();
          for (int i = 0; i < struct.getArity(); i++) {
            terms.add(term2java(struct.getArgumentAt(i)));
          }
        }
        break;
      }
      return NamedList.namedListOf(functor, terms);
    }

    return term.getText();
  }


}
