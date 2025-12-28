package com.igormaznitsa.jprol.jsr223;

import static com.igormaznitsa.jprol.data.TermList.NULL_LIST;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermDouble;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermLong;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.utils.ProlUtils;
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
    return string.startsWith("_") || Character.isUpperCase(string.charAt(0));
  }

  /**
   * Convert a Java object into JProl Term
   *
   * @param obj source object, can be null
   * @return converted object, null will be returned as null list
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
      return TermList.listOf(terms);
    }
    return Terms.newAtom(obj.toString());
  }

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

    return term.getText();
  }


}
