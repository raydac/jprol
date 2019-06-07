package com.igormaznitsa.prol.libraries;

import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

public enum PredicateTemplateType {
  ATOM,
  ATOM_OR_ATOM_LIST,
  ATOMIC,
  BYTE,
  CALLABLE_TERM,
  CHARACTER,
  CHARACTER_CODE,
  CHARACTER_CODE_LIST,
  CHARACTER_LIST,
  CLAUSE,
  COMPOUND_TERM,
  EVALUABLE,
  HEAD,
  IN_BYTE,
  IN_CHARACTER,
  IN_CHARACTER_CODE,
  INTEGER,
  IO_MODE,
  LIST,
  NONVAR,
  NUMBER,
  OPERATOR_SPECIFIER,
  PREDICATE_INDICATOR,
  TERM,
  NON_EMPTY_LIST,
  TRIGGEREVENT;

  private static final Map<String, PredicateTemplateType> map;

  static {
    final Map<String, PredicateTemplateType> temp = new HashMap<>();
    for (final PredicateTemplateType t : PredicateTemplateType.values()) {
      temp.put(t.name().toLowerCase(Locale.ENGLISH), t);
    }
    map = Collections.unmodifiableMap(temp);
  }

  public static PredicateTemplateType findForName(final String name) {
    return map.get(name);
  }

}
