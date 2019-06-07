package com.igormaznitsa.prol.libraries;

public enum PredicateTemplateModifier {
  SHALL_REMAIN_UNALTERED,
  SHALL_BE_INSTANTIATED,
  SHALL_BE_INSTANTIATED_OR_VARIABLE,
  SHALL_BE_VARIABLE;

  public static PredicateTemplateModifier findForName(final char chr) {
    switch (chr) {
      case '+':
        return SHALL_BE_INSTANTIATED;
      case '@':
        return SHALL_REMAIN_UNALTERED;
      case '-':
        return SHALL_BE_VARIABLE;
      case '?':
        return SHALL_BE_INSTANTIATED_OR_VARIABLE;
      default:
        return null;
    }

  }
}
