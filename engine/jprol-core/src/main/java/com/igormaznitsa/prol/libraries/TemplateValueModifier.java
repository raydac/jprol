package com.igormaznitsa.prol.libraries;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;

import java.util.function.Function;

import static com.igormaznitsa.prol.data.TermType.*;

public enum TemplateValueModifier {
  SHALL_REMAIN_UNALTERED(t -> {
    if (!(t.getTermType() == LIST || t.getTermType() == STRUCT) && t.findNonVarOrDefault(t).getTermType() == VAR) {
      throw new ProlInstantiationErrorException("Should be instantiated \'" + t.toSrcString() + '\'', t);
    }
    return true;
  }, false),
  SHALL_BE_INSTANTIATED(t -> {
    if (t.findNonVarOrDefault(t).getTermType() == VAR) {
      throw new ProlInstantiationErrorException("Should be instantiated \'" + t.toSrcString() + '\'', t);
    }
    return true;
  }, false),
  SHALL_BE_INSTANTIATED_OR_VARIABLE(t -> t.findNonVarOrDefault(t).getTermType() != VAR, false),
  SHALL_BE_VARIABLE(t -> {
    if (t.findNonVarOrDefault(t).getTermType() != VAR) {
      throw new ProlInstantiationErrorException("Should be noninstantiated variable \'" + t.toSrcString() + '\'', t);
    }
    return false;
  }, true);

  private final Function<Term, Boolean> checker;
  private final boolean shouldNotBeAltered;

  TemplateValueModifier(final Function<Term, Boolean> checker, final boolean shouldNotBeAlteredFlag) {
    this.checker = checker;
    this.shouldNotBeAltered = shouldNotBeAlteredFlag;
  }

  public static TemplateValueModifier findForName(final char chr) {
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

  public boolean isShouldNotBeAltered() {
    return this.shouldNotBeAltered;
  }

  public boolean shouldCheckTemplate(final Term term) {
    return this.checker.apply(term);
  }
}
