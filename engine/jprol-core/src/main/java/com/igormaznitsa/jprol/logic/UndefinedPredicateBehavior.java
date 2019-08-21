package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;

import java.util.Optional;

import static com.igormaznitsa.jprol.data.Terms.newAtom;
import static java.util.Arrays.stream;

public enum UndefinedPredicateBehavior {
  ERROR(newAtom("error")),
  FAIL(newAtom("fail")),
  WARNING(newAtom("warning"));

  private final Term term;

  UndefinedPredicateBehavior(final Term term) {
    this.term = term;
  }

  public static Optional<UndefinedPredicateBehavior> find(final String text) {
    return stream(values()).filter(x -> x.term.getText().equals(text)).findFirst();
  }

  @Override
  public String toString() {
    return this.getTerm().getText();
  }

  public Term getTerm() {
    return this.term;
  }
}
