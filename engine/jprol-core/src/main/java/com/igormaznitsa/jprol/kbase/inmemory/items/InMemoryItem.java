package com.igormaznitsa.jprol.kbase.inmemory.items;

import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.TermVar;
import java.io.PrintWriter;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Data;

@Data
public abstract class InMemoryItem {
  protected final TermStruct clause;
  protected final Term rightHandSide;
  protected final boolean leftHandSidePresented;

  InMemoryItem(final TermStruct clause) {
    this.clause = clause;

    if (clause.isClause()) {
      this.leftHandSidePresented = true;
      this.rightHandSide = clause.getElement(0);
    } else {
      this.leftHandSidePresented = false;
      this.rightHandSide = clause;
    }
  }

  public static InMemoryItem fromClause(final TermStruct clause) {
    final Term rhs = clause.isClause() ? clause.getElement(0) : clause;

    final List<String> foundKeyVars = rhs.stream()
        .filter(x -> x.getTermType() == TermType.VAR && !((TermVar) x).isAnonymous())
        .map(Term::getText)
        .collect(Collectors.toList());

    boolean complex = false;
    if (foundKeyVars.size() > 1) {
      complex = foundKeyVars.size() != foundKeyVars.stream().distinct().count();
    }

    return complex ? new RhsItemComplex(clause) : new RhsItemSimple(clause);
  }

  public abstract boolean matches(final Term rightHandSide);

  public void write(final PrintWriter writer) {
    requireNonNull(writer, "Writer must not be null")
        .write(String.format("%s.%n", this.clause.toSrcString()));
  }

  @Override
  public String toString() {
    return this.rightHandSide.toString();
  }

  public boolean isFact() {
    return !this.leftHandSidePresented && this.rightHandSide.isGround();
  }

  public boolean isRule() {
    return this.leftHandSidePresented || !this.rightHandSide.isGround();
  }

}
