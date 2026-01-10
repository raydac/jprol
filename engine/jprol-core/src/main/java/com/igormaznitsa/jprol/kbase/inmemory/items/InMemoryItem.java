package com.igormaznitsa.jprol.kbase.inmemory.items;

import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import java.io.PrintWriter;
import java.util.List;
import java.util.stream.Collectors;

public abstract class InMemoryItem {
  protected final TermStruct clause;
  protected final Term head;
  protected final boolean headGrounded;
  protected final boolean bodyPresented;

  InMemoryItem(final TermStruct clause) {
    this.clause = clause;

    if (clause.isClause()) {
      this.bodyPresented = true;
      this.head = clause.getArgumentAt(0);
    } else {
      this.bodyPresented = false;
      this.head = clause;
    }
    this.headGrounded = this.head.isGround();
  }

  public static InMemoryItem fromClause(final TermStruct clause) {
    final Term rhs = clause.isClause() ? clause.getArgumentAt(0) : clause;

    final List<String> foundKeyVars = rhs.stream()
        .filter(x -> x.getTermType() == TermType.VAR && !x.isAnonymous())
        .map(Term::getText)
        .collect(Collectors.toList());

    boolean complex = false;
    if (foundKeyVars.size() > 1) {
      complex = foundKeyVars.size() != foundKeyVars.stream().distinct().count();
    }

    return complex ? new RhsItemComplex(clause) : new RhsItemSimple(clause);
  }

  public boolean isBodyPresented() {
    return this.bodyPresented;
  }

  public Term getHead() {
    return this.head;
  }

  public TermStruct getClause() {
    return this.clause;
  }

  public abstract boolean matches(final Term rightHandSide);

  public void write(final PrintWriter writer) {
    requireNonNull(writer, "Writer must not be null")
        .write(String.format("%s.%n", this.clause.toSrcString()));
  }

  @Override
  public String toString() {
    return this.head.toString();
  }

  public boolean isFact() {
    return !this.bodyPresented && this.headGrounded;
  }

  public boolean isRule() {
    return this.bodyPresented || !this.headGrounded;
  }

}
