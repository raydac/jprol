package com.igormaznitsa.jprol.kbase.inmemory;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.kbase.KnowledgeContext;

public final class InMemoryKnowledgeContext implements KnowledgeContext {
  private final Term path;

  public InMemoryKnowledgeContext(final String path) {
    this.path = Terms.newAtom(path);
  }

  @Override
  public Term asTerm() {
    return this.path;
  }

  public String getPath() {
    return this.path.getText();
  }

  @Override
  public int hashCode() {
    return this.getPath().hashCode();
  }

  @Override
  public boolean equals(final Object that) {
    if (this == that) {
      return true;
    }
    if (that == null) {
      return false;
    }
    if (that instanceof InMemoryKnowledgeContext) {
      return this.getPath().equals(((InMemoryKnowledgeContext) that).getPath());
    }
    return false;
  }

  @Override
  public boolean doesInclude(final KnowledgeContext otherContext) {
    if (otherContext instanceof InMemoryKnowledgeContext) {
      return ((InMemoryKnowledgeContext) otherContext).getPath().startsWith(this.getPath());
    }
    return false;
  }
}
