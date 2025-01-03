package com.igormaznitsa.jprol.kbase.inmemory;

import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import java.util.ArrayList;
import java.util.HashMap;

public final class InMemoryKnowledgeBase extends AbstractInMemoryKnowledgeBase {

  public InMemoryKnowledgeBase(final String baseId) {
    this(baseId, null);
  }

  public InMemoryKnowledgeBase(
      final String baseId,
      final AbstractInMemoryKnowledgeBase base) {
    super(baseId, base, HashMap::new, HashMap::new, ArrayList::new);
  }

  @Override
  public KnowledgeBase makeCopy() {
    return new InMemoryKnowledgeBase(this.knowledgeBaseId + "_copy", this);
  }

  @Override
  public boolean isConcurrent() {
    return false;
  }
}
