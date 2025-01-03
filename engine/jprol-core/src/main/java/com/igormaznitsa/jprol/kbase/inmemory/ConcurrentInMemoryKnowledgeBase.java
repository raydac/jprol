package com.igormaznitsa.jprol.kbase.inmemory;

import com.igormaznitsa.jprol.kbase.KnowledgeBase;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

public final class ConcurrentInMemoryKnowledgeBase extends AbstractInMemoryKnowledgeBase {

  public ConcurrentInMemoryKnowledgeBase(final String baseId) {
    this(baseId, null);
  }

  public ConcurrentInMemoryKnowledgeBase(
      final String baseId,
      final AbstractInMemoryKnowledgeBase base) {
    super(baseId, base, ConcurrentHashMap::new, ConcurrentHashMap::new, CopyOnWriteArrayList::new);
  }

  @Override
  public KnowledgeBase makeCopy() {
    return new ConcurrentInMemoryKnowledgeBase(this.knowledgeBaseId + "_copy", this);
  }

  @Override
  public boolean isConcurrent() {
    return true;
  }
}
