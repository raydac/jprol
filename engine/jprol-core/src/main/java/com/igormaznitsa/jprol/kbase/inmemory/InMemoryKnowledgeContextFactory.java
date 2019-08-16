package com.igormaznitsa.jprol.kbase.inmemory;

import com.igormaznitsa.jprol.kbase.KnowledgeContext;
import com.igormaznitsa.jprol.kbase.KnowledgeContextFactory;

public final class InMemoryKnowledgeContextFactory implements KnowledgeContextFactory {

  public static final KnowledgeContext DEFAULT_KCONTEXT = new InMemoryKnowledgeContext("");

  @Override
  public KnowledgeContext makeDefaultKnowledgeContext() {
    return DEFAULT_KCONTEXT;
  }

  @Override
  public KnowledgeContext makeKnowledgeContext(final String text) {
    return new InMemoryKnowledgeContext(text);
  }
}
