package com.igormaznitsa.jprol.kbase;

public interface KnowledgeContextFactory {
  KnowledgeContext getDefaultKnowledgeContext();

  KnowledgeContext makeKnowledgeContext(String text);
}
