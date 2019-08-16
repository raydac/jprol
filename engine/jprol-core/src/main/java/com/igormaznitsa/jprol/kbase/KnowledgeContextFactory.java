package com.igormaznitsa.jprol.kbase;

public interface KnowledgeContextFactory {
  KnowledgeContext makeDefaultKnowledgeContext();

  KnowledgeContext makeKnowledgeContext(String text);
}
