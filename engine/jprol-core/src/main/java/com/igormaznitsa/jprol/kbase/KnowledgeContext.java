package com.igormaznitsa.jprol.kbase;

import com.igormaznitsa.jprol.data.Term;

public interface KnowledgeContext {
  Term asTerm();

  boolean doesInclude(KnowledgeContext otherContext);
}
