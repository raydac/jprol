/* 
 * Copyright 2014 Igor Maznitsa (http://www.igormaznitsa.com).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.igormaznitsa.prol.logic;

import com.igormaznitsa.prol.containers.KnowledgeBase;
import com.igormaznitsa.prol.containers.MemoryKnowledgeBase;

/**
 * Default knowledge base factory which use MemoryKnpwledgeBase as a default
 * base
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class DefaultKnowledgeBaseFactory implements KnowledgeBaseFactory {

  private static DefaultKnowledgeBaseFactory instance = new DefaultKnowledgeBaseFactory();

  public static DefaultKnowledgeBaseFactory getInstance() {
    return instance;
  }

  @Override
  public KnowledgeBase makeDefaultKnowledgeBase(ProlContext context, String knowledgeBasId) {
    return new MemoryKnowledgeBase(context, knowledgeBasId);
  }

  @Override
  public KnowledgeBase makeKnowledgeBase(ProlContext context, String knowledgeBaseId, String type) {
    throw new UnsupportedOperationException("DefaultKnowledgeBaseFactory doesn't support custom type bases");
  }

}
