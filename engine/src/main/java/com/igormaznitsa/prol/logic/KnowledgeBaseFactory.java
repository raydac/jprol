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

/**
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface KnowledgeBaseFactory {

  /**
   * Make default knowledge base
   *
   * @param context the context which will use the knowledge base, must not be
   * null
   * @param knowledgeBasId the knowledge base id , must not be null
   * @return a knowledge base which is default for the factory
   */
  public KnowledgeBase makeDefaultKnowledgeBase(ProlContext context, String knowledgeBasId);

  /**
   * Make knowledge base for a type
   *
   * @param context the context which will use the knowledge base, must not be
   * null
   * @param knowledgeBaseId the knowledge base id, must not be null
   * @param type the type of the knowledge base, must not be null
   * @return a knowledge base for the type or null if it is impossible to make a
   * base
   */
  public KnowledgeBase makeKnowledgeBase(ProlContext context, String knowledgeBaseId, String type);
}
