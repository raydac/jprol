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
package com.igormaznitsa.prol.exceptions;

/**
 * The class describes a prol engine exception which will be thrown if there is
 * any error in the work with a prol knowledge base.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlKnowledgeBaseException extends ProlException {
  private static final long serialVersionUID = 7055306365499059734L;

  /**
   * A constructor
   */
  public ProlKnowledgeBaseException() {
    super();
  }

  /**
   * A constructor
   *
   * @param message a string message describes the situation
   */
  public ProlKnowledgeBaseException(final String message) {
    super(message);
  }

  /**
   * A constructor
   *
   * @param message a string message describes the situation
   * @param cause the root java exception which was the cause for the error
   */
  public ProlKnowledgeBaseException(final String message, final Throwable cause) {
    super(message, cause);
  }

  /**
   * A constructor
   *
   * @param cause the root java exception which was the cause for the error
   */
  public ProlKnowledgeBaseException(Throwable cause) {
    super(cause);
  }

}
