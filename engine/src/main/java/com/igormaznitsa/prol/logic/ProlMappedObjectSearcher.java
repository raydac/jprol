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

/**
 * The interface desribes a class which can be used for search of a mapped
 * object for a string key. The key is the text of a Term object.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface ProlMappedObjectSearcher {

  /**
   * The function allows to find the mapped object
   *
   * @param termText the text which should be used as the key, must not be null
   * @return Object if it has been found, else null
   */
  Object findProlMappedObject(String termText);

  /**
   * The function allows to find associated text string with a Java object
   *
   * @param obj the object we need the string for
   * @return the string representation of the object, else null if it is not
   * found
   */
  String findProlMappedTerm(Object obj);
}
