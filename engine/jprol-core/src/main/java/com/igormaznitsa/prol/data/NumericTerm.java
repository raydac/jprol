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

package com.igormaznitsa.prol.data;

/**
 * The interface describes a numeric term and its possible operations
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public interface NumericTerm {

  /**
   * The constant describes an Integer number
   */
  int NUMBER_INTEGER = 0;

  /**
   * The constant describes a Float number
   */
  int NUMBER_FLOAT = 1;

  Number getNumericValue();

  int compare(NumericTerm term);

  NumericTerm add(NumericTerm term);

  NumericTerm sub(NumericTerm term);

  NumericTerm div(NumericTerm term);

  NumericTerm mul(NumericTerm term);

  NumericTerm neg();

  NumericTerm abs();

  NumericTerm sign();

  int getNumberType();
}
