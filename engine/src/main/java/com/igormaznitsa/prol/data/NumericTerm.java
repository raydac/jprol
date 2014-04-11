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
  public static final int NUMBER_INTEGER = 0;

  /**
   * The constant describes a Float number
   */
  public static final int NUMBER_FLOAT = 1;

  /**
   * Get the value as Number
   *
   * @return the value as a Number object
   */
  public Number getNumericValue();

  /**
   * Compare the number object with other number object
   *
   * @param term which value should be compared, must not be null
   * @return 0 if the number equals, -1 if less and 1 if more
   */
  public int compare(NumericTerm term);

  /**
   * To add a numeric term and return the result
   *
   * @param term a numeric term which value will be added
   * @return the sum of two numeric terms as a NumericTerm, must not be null
   */
  public NumericTerm add(NumericTerm term);

  /**
   * To subtract a numeric term and return the result
   *
   * @param term a numeric term which value will be subtracted
   * @return the difference of two numeric terms as a NumericTerm, must not be
   * null
   */
  public NumericTerm sub(NumericTerm term);

  /**
   * To divide on a numeric term and return the result
   *
   * @param term a numeric term which value will be used for the operation
   * @return the divided result of two numeric terms as a NumericTerm, must not
   * be null
   */
  public NumericTerm div(NumericTerm term);

  /**
   * To multiplicate on a numeric term and return the result
   *
   * @param term a numeric term which value will be used for the operation
   * @return the result of multiplication of two numeric terms as a NumericTerm,
   * must not be null
   */
  public NumericTerm mul(NumericTerm term);

  /**
   * To get the negative value of the numeric term
   *
   * @return the negative value as a numeric term, must not be null
   */
  public NumericTerm neg();

  /**
   * To get the absolute value of the numeric term
   *
   * @return the absoolute value as a numeric term, must not be null
   */
  public NumericTerm abs();

  /**
   * To get the sign of the numeric term value
   *
   * @return a numeric term contains 0 if it is 0, -1 if it is less than 0, 1 if
   * it is more than 0, must not be null
   */
  public NumericTerm sign();

  /**
   * To get the type of the numeric term
   *
   * @return the type of the numeric term as integer
   */
  public int getNumberType();
}
