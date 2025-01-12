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

package com.igormaznitsa.jprol.annotations;

import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Describes an JProl operator.
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
@Repeatable(value = JProlOperators.class)
public @interface JProlOperator {
  /**
   * Priority in diapason (MIN)1200..0(MAX)
   *
   * @return priority as integer
   * @see com.igormaznitsa.jprol.data.TermOperator#PRIORITY_MAX
   * @see com.igormaznitsa.jprol.data.TermOperator#PRIORITY_MIN
   */
  int priority();

  /**
   * Type of association.
   *
   * @return the association type.
   */
  OpAssoc type();

  /**
   * Operator name. Must be appropriate one and in Prolog style.
   *
   * @return name of the operator.
   */
  String name();
}
