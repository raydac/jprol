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

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Allows to make auto-consult from accessible resources described by URLs.
 *
 * @since 2.2.0
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface JProlConsultUrl {
  /**
   * Array contains resource URLs.
   *
   * @return resource paths as array
   */
  String[] value() default {};

  /**
   * Array allows to define descriptions of predicates loaded by the resources.
   *
   * @return array of predicate descriptions.
   */
  JProlPredicate[] declaredPredicates() default {};
}
