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
 * Info about predicate definition. If placed over a method then the method will be recognized
 * as processor for the predicate. Also, can be used as just description of a predicate added
 * through JProlConsultText.
 */
@Target(value = ElementType.METHOD)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface JProlPredicate {
  /**
   * Predicated signature in Prolog format like 'hello/3','some/2','fact/0'
   *
   * @return the main predicate signature.
   */
  String signature();

  /**
   * Allows to define synonym signatures for the predicate, sometime it helps if we want to use the same predicate body for multiple predicate definitions.
   *
   * @return array of synonym signatures, all arity must be the same as in main predicate signature
   */
  String[] synonyms() default {};

  /**
   * Array of strings where each string contains descriptor of expected pair modificator+type per each predicate argument '+atom,?list'.
   * List of variants to be validated before predicate call. Empty value meant do not validate.
   * Check of arguments will be only if verify flag of context is true. Also the field can be empty so it will be recognized as any arguments.
   *
   * @return array of allowed argument type combinations
   * @see com.igormaznitsa.jprol.logic.ValidateType
   * @see com.igormaznitsa.jprol.logic.ValidateModificator
   * @see com.igormaznitsa.jprol.logic.JProlSystemFlag#VERIFY
   * @since 3.0.0
   */
  String[] validate() default {};

  /**
   * Description of the predicate, it will be used in help system.
   *
   * @return the description as string.
   */
  String reference() default "";

  /**
   * Flag marks the predicate as determined one (i.e. doesn't have variants).
   *
   * @return true if determined one, false otherwise
   */
  boolean determined() default false;

  /**
   * Flag marks the predicate as usable for is/2 calculable operations.
   *
   * @return true if the predicate is calculable one, false otherwise
   */
  boolean evaluable() default false;

  /**
   * Special flag shows that execution of the predicate changes the choose chain. It is for special predicates like `->/2`
   *
   * @return true if the predicate can change choose chain by its execution.
   * @see com.igormaznitsa.jprol.libs.JProlCoreLibrary#predicateIFTHEN
   */
  boolean changesChooseChain() default false;

  /**
   * Flag shows that the predicate make some guarded operation and should be allowed by a guard processor.
   *
   * @return true if the predicate can be checked by a critical predicate checked defined in JProl engine.
   * @since 3.0.0
   */
  boolean guarded() default false;
}
