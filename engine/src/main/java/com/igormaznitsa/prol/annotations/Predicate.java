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
package com.igormaznitsa.prol.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * The annotation signals that a function which has the annotation implements a
 * prolog predicate the function can be static or dynamic and it should return
 * boolean or void it must have as arguments Goal and TermStruct
 * <pre>
 * &#64;Predicate(Signature = "put/1", Template = "+number")
 * &#64;Determined
 * public final void predicatePUT(final Goal goal, final TermStruct predicate) {
 *   final Term arg = Utils.getTermFromElement(predicate.getElement(0));
 *   ProlTextWriter outStream = goal.getContext().getCurrentOutStream();
 *   if (outStream == null) {
 *     return;
 *   }
 *
 *   try {
 *     outStream.writeChar(arg);
 *   } catch (IOException ex) {
 *     throw new ProlPermissionErrorException("write", "text_output", predicate);
 *   }
 * }
 * </pre>
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 * @see com.igormaznitsa.prol.annotations.Evaluable
 */
@Target(value = ElementType.METHOD)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface Predicate {

  /**
   * The signature of the predicate (as examples "+/2","nl/0","=../3". It must
   * be defined
   *
   * @return the signature as string
   */
  String Signature();

  /**
   * The template of the predicate. It uses ISO definitions (+.?,@,-) and types
   * from PredicateTemplate without TYPE_ prefix and in low case (atomic =
   * TYPE_ATOMIC)
   *
   * @return string array of possible states, it can be empty (example
   * "@evaluable,@evaluable", "-nonvar,+non_empty_list")
   * @see com.igormaznitsa.prol.libraries.PredicateTemplate
   */
  String[] Template() default {};

  /**
   * The field can have reference of the predicate. Default it is empty.
   *
   * @return the reference of the predicate as String, default it is empty ("")
   */
  String Reference() default "";
}
