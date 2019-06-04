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
 * The annotation allows user to place its program just in java sources. It
 * should place the annotation before class definition. The annotation
 * parameters are processed in the order - URL -> URLs -> Text -> Texts
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface Consult {

  /**
   * The field contains the url of a consult data which should be loaded and
   * parsed
   *
   * @return the value as String, default it has "" value
   * @since 1.01
   */
  String URL() default "";

  /**
   * The field contains a url array url of data which should be read and parsed
   * during consulting
   *
   * @return the value as String array, default it is the zero length array
   * @since 1.01
   */
  String[] URLs() default {};

  /**
   * The field contains text as single string. The field will be processed as
   * the first one.
   *
   * @return the value of the fied as a String
   * @since 1.00
   */
  String Text() default "";

  /**
   * The field contains text which can be broken in a few strings and every
   * string will be processed separately.
   *
   * @return array of strings
   * @since 1.00
   */
  String[] Texts() default {};
}
