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
 * The annotation allows to define an operator.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface ProlOperator {

    /**
     * The priority of the operator from 0 to 1200, remember 1200 priority is less
     * than 1199
     *
     * @return the operator priority as integer value between 0 and 1200
     */
    int Priority();

    /**
     * The operator type [xfx,yfx,xfy,xf,yf,fx,fy]
     *
     * @return the operator type value
     * @see com.igormaznitsa.prol.data.Operator
     */
    int Type();

    /**
     * The name of the operator
     *
     * @return the name of the operator as String must not be null and have spaces
     */
    String Name();
}
