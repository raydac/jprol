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

package com.igormaznitsa.jprol.exceptions;

/**
 * The class describes a critical error after which a prol engine can't work and
 * will be stopped
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlCriticalError extends Error {

  public ProlCriticalError(final Throwable cause) {
    super(cause);
  }

  public ProlCriticalError(final String message, final Throwable cause) {
    super(message, cause);
  }

  public ProlCriticalError(final String message) {
    super(message);
  }

  public ProlCriticalError() {
    super();
  }

}
