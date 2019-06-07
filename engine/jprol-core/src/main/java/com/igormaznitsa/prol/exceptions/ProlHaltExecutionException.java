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

package com.igormaznitsa.prol.exceptions;

public class ProlHaltExecutionException extends ProlException {

  private static final long serialVersionUID = -8265930292043474634L;

  private final int status;

  public ProlHaltExecutionException() {
    super("The program is halted.");
    this.status = 0;
  }

  public ProlHaltExecutionException(final int status) {
    super("The program is halted.");
    this.status = status;
  }

  public ProlHaltExecutionException(final String cause, final int status) {
    super(cause);
    this.status = status;
  }

  public int getStatus() {
    return this.status;
  }
}
