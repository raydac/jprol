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

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.TermStruct;

public class ProlPermissionErrorException extends ProlAbstractCatcheableException {

  private static final long serialVersionUID = 7752699517532895102L;

  private static final Term TERM_ERROR = new Term("permission_error");
  private final String operation;

  private final String permissionType;

  public ProlPermissionErrorException(final String operation, final String permissionType, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.operation = operation;
    this.permissionType = permissionType;
  }

  public ProlPermissionErrorException(final String operation, final String permissionType, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.operation = operation;
    this.permissionType = permissionType;
  }

  public ProlPermissionErrorException(final String operation, final String permissionType, final String message, final Term culprit) {
    super(message, culprit);
    this.operation = operation;
    this.permissionType = permissionType;
  }

  public ProlPermissionErrorException(final String operation, final String permissionType, final Term culprit) {
    super(culprit);
    this.operation = operation;
    this.permissionType = permissionType;
  }

  public String getOperation() {
    return operation;
  }

  public String getPermissionType() {
    return permissionType;
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    return new TermStruct(TERM_ERROR, new Term[] {(operation == null ? UNDEFINED : new Term(operation)), (permissionType == null ? UNDEFINED : new Term(permissionType)), getCulprit() == null ? UNDEFINED : getCulprit()});
  }

}
