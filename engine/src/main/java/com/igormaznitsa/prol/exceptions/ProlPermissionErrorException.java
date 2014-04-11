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

/**
 * The class describes the permission_error prolog exception. There shall be a
 * Permission Error when it is not permitted to perform the specified operation
 * on the given object.
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class ProlPermissionErrorException extends ProlAbstractCatcheableException {
  private static final long serialVersionUID = 7752699517532895102L;

  /**
   * The constant has the commont shareable part for all instances of the
   * exception
   */
  private static final Term TERM_ERROR = new Term("permission_error");
  /**
   * The variable contains the string representation of the operation which has
   * thrown the exception
   */
  private final String operation;

  /**
   * The variable contains the string representation of the permission type
   */
  private final String permissionType;

  /**
   * A constructor
   *
   * @param operation the string representation of the operation which has
   * thrown the exception
   * @param permissionType the permission type
   * @param culprit the culprit term
   * @param cause the root java exception which was the cause for the error
   */
  public ProlPermissionErrorException(final String operation, final String permissionType, final Term culprit, final Throwable cause) {
    super(culprit, cause);
    this.operation = operation;
    this.permissionType = permissionType;
  }

  /**
   * A constructor
   *
   * @param operation the string representation of the operation which has
   * thrown the exception
   * @param permissionType the permission type
   * @param message a string message describing the situation
   * @param culprit the culprit term
   * @param cause the root java exception which was the cause for the error
   */
  public ProlPermissionErrorException(final String operation, final String permissionType, final String message, final Term culprit, final Throwable cause) {
    super(message, culprit, cause);
    this.operation = operation;
    this.permissionType = permissionType;
  }

  /**
   * A constructor
   *
   * @param operation the string representation of the operation which has
   * thrown the exception
   * @param permissionType the permission type
   * @param message a string message describing the situation
   * @param culprit the culprit term
   */
  public ProlPermissionErrorException(final String operation, final String permissionType, final String message, final Term culprit) {
    super(message, culprit);
    this.operation = operation;
    this.permissionType = permissionType;
  }

  /**
   * A constructor
   *
   * @param operation the string representation of the operation which has
   * thrown the exception
   * @param permissionType the permission type
   * @param culprit the culprit term
   */
  public ProlPermissionErrorException(final String operation, final String permissionType, final Term culprit) {
    super(culprit);
    this.operation = operation;
    this.permissionType = permissionType;
  }

  /**
   * Get the string representation of the operation which has thrown the
   * exception
   *
   * @return the operation as a String object
   */
  public String getOperation() {
    return operation;
  }

  /**
   * Gedt the permission type
   *
   * @return the permission type as a String object
   */
  public String getPermissionType() {
    return permissionType;
  }

  @Override
  public Term getFunctorForErrorStruct() {
    return TERM_ERROR;
  }

  @Override
  public TermStruct getAsStruct() {
    final TermStruct struct = new TermStruct(TERM_ERROR, new Term[]{(operation == null ? UNDEFINED : new Term(operation)), (permissionType == null ? UNDEFINED : new Term(permissionType)), getCulprit() == null ? UNDEFINED : getCulprit()});
    struct.setCarriedObject(this);
    return struct;
  }

}
