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

package com.igormaznitsa.jprol.data;

import java.util.Optional;

/**
 * Java value that can be turned into a Prolog term.
 *
 * <p>Implement this on a domain object when a caller wants one term for it and the object itself
 * decides whether that term exists. The engine does not discover implementations on its own:
 * the caller asks {@link #asProlTerm()} and then unifies, asserts, or prints the result.
 * An empty optional means this value has no term. It is a normal answer, so the caller skips it.
 *
 * <pre>{@code
 * public final class Person implements ConvertibleToTerm {
 *   private final String name;
 *
 *   public Person(String name) {
 *     this.name = name;
 *   }
 *
 *   @Override
 *   public Optional<Term> asProlTerm() {
 *     if (this.name == null || this.name.isBlank()) {
 *       return Optional.empty();
 *     }
 *     return Optional.of(Terms.newAtom(this.name));
 *   }
 * }
 *
 * person.asProlTerm().ifPresent(term ->
 *     base.assertZ(context, Terms.newStruct(Terms.newAtom("person"), term)));
 * }</pre>
 */
public interface ConvertibleToTerm {

  /**
   * Build the Prolog term represented by this value.
   *
   * <p>Return a term the caller may unify or store. Return {@link Optional#empty()} when this
   * value has nothing to contribute. The term should be safe for the caller to keep: do not
   * return a live variable that another proof is still binding.
   *
   * @return the term for this value, or an empty optional when there is none; never {@code null}
   * @throws RuntimeException when the value should have a term and the conversion fails
   */
  Optional<Term> asProlTerm();
}
