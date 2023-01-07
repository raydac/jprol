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

package com.igormaznitsa.jprol.logic.triggers;

import static com.igormaznitsa.jprol.utils.Utils.normalizeSignature;
import static com.igormaznitsa.jprol.utils.Utils.validateSignature;
import static java.util.Objects.requireNonNull;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public abstract class AbstractJProlTrigger implements JProlTrigger {

  private final Map<String, JProlTriggerType> signatureMap;

  public AbstractJProlTrigger() {
    this.signatureMap = Collections.synchronizedMap(new HashMap<>());
  }

  public AbstractJProlTrigger addSignature(final String signature,
                                           final JProlTriggerType observedEvent) {
    String validatedSignature = validateSignature(requireNonNull(signature));

    if (validatedSignature == null) {
      throw new IllegalArgumentException("Wrong signature format [" + signature + ']');
    } else {
      validatedSignature = normalizeSignature(validatedSignature);
    }

    this.signatureMap.put(validatedSignature, requireNonNull(observedEvent));

    return this;
  }

  public AbstractJProlTrigger removeSignature(final String signature) {
    String validatedSignature = validateSignature(requireNonNull(signature, "Signature is null"));

    if (validatedSignature == null) {
      throw new IllegalArgumentException("Wrong signature format [" + signature + ']');
    } else {
      validatedSignature = normalizeSignature(validatedSignature);
    }

    this.signatureMap.remove(validatedSignature);

    return this;
  }

  @Override
  public Map<String, JProlTriggerType> getSignatures() {
    return this.signatureMap;
  }
}
