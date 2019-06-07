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

package com.igormaznitsa.prol.logic.triggers;

import com.igormaznitsa.prol.utils.Utils;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public abstract class AbstractProlTrigger implements ProlTrigger {

  protected final Map<String, ProlTriggerType> signatureMap;

  public AbstractProlTrigger() {
    signatureMap = Collections.synchronizedMap(new HashMap<>());
  }

  public AbstractProlTrigger addSignature(final String signature, final ProlTriggerType observedEvent) {
    if (signature == null || observedEvent == null) {
      throw new NullPointerException("Both arguments must not be null");
    }

    String processedsignature = Utils.validateSignature(signature);

    if (processedsignature == null) {
      throw new IllegalArgumentException("Wrong signature format [" + signature + ']');
    } else {
      processedsignature = Utils.normalizeSignature(processedsignature);
    }

    signatureMap.put(processedsignature, observedEvent);

    return this;
  }

  public AbstractProlTrigger removeSignature(final String signature) {
    if (signature == null) {
      throw new NullPointerException("Signature is null");
    }

    String processedsignature = Utils.validateSignature(signature);

    if (processedsignature == null) {
      throw new IllegalArgumentException("Wrong signature format [" + signature + ']');
    } else {
      processedsignature = Utils.normalizeSignature(processedsignature);
    }

    signatureMap.remove(processedsignature);

    return this;
  }

  @Override
  public Map<String, ProlTriggerType> getSignatures() {
    return signatureMap;
  }
}
