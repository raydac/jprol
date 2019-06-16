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

package com.igormaznitsa.prol.io;

import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.data.Terms;
import com.igormaznitsa.prol.logic.ProlContext;

import java.util.Optional;

public interface ProlStreamManager {

  public static final Term END_OF_FILE = Terms.newAtom("end of file");

  Optional<ProlReader> findReaderForId(ProlContext context, String id);

  Optional<ProlWriter> findWriterForId(ProlContext context, String id, boolean append);

  void dispose();
}
