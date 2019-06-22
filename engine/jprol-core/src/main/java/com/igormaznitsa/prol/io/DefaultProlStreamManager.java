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

import com.igormaznitsa.prol.logic.ProlContext;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

import static java.util.Objects.requireNonNull;

public class DefaultProlStreamManager implements ProlStreamManager, ProlIoResource.ProlResourceCloseListener {

  public static final String USER_STREAM = "user";
  public static final String ERROR_STREAM = "error";

  private final Map<ProlContext, Map<String, ProlWriter>> writers = new ConcurrentHashMap<>();
  private final Map<ProlContext, Map<String, ProlReader>> readers = new ConcurrentHashMap<>();

  private final AtomicBoolean disposed = new AtomicBoolean();

  public DefaultProlStreamManager() {
  }

  public void dispose() {
    if (this.disposed.compareAndSet(false, true)) {
      this.readers.values().stream().flatMap(x -> x.values().stream()).forEach(x -> {
        try {
          x.close();
        } catch (IOException ex) {
        }
      });
      this.writers.values().stream().flatMap(x -> x.values().stream()).forEach(x -> {
        try {
          x.close();
        } catch (IOException ex) {

        }
      });
      this.readers.clear();
      this.writers.clear();
    }
  }

  private void assertNonDisposed() {
    if (this.disposed.get()) {
      throw new IllegalStateException("Disposed");
    }
  }

  @Override
  public Optional<ProlReader> findReaderForId(final ProlContext context, final String id) {
    requireNonNull(id, "Stream Id must not be null");
    assertNonDisposed();

    final Map<String, ProlReader> readers = this.readers.computeIfAbsent(context, x -> new ConcurrentHashMap<>());

    return Optional.ofNullable(readers.computeIfAbsent(id, key -> {
      final ProlReader result;

      if (USER_STREAM.equals(key)) {
        result = new ProlReader(context, USER_STREAM, new InputStreamReader(System.in, Charset.defaultCharset()));
      } else {
        result = null;
      }

      if (result != null) {
        result.addProlResourceCloseListener(this);
      }
      return result;
    }));
  }

  @Override
  public Optional<ProlWriter> findWriterForId(final ProlContext context, final String id, boolean append) {
    requireNonNull(id, "Stream Id must not be null");
    assertNonDisposed();

    final Map<String, ProlWriter> writers = this.writers.computeIfAbsent(context, x -> new ConcurrentHashMap<>());

    return Optional.ofNullable(writers.computeIfAbsent(id, key -> {
      final ProlWriter result;
      if (USER_STREAM.equals(key)) {
        result = new ProlWriter(USER_STREAM, new PrintWriter(System.out)) {
          @Override
          public void close() {
          }
        };
      } else if (ERROR_STREAM.equals(id)) {
        result = new ProlWriter(ERROR_STREAM, new PrintWriter(System.out)) {
          @Override
          public void close() {
          }
        };
      } else {
        result = null;
      }
      if (result != null) {
        result.addProlResourceCloseListener(this);
      }
      return result;
    }));
  }

  @Override
  public void onProlResourceClosed(final ProlIoResource<? extends Closeable> closedResource) {
    if (closedResource instanceof ProlWriter) {
      this.writers.remove(closedResource.getId());
    } else if (closedResource instanceof ProlReader) {
      this.readers.remove(closedResource.getId());
    }
  }
}
