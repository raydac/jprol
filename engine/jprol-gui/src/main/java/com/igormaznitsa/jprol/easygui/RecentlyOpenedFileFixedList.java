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

package com.igormaznitsa.jprol.easygui;

import java.util.*;

/**
 * An auxiliary class to make fixed length queue
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class RecentlyOpenedFileFixedList {

  private final int maxLen;
  private final List<String> paths = new ArrayList<>();

  public RecentlyOpenedFileFixedList(final int maxlen) {
    this.maxLen = maxlen;
  }

  public synchronized void clear() {
    this.paths.clear();
  }

  public synchronized void put(final String path) {
    // remove it to make as the last one
    this.paths.remove(path);
    paths.add(0, path);

    while (this.paths.size() > maxLen) {
      this.paths.remove(paths.size() - 1);
    }
  }

  public synchronized void add(final String path) {
    // remove it to make as the last one
    this.paths.remove(path);
    if (this.paths.size() < maxLen) {
      this.paths.add(path);
    }
  }

  public synchronized boolean isEmpty() {
    return this.paths.isEmpty();
  }

  public synchronized Iterator<String> getIterator() {
    return new ArrayList<>(this.paths).iterator();
  }

  public synchronized Collection<String> getCollection() {
    return Collections.unmodifiableCollection(new ArrayList<>(this.paths));
  }

  public synchronized void remove(final String path) {
    this.paths.remove(path);
  }
}
