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
package com.igormaznitsa.prol.easygui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * An auxiliary class to make fixed length queue
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public class RecentlyOpenedFileFixedList {

  private final int maxLen;
  private final List<String> paths = new LinkedList<String>();

  public RecentlyOpenedFileFixedList(final int maxlen) {
    maxLen = maxlen;
  }

  public synchronized void clear() {
    paths.clear();
  }

  public synchronized void put(final String path) {
    if (paths.contains(path)) {
      // remove it to make as the last one
      paths.remove(path);
    }
    paths.add(0, path);
    
    while(paths.size()>maxLen){
      paths.remove(paths.size()-1);
    }
  }

  public  synchronized void add(final String path){
    if (paths.contains(path)) {
      // remove it to make as the last one
      paths.remove(path);
    }
    if (paths.size()<maxLen){
      paths.add(path);
    }
  }
  
  public synchronized boolean isEmpty() {
    return paths.isEmpty();
  }

  public synchronized Iterator<String> getIterator() {
    return new LinkedList<String>(paths).iterator();
  }

  public synchronized Collection<String> getCollection() {
    return Collections.unmodifiableCollection(new ArrayList<String>(paths));
  }

  public synchronized void remove(final String path) {
    paths.remove(path);
  }
}
