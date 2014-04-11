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
package com.igormaznitsa.prol.utils;

/**
 * Small auxiliary class to save integer values in the hash set for quick
 * search, also it optimizes the value order during requests
 *
 * @author Igor Maznitsa (igor.maznitsa@igormaznitsa.com)
 */
public final class IntegerHashSet {

  /**
   * The size of the inside list array
   */
  private static final int SIZE = 0x20;
  /**
   * The mask to get array index
   */
  private static final int MASK = SIZE - 1;
  /**
   * Inside array to save values
   */
  private final int[][] insideArray;

  /**
   * The constructor
   */
  public IntegerHashSet() {
    insideArray = new int[SIZE][];
  }

  /**
   * Check that a value is being saved into the hash set
   *
   * @param value the integer value to be checked
   * @return true if the value is being saved, else false
   */
  public boolean contains(final int value) {
    final int index = value & MASK;
    if (insideArray[index] != null) {
      final int[] arr = insideArray[index];
      final int len = arr.length;
      for (int li = 0; li < len; li++) {
        final int val = arr[li];

        if (val == value) {
          if (li != 0) {
            final int oldzero = arr[0];
            arr[0] = val;
            arr[li] = oldzero;
          }
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Add new value into the hash set
   *
   * @param value an integer value to be saved into the set
   */
  public void add(final int value) {
    final int index = value & MASK;

    if (insideArray[index] == null) {
      insideArray[index] = new int[]{value};
    }
    else {
      final int[] arr = insideArray[index];
      final int len = arr.length;
      for (int li = 0; li < len; li++) {
        if (arr[li] == value) {
          return;
        }
      }

      final int[] newarr = new int[len + 1];
      System.arraycopy(arr, 0, newarr, 0, len);
      newarr[len] = value;
      insideArray[index] = newarr;
    }
  }
}
