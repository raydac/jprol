package com.igormaznitsa.jprol.utils;

import java.util.Iterator;
import java.util.NoSuchElementException;

public final class SingletoneIterator<T> implements Iterator<T> {

  private T value;

  public SingletoneIterator(final T value) {
    this.value = value;
  }

  @Override
  public boolean hasNext() {
    return this.value != null;
  }

  @Override
  public T next() {
    final T result = this.value;
    this.value = null;
    if (result == null) {
      throw new NoSuchElementException();
    }
    return result;
  }

}
