package com.igormaznitsa.jprol.utils;

import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermOperatorContainer;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class OperatorIterator implements CloseableIterator<TermOperator> {
  private final Iterator<TermOperatorContainer> iterator;
  private Iterator<TermOperator> operatorIterator;

  public OperatorIterator(final Iterator<TermOperatorContainer> iterator) {
    this.iterator = iterator;
    if (this.iterator.hasNext()) {
      this.operatorIterator = this.iterator.next().toList().iterator();
    }
  }

  @Override
  public boolean hasNext() {
    return this.operatorIterator != null && this.operatorIterator.hasNext();
  }

  @Override
  public void close() {

  }

  @Override
  public TermOperator next() {
    if (this.operatorIterator == null || !this.operatorIterator.hasNext()) {
      throw new NoSuchElementException();
    }
    final TermOperator result = this.operatorIterator.next();
    if (!this.operatorIterator.hasNext()) {
      if (this.iterator.hasNext()) {
        this.operatorIterator = this.iterator.next().toList().iterator();
      }
    }
    return result;
  }
}

