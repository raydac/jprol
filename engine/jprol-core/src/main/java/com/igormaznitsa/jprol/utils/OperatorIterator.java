package com.igormaznitsa.jprol.utils;

import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermOperatorContainer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

public final class OperatorIterator implements CloseableIterator<TermOperator> {
  private final Iterator<TermOperatorContainer> iterator;
  private Iterator<TermOperator> operatorIterator;

  public OperatorIterator(final Iterator<TermOperatorContainer> iterator) {
    final List<TermOperatorContainer> containers = new ArrayList<>();
    iterator.forEachRemaining(containers::add);
    this.iterator = containers.iterator();
    if (this.iterator.hasNext()) {
      this.operatorIterator = this.iterator.next().makeList().iterator();
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
        this.operatorIterator = this.iterator.next().makeList().iterator();
      }
    }
    return result;
  }
}

