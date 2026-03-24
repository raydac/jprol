package com.igormaznitsa.jprol.logic;

final class ProveStack {

  ProveStackFrame current;
  int depth;

  ProveStack(final int maxDepth) {
    this.depth = maxDepth;
  }

  boolean isEmpty() {
    return this.current == null;
  }

  void push(final JProlChoicePoint choicePoint) {
    this.current = new ProveStackFrame(this.current, choicePoint);
    this.depth--;
    if (this.depth < 0) {
      throw new StackOverflowError("Detected prove stack overflow");
    }
  }

  ProveStackFrame peek() {
    return this.current;
  }

  void drop() {
    this.current = this.current.prev;
    this.depth++;
  }
}
