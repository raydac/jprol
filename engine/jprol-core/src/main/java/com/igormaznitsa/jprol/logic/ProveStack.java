package com.igormaznitsa.jprol.logic;

import java.util.Arrays;

final class ProveStack {

  private static final int INITIAL_CAPACITY = 32;

  private final int maxDepth;
  private ProveStackFrame[] frames;
  private int size;

  ProveStack(final int maxDepth) {
    this.maxDepth = Math.max(0, maxDepth);
    final int cap = this.maxDepth == 0 ? 0 : Math.min(INITIAL_CAPACITY, this.maxDepth);
    this.frames = new ProveStackFrame[cap];
  }

  boolean isEmpty() {
    return this.size == 0;
  }

  void push(final JProlChoicePoint choicePoint) {
    if (this.size >= this.maxDepth) {
      throw new StackOverflowError("Detected prove stack overflow");
    }
    if (this.size == this.frames.length) {
      this.grow();
    }
    ProveStackFrame frame = this.frames[this.size];
    if (frame == null) {
      frame = new ProveStackFrame();
      this.frames[this.size] = frame;
    }
    frame.prepare(choicePoint);
    this.size++;
  }

  ProveStackFrame peek() {
    return this.frames[this.size - 1];
  }

  void drop() {
    this.size--;
    this.frames[this.size].release();
  }

  private void grow() {
    final int next = Math.min(this.maxDepth, this.frames.length * 2);
    this.frames = Arrays.copyOf(this.frames, Math.max(next, this.size + 1));
  }
}
