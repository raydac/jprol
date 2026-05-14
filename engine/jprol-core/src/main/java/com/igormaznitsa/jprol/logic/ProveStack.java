package com.igormaznitsa.jprol.logic;

import java.util.ArrayList;

/**
 * LIFO stack of immutable {@link ProveStackFrame}s. {@link #drop()} removes the top element.
 * Backing list grows with depth only (not with configured {@code maxDepth}).
 */
final class ProveStack {

  private static final int INITIAL_CAPACITY = 32;

  private final int maxDepth;
  private final ArrayList<ProveStackFrame> frames;

  ProveStack(final int maxDepth) {
    this.maxDepth = Math.max(0, maxDepth);
    final int cap = this.maxDepth == 0 ? 0 : Math.min(INITIAL_CAPACITY, Math.max(1, this.maxDepth));
    this.frames = new ArrayList<>(cap);
  }

  boolean isEmpty() {
    return this.frames.isEmpty();
  }

  void push(final JProlChoicePoint choicePoint) {
    if (this.frames.size() >= this.maxDepth) {
      throw new StackOverflowError("Detected prove stack overflow");
    }
    this.frames.add(ProveStackFrame.fresh(choicePoint));
  }

  ProveStackFrame peek() {
    return this.frames.get(this.frames.size() - 1);
  }

  void replaceTop(final ProveStackFrame frame) {
    this.frames.set(this.frames.size() - 1, frame);
  }

  void drop() {
    this.frames.remove(this.frames.size() - 1);
  }
}
