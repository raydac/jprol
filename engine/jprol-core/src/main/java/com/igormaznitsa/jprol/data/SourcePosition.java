package com.igormaznitsa.jprol.data;

public final class SourcePosition {

  public static final SourcePosition UNKNOWN = new SourcePosition(-1, -1);

  private final int line;
  private final int position;

  private SourcePosition(final int line, final int position) {
    this.line = line;
    this.position = position;
  }

  public static SourcePosition positionOf(final int line, final int position) {
    if (line < 0 || position < 0) {
      return UNKNOWN;
    } else {
      return new SourcePosition(line, position);
    }
  }

  public boolean isUnknown() {
    return this == UNKNOWN;
  }

  public int getLine() {
    return this.line;
  }

  public int getPosition() {
    return this.position;
  }

  @Override
  public String toString() {
    if (this.isUnknown()) {
      return "UNKNOWN";
    } else {
      return Integer.toString(this.line) + ':' + this.position;
    }
  }
}
