package com.igormaznitsa.jprol.example.life;

import java.util.Arrays;

public class LifeField {

  public static final int HEIGHT = 32;
  private static final int COLUMNS = 1;
  public static final int WIDTH = COLUMNS * 64;
  private final long[] generationOne = new long[COLUMNS * HEIGHT];
  private final long[] generationTwo = new long[COLUMNS * HEIGHT];

  private long[] generationCurrent = generationOne;
  private long[] generationNext = generationTwo;

  public LifeField() {

  }

  public synchronized void blinkGeneration() {
    this.generationCurrent =
        this.generationCurrent == this.generationOne ? this.generationTwo : this.generationOne;
    this.generationNext =
        this.generationNext == this.generationOne ? this.generationTwo : this.generationOne;
    Arrays.fill(this.generationNext, 0L);
  }

  public synchronized boolean get(final int x, final int y) {
    int nx = x % WIDTH;
    if (nx < 0) {
      nx += WIDTH;
    }

    int ny = y % HEIGHT;
    if (ny < 0) {
      ny += HEIGHT;
    }

    final int dc = nx / 64;
    final int dx = nx % 64;

    final int cellIndex = dc + (ny * COLUMNS);

    return (this.generationCurrent[cellIndex] & (1L << dx)) != 0;
  }

  public synchronized void clear() {
    Arrays.fill(this.generationCurrent, 0L);
    Arrays.fill(this.generationNext, 0L);
  }

  public synchronized void set(final int x, final int y, final boolean value) {
    int nx = x % WIDTH;
    if (nx < 0) {
      nx += WIDTH;
    }

    int ny = y % HEIGHT;
    if (ny < 0) {
      ny += HEIGHT;
    }

    final int dc = nx / 64;
    final int dx = nx % 64;

    final int cellIndex = dc + (ny * COLUMNS);

    final long mask = 1L << dx;

    long oldValue = this.generationCurrent[cellIndex];
    long newValue = value ? oldValue | mask : oldValue & ~mask;
    this.generationCurrent[cellIndex] = newValue;
  }

  public synchronized void setNext(final int x, final int y, final boolean value) {
    int nx = x % WIDTH;
    if (nx < 0) {
      nx += WIDTH;
    }

    int ny = y % HEIGHT;
    if (ny < 0) {
      ny += HEIGHT;
    }

    final int dc = nx / 64;
    final int dx = nx % 64;

    final int cellIndex = dc + (ny * COLUMNS);

    final long mask = 1L << dx;

    long oldValue = this.generationNext[cellIndex];
    long newValue = value ? oldValue | mask : oldValue & ~mask;
    this.generationNext[cellIndex] = newValue;
  }

}
