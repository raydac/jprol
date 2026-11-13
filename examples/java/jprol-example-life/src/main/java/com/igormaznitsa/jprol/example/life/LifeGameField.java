package com.igormaznitsa.jprol.example.life;

import static java.lang.Math.floorMod;

import java.util.Arrays;
import java.util.concurrent.locks.ReentrantLock;

public class LifeGameField {

  public static final int HEIGHT = 32;
  private static final int COLUMNS = 1;
  public static final int WIDTH = COLUMNS * 64;

  // guards both generation references and the content of both arrays
  private final ReentrantLock lock = new ReentrantLock();

  private long[] generationCurrent = new long[COLUMNS * HEIGHT];
  private long[] generationNext = new long[COLUMNS * HEIGHT];

  private static void writeCell(final long[] generation, final int x, final int y,
                                final boolean value) {
    final int index = cellIndex(x, y);
    final long mask = cellMask(x);
    generation[index] = value ? generation[index] | mask : generation[index] & ~mask;
  }

  private static int cellIndex(final int x, final int y) {
    return floorMod(x, WIDTH) / 64 + floorMod(y, HEIGHT) * COLUMNS;
  }

  private static long cellMask(final int x) {
    return 1L << (floorMod(x, WIDTH) % 64);
  }

  public void blinkGeneration() {
    this.lock.lock();
    try {
      final long[] previous = this.generationCurrent;
      this.generationCurrent = this.generationNext;
      this.generationNext = previous;
      Arrays.fill(this.generationNext, 0L);
    } finally {
      this.lock.unlock();
    }
  }

  public void resetNextGeneration() {
    this.lock.lock();
    try {
      Arrays.fill(this.generationNext, 0L);
    } finally {
      this.lock.unlock();
    }
  }

  public boolean get(final int x, final int y) {
    this.lock.lock();
    try {
      return (this.generationCurrent[cellIndex(x, y)] & cellMask(x)) != 0;
    } finally {
      this.lock.unlock();
    }
  }

  public void clear() {
    this.lock.lock();
    try {
      Arrays.fill(this.generationCurrent, 0L);
      Arrays.fill(this.generationNext, 0L);
    } finally {
      this.lock.unlock();
    }
  }

  public void set(final int x, final int y, final boolean value) {
    this.lock.lock();
    try {
      writeCell(this.generationCurrent, x, y, value);
    } finally {
      this.lock.unlock();
    }
  }

  public void setNext(final int x, final int y, final boolean value) {
    this.lock.lock();
    try {
      writeCell(this.generationNext, x, y, value);
    } finally {
      this.lock.unlock();
    }
  }
}
