package com.igormaznitsa.jprol.utils;

import java.util.Objects;

/**
 * Auxiliary class to keep some data in pairs.
 *
 * @param <L> left data nullable
 * @param <R> right data nullable
 * @since 2.2.0
 */
public final class ProlPair<L, R> {
  private final L left;
  private final R right;

  public ProlPair(L left, R right) {
    this.left = left;
    this.right = right;
  }

  public static <L, R> ProlPair<L, R> makeOf(L left, R right) {
    return new ProlPair<>(left, right);
  }

  public R getRight() {
    return this.right;
  }

  public L getLeft() {
    return this.left;
  }

  @Override
  public boolean equals(final Object o) {
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ProlPair<?, ?> prolPair = (ProlPair<?, ?>) o;
    return Objects.equals(left, prolPair.left) &&
        Objects.equals(right, prolPair.right);
  }

  @Override
  public int hashCode() {
    return Objects.hash(this.left, this.right);
  }

  @Override
  public String toString() {
    return "ProlPair{" +
        "left=" + left +
        ", right=" + right +
        '}';
  }
}
