package com.igormaznitsa.jprol.kbase;

/**
 * One {@code retract/1} scan over the clauses that were visible when the scan opened.
 *
 * <p>This is the logical-update view for a single retract goal. The cursor is created on
 * the first call and then advanced on each redo. It keeps seeing the clauses that existed
 * at open time: a clause asserted later is invisible to this scan, and a clause retracted
 * by another goal before this scan reaches it is skipped. Each successful step removes one
 * remaining match and unifies the retract pattern with that clause.
 */
public interface RetractCursor {

  /**
   * Cursor for a predicate that had no clauses when the scan opened.
   * {@link #retractNext()} is always {@code false}.
   */
  RetractCursor EMPTY = () -> false;

  /**
   * Remove the next clause from the generation captured at open time that still unifies
   * with the retract pattern.
   *
   * @return {@code true} when a clause was removed, {@code false} when no visible match remains
   */
  boolean retractNext();
}
