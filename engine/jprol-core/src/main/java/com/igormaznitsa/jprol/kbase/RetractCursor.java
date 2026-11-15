package com.igormaznitsa.jprol.kbase;

/**
 * One {@code retract/1} scan over the clauses that were visible when the scan opened.
 *
 * <p>Obtain a cursor from {@link KnowledgeBase#openRetract}. The first
 * {@link #retractNext()} is the first solution. Each later call is a redo. The cursor keeps the
 * clause generation captured at {@code openRetract}: a clause asserted afterwards stays invisible,
 * and a clause already removed by another goal is skipped when the scan reaches it. A successful
 * step removes one remaining match and unifies the retract pattern with that clause.
 *
 * <pre>{@code
 * RetractCursor cursor = context.getKnowledgeBase().openRetract(context, pattern);
 * while (cursor.retractNext()) {
 *   // pattern is unified with the clause just removed
 * }
 * }</pre>
 *
 * @see KnowledgeBase#openRetract
 */
public interface RetractCursor {

  /**
   * Cursor for a predicate that had no clauses when the scan opened.
   * {@link #retractNext()} is always {@code false}.
   */
  RetractCursor EMPTY = () -> false;

  /**
   * Remove the next clause from the generation captured at open time that still unifies with the
   * retract pattern.
   *
   * <p>On success the removed clause is unified with the pattern given to
   * {@link KnowledgeBase#openRetract}, and a retract trigger for that predicate is notified.
   * A later call continues after that clause. The method returns {@code false} when no visible
   * match remains. That {@code false} is the normal end of the scan.
   *
   * @return {@code true} when a clause was removed and unified with the pattern, {@code false}
   *     when no visible match remains
   * @throws com.igormaznitsa.jprol.exceptions.ProlException when the context is already disposed
   *     and a retract trigger must be notified
   * @throws RuntimeException when a retract trigger throws
   * @throws Error when a clause that matched the scan cannot be unified back onto the pattern
   */
  boolean retractNext();
}
