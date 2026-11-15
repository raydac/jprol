package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermVar;
import java.io.Reader;
import java.util.Map;

/**
 * Decides what happens to {@code ?-} queries found while a source is consulted.
 *
 * <p>Pass an instance to {@link JProlContext#consult(Reader, QueryInteractor)} or
 * {@link JProlContext#consult(String, QueryInteractor)}. Clauses and directives are consulted as
 * usual. Each top-level {@code ?- goal.} is offered to this interactor instead of being stored.
 * {@link JProlContext#consult(Reader)} passes no interactor, and those queries are skipped.
 *
 * <p>The calls for one query are {@link #isQueryAllowed}, then, when that returns {@code true},
 * a proof. Each success calls {@link #onQuerySuccess}. The first failure calls
 * {@link #onQueryFail} and the consult continues with the next phrase. Returning {@code false}
 * from {@link #onQuerySuccess} stops the consult: the engine throws
 * {@link com.igormaznitsa.jprol.exceptions.ProlHaltExecutionException} with status {@code 1},
 * and the rest of the source is not read.
 *
 * <pre>{@code
 * context.consult("?- member(X, [a, b]).", new QueryInteractor() {
 *   @Override
 *   public boolean isQueryAllowed(JProlContext source, Term query) {
 *     return true;
 *   }
 *
 *   @Override
 *   public boolean onQuerySuccess(JProlContext source, Term query,
 *                                 Map<String, TermVar> varValues, int successCounter) {
 *     System.out.println(varValues.get("X").getValue());
 *     return true;
 *   }
 *
 *   @Override
 *   public void onQueryFail(JProlContext source, Term query, int successCounter) {
 *   }
 * });
 * }</pre>
 *
 * @see JProlContext#consult(Reader, QueryInteractor)
 * @since 3.0.0
 */
public interface QueryInteractor {
  /**
   * Accept or skip one {@code ?-} query before it is proved.
   *
   * <p>Called once per query, in source order. {@code query} is the goal after {@code ?-}.
   * Return {@code true} to prove it. Return {@code false} to ignore it and continue with the
   * next phrase. Neither choice by itself stops the consult.
   *
   * @param source context that is consulting the source, never {@code null}
   * @param query goal to prove, never {@code null}
   * @return {@code true} to prove {@code query}, {@code false} to skip it
   * @throws RuntimeException when the implementation fails; consult aborts with that exception
   */
  boolean isQueryAllowed(JProlContext source, Term query);

  /**
   * One solution of the current query has been found.
   *
   * <p>{@code varValues} maps each named variable text to the live variable from this solution.
   * {@link TermVar#getValue()} is the bound term, or {@code null} while that variable is still
   * free. The map is cleared before the next attempt, so keep a clone of any term that must
   * outlive this call. {@code successCounter} is {@code 1} for the first solution and increases
   * by one for each further solution.
   *
   * <p>Return {@code true} to ask for another solution. Return {@code false} to stop. Stopping
   * aborts the whole consult: the caller of {@code consult} receives
   * {@link com.igormaznitsa.jprol.exceptions.ProlHaltExecutionException} with status {@code 1}, and
   * later phrases are not consulted.
   *
   * @param source context that proved the query, never {@code null}
   * @param query goal that succeeded, never {@code null}
   * @param varValues named variables of this solution, never {@code null}; empty when the goal
   *     has no named variables
   * @param successCounter number of solutions found so far for this query, starting at {@code 1}
   * @return {@code true} to search for another solution, {@code false} to halt the consult
   * @throws RuntimeException when the implementation fails; consult aborts with that exception
   */
  boolean onQuerySuccess(JProlContext source, Term query, Map<String, TermVar> varValues,
                         int successCounter);

  /**
   * The current query has no further solution.
   *
   * <p>Called once, after the last success or immediately when the first attempt fails.
   * {@code successCounter} is the number of solutions already reported, {@code 0} when the query
   * failed at once. Consult then continues with the next phrase.
   *
   * @param source context that proved the query, never {@code null}
   * @param query goal that failed, never {@code null}
   * @param successCounter number of solutions found before this failure
   * @throws RuntimeException when the implementation fails; consult aborts with that exception
   */
  void onQueryFail(JProlContext source, Term query, int successCounter);
}
