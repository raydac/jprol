package com.igormaznitsa.jprol.logic;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermVar;
import java.io.Reader;
import java.util.Map;

/**
 * Interface describes a class which get notifications for met queries '?-' during consult
 *
 * @see JProlContext#consult(Reader, QueryInteractor)
 * @since 3.0.0
 */
public interface QueryInteractor {
  /**
   * Called for found queries in format '?-...' to ignore them or prove.
   *
   * @param source the source context, can't be null
   * @param query  query to be processed, can't be null
   * @return true if processing of the query allowed, false otherwise
   */
  boolean isQueryAllowed(JProlContext source, Term query);

  /**
   * Called on successful prove of a query.
   *
   * @param source         source context, can't be null
   * @param query          proven query, can't be null
   * @param varValues      instantiated variables from the query, can't be null
   * @param successCounter counter of total success proves
   * @return true if try to re-prove the query, false if to stop proving
   */
  boolean onQuerySuccess(JProlContext source, Term query, Map<String, TermVar> varValues,
                         int successCounter);

  /**
   * Called on failed query prove.
   *
   * @param source         source context, can't be null
   * @param query          proven query, can't be null
   * @param successCounter counter of total success proves
   */
  void onQueryFail(JProlContext source, Term query, int successCounter);
}