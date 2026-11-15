package com.igormaznitsa.jprol.utils;

import java.io.Closeable;
import java.util.Iterator;

/**
 * Iterator that owns a resource and must be closed when the scan ends.
 *
 * <p>Knowledge-base scans return this type: clause lists, predicate indicators, and operator
 * tables. Use it in try-with-resources so {@link #close()} runs both after a normal end and when
 * {@link Iterator#next()} or the loop body throws. Build one around an existing iterator with
 * {@link ProlUtils#makeCloseableIterator}.
 *
 * <pre>{@code
 * try (CloseableIterator<TermStruct> clauses = base.iterate(
 *     IteratorType.ANY, template, (signature, term) -> { })) {
 *   while (clauses.hasNext()) {
 *     TermStruct clause = clauses.next();
 *   }
 * }
 * }</pre>
 *
 * <p>{@link Iterator#hasNext()} is {@code false} at the end. {@link Iterator#next()} then throws
 * {@link java.util.NoSuchElementException}. {@link Closeable#close()} may be called more than
 * once; a second close is a no-op for the iterators produced by this engine. The inherited close
 * declares {@link java.io.IOException}. The engine iterators do not throw it. A custom iterator
 * may.
 *
 * @param <T> element type
 */
public interface CloseableIterator<T> extends Iterator<T>, Closeable {
}
