package com.igormaznitsa.jprol.logic.io;

import com.igormaznitsa.jprol.logic.JProlContext;
import java.io.Reader;
import java.io.Writer;

/**
 * Supplies the readers and writers behind Prolog stream identifiers.
 *
 * <p>Register a provider with {@link JProlContext#addIoResourceProvider}. The I/O library asks the
 * context for a stream by its atom, and the context asks providers in registration order.
 * {@code see/1}, {@code tell/1}, {@code append/1}, {@code consult/1} of a resource name, and the
 * current-input and current-output streams all go through this lookup.
 *
 * <p>Return the stream this provider owns. Return {@code null} to decline, so the next provider
 * is asked. The first non-null stream wins. A provider that throws stops the search: the
 * exception leaves {@link JProlContext#findResourceReader} or
 * {@link JProlContext#findResourceWriter}. Decline unknown ids with {@code null} when another
 * provider might own them.
 *
 * <p>The well-known ids are {@code user} (current input and output) and {@code error} (standard
 * error). When every provider declines, the engine uses standard input for a {@code user} reader
 * and standard output or standard error for a {@code user} or {@code error} writer. Any other
 * missing id fails the Prolog goal with an existence error or a file-not-found I/O error.
 * The caller closes the stream when the Prolog goal closes it, so the provider should return a
 * stream it is prepared to hand off.
 *
 * <pre>{@code
 * context.addIoResourceProvider(new IoResourceProvider() {
 *   @Override
 *   public Reader findReader(JProlContext context, String readerId) {
 *     return "user".equals(readerId) ? new StringReader("hello.\n") : null;
 *   }
 *
 *   @Override
 *   public Writer findWriter(JProlContext context, String writerId, boolean append) {
 *     return "user".equals(writerId) ? new StringWriter() : null;
 *   }
 * });
 * }</pre>
 *
 * @see JProlContext#findResourceReader(String)
 * @see JProlContext#findResourceWriter(String, boolean)
 */
public interface IoResourceProvider {

  /**
   * Open a reader for {@code readerId}, or decline.
   *
   * <p>The default declines every id. {@code user} is the current input stream.
   *
   * @param context  context that is resolving the stream, never {@code null}
   * @param readerId stream name from the Prolog goal, never {@code null}
   * @return a reader this provider owns, or {@code null} when the next provider should be asked
   * @throws RuntimeException when this provider fails while opening a stream it does own; the
   *                          search stops and the exception reaches the caller
   */
  default Reader findReader(JProlContext context, String readerId) {
    return null;
  }

  /**
   * Open a writer for {@code writerId}, or decline.
   *
   * <p>The default declines every id. {@code append} is {@code true} for {@code append/1} and for
   * the current output stream, and {@code false} for {@code tell/1}. A provider that ignores
   * {@code append} replaces the previous contents of the destination.
   *
   * @param context context that is resolving the stream, never {@code null}
   * @param writerId stream name from the Prolog goal, never {@code null}
   * @param append {@code true} to write after the existing contents, {@code false} to replace them
   * @return a writer this provider owns, or {@code null} when the next provider should be asked
   * @throws RuntimeException when this provider fails while opening a stream it does own; the
   *     search stops and the exception reaches the caller
   */
  default Writer findWriter(JProlContext context, String writerId, boolean append) {
    return null;
  }

}
