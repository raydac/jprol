package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.io.IOException;
import java.io.Writer;
import java.util.Optional;

/**
 * Bootstrap JProl library to be used in JSR 223 engine wrapper.
 *
 * @since 2.2.2
 */
public class JProlJsr223BootstrapLibrary extends AbstractJProlLibrary {

  /**
   * Alias for System.out
   */
  public static final String WRITER_OUT = "out";
  /**
   * Alias for System.err
   */
  public static final String WRITER_ERR = "err";
  /**
   * Alias for System.in
   */
  public static final String READER_IN = "in";

  public JProlJsr223BootstrapLibrary() {
    super("jprol-jsr223-bootstrap-lib");
  }

  private static Optional<Writer> findWriter(
      final String id,
      final JProlChoicePoint prolChoicePoint
  ) {
    return prolChoicePoint.getContext().findResourceWriter(id, true);
  }

  @JProlPredicate(
      signature = "write/1",
      validate = "?term",
      determined = true,
      reference = "Print the message to the standard output stream"
  )
  public static boolean predicateWrite(JProlChoicePoint goal, TermStruct predicate) {
    findWriter(WRITER_OUT, goal).ifPresent(out -> {
      var text = predicate.getArgumentAt(0).tryGround().getText();
      try {
        out.write(text.toCharArray());
        out.flush();
      } catch (IOException ex) {
        // ignore
      }
    });
    return true;
  }

  @JProlPredicate(
      signature = "format/1",
      synonyms = {"format/2", "format/3"},
      validate = {"+format", "+format,+list", "+atom,+format,+list"},
      determined = true,
      reference = "Print the message to the standard output stream"
  )
  public static boolean predicateFORMAT1_2_3(final JProlChoicePoint goal,
                                             final TermStruct predicate) {
    final Term output = predicate.getArity() == 3 ? predicate.getArgumentAt(0).tryGround() : null;
    final Term format = predicate.getArity() == 3 ? predicate.getArgumentAt(1).tryGround() :
        predicate.getArgumentAt(0).tryGround();

    final Term arguments;
    if (predicate.getArity() == 1) {
      arguments = TermList.NULL_LIST;
    } else {
      arguments = predicate.getArity() == 3 ? predicate.getArgumentAt(2).tryGround() :
          predicate.getArgumentAt(1).tryGround();
    }

    final String formatText = format.getText();

    if (arguments.getTermType() != TermType.LIST) {
      throw new ProlTypeErrorException("list", "Expected list", arguments);
    }

    findWriter(output == null ? WRITER_OUT : output.getText(), goal).ifPresent(out -> {
      final String formatted = "";
      try {
        out.write(formatted.toCharArray());
        out.flush();
      } catch (IOException ex) {

      }
    });
    return true;
  }

  @JProlPredicate(
      signature = "writeln/1",
      validate = "?term",
      determined = true,
      reference = "Print the message to the output followed by a newline"
  )
  public static boolean predicateWriteLn(JProlChoicePoint goal, TermStruct predicate) {
    findWriter(WRITER_OUT, goal).ifPresent(out -> {
      var text = predicate.getArgumentAt(0).tryGround().getText();
      try {
        out.write(text.toCharArray());
        out.write('\n');
        out.flush();
      } catch (IOException ex) {
        // ignore
      }
    });
    return true;
  }

  @JProlPredicate(
      signature = "nl/0",
      determined = true,
      reference = "Write a newline character to the output"
  )
  public static boolean predicateNl(JProlChoicePoint goal, TermStruct predicate) {
    findWriter(WRITER_OUT, goal).ifPresent(out -> {
      try {
        out.write('\n');
        out.flush();
      } catch (IOException ex) {
        // ignore
      }
    });
    return true;
  }


}
