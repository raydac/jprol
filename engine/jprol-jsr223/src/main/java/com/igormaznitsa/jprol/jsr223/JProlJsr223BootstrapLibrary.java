package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.io.IOException;
import java.io.Writer;
import java.util.Optional;

public class JProlJsr223BootstrapLibrary extends AbstractJProlLibrary {

  public static final String WRITER_OUT = "out";
  public static final String WRITER_ERR = "err";
  public static final String READER_IN = "in";

  public JProlJsr223BootstrapLibrary() {
    super("jprol-jsr223-bootstrap-lib");
  }

  private static Optional<Writer> findWriter(final String writerId,
                                             final JProlChoicePoint prolChoicePoint) {
    return prolChoicePoint.getContext().findResourceWriter(writerId, true);
  }

  @JProlPredicate(
      signature = "write/1",
      args = {"+term"},
      determined = true,
      reference = "Print message to the output"
  )
  public static boolean predicateWrite(JProlChoicePoint goal, TermStruct predicate) {
    findWriter(WRITER_OUT, goal).ifPresent(out -> {
      var text = predicate.getElement(0).findNonVarOrSame().getText();
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
      signature = "writeln/1",
      args = {"+term"},
      determined = true,
      reference = "Print message into output with following start next line"
  )
  public static boolean predicateWriteLn(JProlChoicePoint goal, TermStruct predicate) {
    findWriter(WRITER_OUT, goal).ifPresent(out -> {
      var text = predicate.getElement(0).findNonVarOrSame().getText();
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
      reference = "Write a newline character to the output. "
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
