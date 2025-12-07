package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

public class JProlJsr223BootstrapLibrary extends AbstractJProlLibrary {

  public JProlJsr223BootstrapLibrary() {
    super("jprol-jsr223-bootstrap-lib");
  }

  @JProlPredicate(
      signature = "write/1",
      args = {"+term"},
      determined = true,
      reference = "Print message into current system output"
  )
  public static boolean predicateWrite(JProlChoicePoint goal, TermStruct predicate) {
    var out = System.out;
    if (out != null) {
      out.print(predicate.getElement(0).findNonVarOrSame().getText());
    }
    return true;
  }

  @JProlPredicate(
      signature = "writeln/1",
      args = {"+term"},
      determined = true,
      reference = "Print message into current system output"
  )
  public static boolean predicateWriteLn(JProlChoicePoint goal, TermStruct predicate) {
    var out = System.out;
    if (out != null) {
      out.print(predicate.getElement(0).findNonVarOrSame().getText());
      out.println();
    }
    return true;
  }

  @JProlPredicate(
      signature = "nl/0",
      determined = true,
      reference = "Write a newline character to the current system output stream. "
  )
  public static boolean predicateNl(JProlChoicePoint goal, TermStruct predicate) {
    var out = System.out;
    if (out != null) {
      out.println();
    }
    return true;
  }


}
