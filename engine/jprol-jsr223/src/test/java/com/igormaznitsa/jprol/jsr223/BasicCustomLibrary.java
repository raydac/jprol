package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

/**
 * Basic custom library with simple predicates.
 */
public class BasicCustomLibrary extends AbstractJProlLibrary {

  public BasicCustomLibrary() {
    super("BasicCustomLibrary");
  }

  @JProlPredicate(
      signature = "hello_world/0",
      determined = true,
      reference = "Print hello world message"
  )
  public static boolean predicateHelloWorld(JProlChoicePoint goal, TermStruct predicate) {
    System.out.println("  [Library Output] Hello from custom JProl library!");
    return true;
  }

  @JProlPredicate(
      signature = "greet/1",
      validate = {"+atom"},
      determined = true,
      reference = "Greet someone by name"
  )
  public static boolean predicateGreet(JProlChoicePoint goal, TermStruct predicate) {
    Term name = predicate.getArgumentAt(0).tryGround();
    System.out.println("  [Library Output] Hello, " + name.getText() + "!");
    return true;
  }
}
