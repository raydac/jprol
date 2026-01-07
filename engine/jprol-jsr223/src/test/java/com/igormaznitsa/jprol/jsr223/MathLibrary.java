package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

/**
 * Math library with arithmetic operations.
 */
public class MathLibrary extends AbstractJProlLibrary {

  public MathLibrary() {
    super("MathLibrary");
  }

  @JProlPredicate(
      signature = "square/2",
      validate = {"+number,?number"},
      determined = true,
      reference = "Calculate square of a number"
  )
  public static boolean predicateSquare(JProlChoicePoint goal, TermStruct predicate) {
    NumericTerm input = predicate.getArgumentAt(0).tryGround();
    long value = input.toNumber().longValue();
    return predicate.getArgumentAt(1).tryGround().unifyWith(Terms.newLong(value * value));
  }

  @JProlPredicate(
      signature = "cube/2",
      validate = {"+number,?number"},
      determined = true,
      reference = "Calculate cube of a number"
  )
  public static boolean predicateCube(JProlChoicePoint goal, TermStruct predicate) {
    NumericTerm input = predicate.getArgumentAt(0).tryGround();
    long value = input.toNumber().longValue();
    return predicate.getArgumentAt(1).tryGround()
        .unifyWith(Terms.newLong(value * value * value));
  }

  @JProlPredicate(
      signature = "factorial_custom/2",
      validate = {"+integer,?integer"},
      determined = true,
      reference = "Calculate factorial"
  )
  public static boolean predicateFactorial(JProlChoicePoint goal, TermStruct predicate) {
    NumericTerm input = predicate.getArgumentAt(0).tryGround();
    int n = input.toNumber().intValue();

    long result = 1;
    for (int i = 2; i <= n; i++) {
      result *= i;
    }

    return predicate.getArgumentAt(1).tryGround().unifyWith(Terms.newLong(result));
  }

  @JProlPredicate(
      signature = "is_even/1",
      validate = {"+integer"},
      determined = true,
      reference = "Check if number is even"
  )
  public static boolean predicateIsEven(JProlChoicePoint goal, TermStruct predicate) {
    NumericTerm input = predicate.getArgumentAt(0).tryGround();
    long value = input.toNumber().longValue();
    return value % 2 == 0;
  }

  @JProlPredicate(
      signature = "is_odd/1",
      validate = {"+integer"},
      determined = true,
      reference = "Check if number is odd"
  )
  public static boolean predicateIsOdd(JProlChoicePoint goal, TermStruct predicate) {
    NumericTerm input = predicate.getArgumentAt(0).tryGround();
    long value = input.toNumber().longValue();
    return value % 2 != 0;
  }
}
