package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;

/**
 * String manipulation library.
 */
public class StringLibrary extends AbstractJProlLibrary {

  public StringLibrary() {
    super("StringLibrary");
  }

  @JProlPredicate(
      signature = "str_upper/2",
      validate = {"+atom,?atom"},
      determined = true,
      reference = "Convert string to uppercase"
  )
  public static boolean predicateStrUpper(JProlChoicePoint goal, TermStruct predicate) {
    Term input = predicate.getArgumentAt(0).tryGround();
    String text = input.getText();
    return predicate.getArgumentAt(1).tryGround()
        .unifyWith(Terms.newAtom(text.toUpperCase()));
  }

  @JProlPredicate(
      signature = "str_lower/2",
      validate = {"+atom,?atom"},
      determined = true,
      reference = "Convert string to lowercase"
  )
  public static boolean predicateStrLower(JProlChoicePoint goal, TermStruct predicate) {
    Term input = predicate.getArgumentAt(0).tryGround();
    String text = input.getText();
    return predicate.getArgumentAt(1).tryGround()
        .unifyWith(Terms.newAtom(text.toLowerCase()));
  }

  @JProlPredicate(
      signature = "str_length/2",
      validate = {"+atom,?integer"},
      determined = true,
      reference = "Get length of string"
  )
  public static boolean predicateStrLength(JProlChoicePoint goal, TermStruct predicate) {
    Term input = predicate.getArgumentAt(0).tryGround();
    String text = input.getText();
    return predicate.getArgumentAt(1).tryGround().unifyWith(Terms.newLong(text.length()));
  }

  @JProlPredicate(
      signature = "str_reverse/2",
      validate = {"+atom,?atom"},
      determined = true,
      reference = "Reverse a string"
  )
  public static boolean predicateStrReverse(JProlChoicePoint goal, TermStruct predicate) {
    Term input = predicate.getArgumentAt(0).tryGround();
    String text = input.getText();
    String reversed = new StringBuilder(text).reverse().toString();
    return predicate.getArgumentAt(1).tryGround().unifyWith(Terms.newAtom(reversed));
  }

  @JProlPredicate(
      signature = "str_concat_custom/3",
      validate = {"+term,+term,?term"},
      determined = true,
      reference = "Concatenate two strings"
  )
  public static boolean predicateStrConcat(JProlChoicePoint goal, TermStruct predicate) {
    Term input1 = predicate.getArgumentAt(0).tryGround();
    Term input2 = predicate.getArgumentAt(1).tryGround();
    String result = input1.getText() + input2.getText();
    return predicate.getArgumentAt(2).tryGround().unifyWith(Terms.newAtom(result));
  }
}
