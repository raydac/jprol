package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Collection utilities library
 */
public class CollectionLibrary extends AbstractJProlLibrary {

  public CollectionLibrary() {
    super("CollectionLibrary");
  }

  @JProlPredicate(
      signature = "list_unique/2",
      validate = {"+list,?list"},
      determined = true,
      reference = "Remove duplicates from list"
  )
  public static boolean predicateListUnique(JProlChoicePoint goal, TermStruct predicate) {
    TermList inputList = predicate.getArgumentAt(0).tryGround();

    Set<String> seen = new LinkedHashSet<>();
    List<Term> uniqueTerms = new ArrayList<>();

    TermList current = inputList;
    while (!current.isNullList()) {
      Term head = current.getHead();
      String repr = head.toString();
      if (seen.add(repr)) {
        uniqueTerms.add(head);
      }

      Term tail = current.getTail();
      if (tail instanceof TermList) {
        current = (TermList) tail;
      } else {
        break;
      }
    }

    // Build result list
    return predicate.getArgumentAt(1).tryGround().unifyWith(TermList.listOf(uniqueTerms));
  }

  @JProlPredicate(
      signature = "list_sum_custom/2",
      validate = {"+list,?number"},
      determined = true,
      reference = "Calculate sum of numeric list"
  )
  public static boolean predicateListSum(JProlChoicePoint goal, TermStruct predicate) {
    TermList inputList = predicate.getArgumentAt(0).tryGround();

    long sum = 0;
    TermList current = inputList;

    while (!current.isNullList()) {
      Term head = current.getHead().tryGround();
      if (head instanceof NumericTerm) {
        sum += head.toNumber().longValue();
      }

      Term tail = current.getTail().tryGround();
      if (tail instanceof TermList) {
        current = (TermList) tail;
      } else {
        break;
      }
    }

    return predicate.getArgumentAt(1).tryGround().unifyWith(Terms.newLong(sum));
  }
}
