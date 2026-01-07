package com.igormaznitsa.jprol.jsr223;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.libs.AbstractJProlLibrary;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import java.util.Calendar;

/**
 * Date/Time library
 */
public class DateTimeLibrary extends AbstractJProlLibrary {

  public DateTimeLibrary() {
    super("DateTimeLibrary");
  }

  @JProlPredicate(
      signature = "current_timestamp/1",
      validate = {"?integer"},
      determined = true,
      reference = "Get current Unix timestamp"
  )
  public static boolean predicateCurrentTimestamp(JProlChoicePoint goal, TermStruct predicate) {
    final Term term = predicate.getArgumentAt(0).tryGround();
    return term.unifyWith(Terms.newLong(System.currentTimeMillis() / 1000L));
  }

  @JProlPredicate(
      signature = "current_date/3",
      validate = {"?integer,?integer,?integer"},
      determined = true,
      reference = "Get current date as year, month, day"
  )
  public static boolean predicateCurrentDate(JProlChoicePoint goal, TermStruct predicate) {
    final Calendar cal = Calendar.getInstance();
    return
        predicate.getArgumentAt(0).tryGround()
            .unifyWith(Terms.newLong(cal.get(Calendar.YEAR)))
            && predicate.getArgumentAt(1).tryGround()
            .unifyWith(Terms.newLong(cal.get(Calendar.MONTH) + 1))
            && predicate.getArgumentAt(2).tryGround()
            .unifyWith(Terms.newLong(cal.get(Calendar.DAY_OF_MONTH)));
  }
}
