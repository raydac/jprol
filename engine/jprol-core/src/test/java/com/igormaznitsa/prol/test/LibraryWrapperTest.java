package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.annotations.WrappedPredicate;
import com.igormaznitsa.prol.data.Term;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.libraries.ProlLibraryWrapper;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.utils.Utils;
import static org.junit.Assert.*;
import org.junit.Test;

import java.util.List;
import java.util.Set;
import org.junit.Ignore;


public class LibraryWrapperTest extends AbstractProlTest {

  volatile boolean testemptycalled = false;

  @WrappedPredicate
  public static void testArray(String[] strings) {
    assertEquals(strings.length, 5);
    assertEquals("one", strings[0]);
    assertEquals("two", strings[1]);
    assertEquals("three", strings[2]);
    assertEquals("four", strings[3]);
    assertEquals("five", strings[4]);
  }

  @WrappedPredicate
  public static void testFunc(int arg0, float arg1, boolean arg2) {
    assertEquals(999, arg0);
    assertTrue(Float.compare(arg1, 111.111f) == 0);
    assertEquals(true, arg2);
  }

  @WrappedPredicate
  public void testEmpty() {
    testemptycalled = true;
  }

  @WrappedPredicate
  public void testEvaluable(String out) {
    assertEquals("Hello world", out);
  }

  @WrappedPredicate(Name = "test222")
  public int testEvaluable121212(int arg1, int arg2) {
    return arg1 / arg2;
  }

  @WrappedPredicate
  public void testList(List<?> list) {
    assertEquals(list.size(), 5);
    assertEquals(list.get(0), 9);
    assertEquals(list.get(1), 10);
    assertEquals(list.get(2), 11);
    assertEquals(list.get(3), 12);
    assertEquals(list.get(4), 13);
  }

  @WrappedPredicate
  public void testSet(Set<?> set) {
    assertEquals(set.size(), 3);
    assertTrue(set.contains("one"));
    assertTrue(set.contains("two"));
    assertTrue(set.contains("three"));
  }

  @WrappedPredicate
  public void testArray2(int[] arr) {
    assertEquals(arr.length, 5);
    assertEquals(1, arr[0]);
    assertEquals(2, arr[1]);
    assertEquals(3, arr[2]);
    assertEquals(4, arr[3]);
    assertEquals(5, arr[4]);
  }

  @WrappedPredicate
  public void testChar(char[] chars) {
    assertEquals(chars.length, 5);
    assertEquals('H', chars[0]);
    assertEquals('e', chars[1]);
    assertEquals('l', chars[2]);
    assertEquals('l', chars[3]);
    assertEquals('o', chars[4]);
  }

  @Test
  @Ignore
  public void testLibraryWrapper() throws Exception {
    final ProlContext context = new ProlContext("test_context", DefaultProlStreamManagerImpl.getInstance());
    context.addLibrary(ProlLibraryWrapper.makeWrapper(this));

    final Goal goal = new Goal("testset([one,two,three,three,one,three,two,two]),testchar(['H',e,l,l,o]), testfunc(999,111.111,true), testlist([9,10,11,12,13]), testarray2([1,2,3,4,5]), testarray([one,two,three,four,five]), testempty,testevaluable('Hello world'), X is test222(32,16), X = 2.", context);

    int num = 0;
    int returned = 0;

    while (true) {
      final Term result = goal.solve();
      if (result == null) {
        break;
      }
      returned = (Integer) Utils.term2obj(context, Utils.findVarInsideTerm(result, "X"));
      num++;
    }

    assertEquals(num, 1);
    assertEquals(32 / 16, returned);
    assertTrue(testemptycalled);
  }
}
