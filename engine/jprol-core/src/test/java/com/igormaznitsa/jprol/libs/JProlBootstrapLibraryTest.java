package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JProlBootstrapLibraryTest extends AbstractJProlTest {
  @Test
  void testCurrentPrologFlag2() {
    final ChoicePoint point = prepareGoal("current_prolog_flag(" + JProlSystemFlag.VERIFY.getNameTerm().getText() + ",X).");
    assertNotNull(point.next());
    assertTrue(JProlSystemFlag.VERIFY.getDefaultValue().unifyTo(point.getVarForName("X").getValue()));
    assertNull(point.next());

    final ChoicePoint all = prepareGoal("current_prolog_flag(A,B).");
    for (final JProlSystemFlag f : JProlSystemFlag.values()) {
      assertNotNull(all.next());
      assertTrue(f.getNameTerm().dryUnifyTo(all.getVarForName("A")));
      assertTrue(f.getDefaultValue().dryUnifyTo(all.getVarForName("B")));
    }
    assertNull(all.next());

    checkException("current_prolog_flag(5, V).");
    checkException("current_prolog_flag(some_unknown_flag_lalala, V).");
  }

  @Test
  void testSetPrologFlag2() {
    final ChoicePoint point = prepareGoal("current_prolog_flag(verify,true), set_prolog_flag(verify,false), current_prolog_flag(verify,false).");
    assertNotNull(point.next());
    assertNull(point.next());
    assertEquals("false", point.getContext().getSystemFlag(JProlSystemFlag.VERIFY).getText());
    checkException("set_prolog_flag(someunknownlalala, true).");
    checkException("set_prolog_flag(" + JProlSystemFlag.VERSION_DATA.getNameTerm().getText() + ", true).");
  }

  @Test
  void testIs2() {
    //['is'(X,float(3)),[[X <-- 3.0]]].
    checkException("is(X,float(3)).");

    //['is'(Result,3 + 11.0),[[Result <-- 14.0]]].
    checkOnceVar("is(Result,3+11.0).", "Result", 14.0d);

    //[(X = 1 + 2, 'is'(Y, X * 3)),[[X <-- (1 + 2), Y <-- 9]]]. % error? 1+2
    ChoicePoint goal = proveGoal("X=1+2,is(Y,X*3).");
    assertEquals(goal.getVarAsText("X"), "1 + 2");
    assertEquals(goal.getVarAsNumber("Y"), 9L);
    assertNull(goal.next());

    //['is'(foo,77), failure]. % error? foo
    checkException("is(foo,77).");
    //['is'(77, N), instantiation_error].
    checkException("is(77,N).");
    //['is'(77, foo), type_error(evaluable, foo/0)].
    checkException("is(77,foo).");
  }

  @Test
  void testRepeat() {
    //[(repeat,!,fail), failure].
    checkOnce("repeat,!,fail.", false);
  }

  @Test
  void testTrue() {
    checkOnce("true.", true);
  }

  @Test
  void testFail() {
    checkOnce("fail.", false);
  }

  @Test
  void testDiff() {
    //['\\=='(1,1), failure].
    checkOnce("'\\\\=='(1,1).", false);
    //['\\=='(X,X), failure].
    checkOnce("'\\\\=='(X,X).", false);
    //['\\=='(1,2), success].
    checkOnce("'\\\\=='(1,2).", true);
    //['\\=='(X,1), success].
    checkOnce("'\\\\=='(X,1).", true);
    //['\\=='(X,Y), success].
    checkOnce("'\\\\=='(X,Y).", true);
    //['\\=='(_,_), success].
    checkOnce("'\\\\=='(_,_).", true);
    //['\\=='(X,a(X)), success].
    checkOnce("'\\\\=='(X,a(X)).", true);
    //['\\=='(f(a),f(a)), failure].
    checkOnce("'\\\\=='(f(a),f(a)).", false);
  }

  @Test
  void testEqu2() {
    //['=='(1,1), success].
    checkOnce("'=='(1,1).", true);
    //['=='(X,X), success].
    checkOnce("'=='(X,X).", true);
    //['=='(1,2), failure].
    checkOnce("'=='(1,2).", false);
    //['=='(X,1), failure].
    checkOnce("'=='(X,1).", false);
    //['=='(X,Y), failure].
    checkOnce("'=='(X,Y).", false);
    //['=='(_,_), failure].
    checkOnce("'=='(_,_).", false);
    //['=='(X,a(X)), failure].
    checkOnce("'=='(X,a(X)).", false);
    //['=='(f(a),f(a)), success].
    checkOnce("'=='(f(a),f(a)).", true);
  }

  @Test
  void testNotEqu2() {
    //['\\='(1,1), failure].
    checkOnce("'\\\\='(1,1).", false);
    //['\\='(X,1), failure].
    checkOnce("'\\\\='(X,1).", false);
    //['\\='(X,Y), failure].
    checkOnce("'\\\\='(X,Y).", false);
    //[('\\='(X,Y),'\\='(X,abc)), failure].
    checkOnce("'\\\\='(X,Y),'\\\\='(X,abc).", false);
    //['\\='(f(X,def),f(def,Y)), failure].
    checkOnce("'\\\\='(f(X,def),f(def,Y)).", false);
    //['\\='(1,2), success].
    checkOnce("'\\\\='(1,2).", true);
    //['\\='(1,1.0), success].
    checkOnce("'\\\\='(1,1.00001).", true);
    //['\\='(g(X),f(f(X))), success].
    checkOnce("'\\\\='(g(X),f(f(X))).", true);
    //['\\='(f(X,1),f(a(X))), success].
    checkOnce("'\\\\='(f(X,1),f(a(X))).", true);
    //['\\='(f(X,Y,X),f(a(X),a(Y),Y,2)), success].
    checkOnce("'\\\\='(f(X,Y,X),f(a(X),a(Y),Y,2)).", true);
  }

  @Test
  void testOr() {
    //[';'(true, fail), success].
    checkOnce("';'(true, fail).", true);
    //[';'((!, fail), true), failure].
    checkOnce("';'((!, fail), true).", false);
    //[';'(!, call(3)), success].
    checkOnce("';'(!, call(3)).", true);
    //[';'((X=1, !), X=2), [[X <-- 1]]].
    checkOnceVar("';'((X=1, !), X=2).", "X", 1L);
    //[';'(X=1, X=2), [[X <-- 1], [X <-- 2]]].
    checkOnceVar("';'(X=1, X=2).", "X", 1L, 2L);
  }

  @Test
  void testAnd() throws Exception {
    //[','(X=1, var(X)), failure].
    checkOnce("','(X=1,var(X)).", false);
    //[','(var(X), X=1), [[X <-- 1]]].
    checkOnceVar("','(var(X),X=1).", "X", 1L);
    //[','(fail, call(3)), failure].
    checkOnce("','(fail,call(3)).", false);
    //[','(X = true, call(X)), [[X <-- true]]].
    checkOnceVar("','(X=true,call(X)).", "X", "true");
    //[','(nofoo(X), call(X)), existence_error(procedure, nofoo/1)].
    checkOnce("','(nofoo(X), call(X)).", false);
  }

  @Test
  void testIfThenElse() {
    //[';'('->'(true, true), fail), success].
    checkOnce(";('->'(true, true), fail).", true);
    //[';'('->'(fail, true), true), success].
    checkOnce(";('->'(fail, true), true).", true);
    //[';'('->'(true, fail), fail), failure].
    checkOnce("';'('->'(true, fail), fail).", false);
    //[';'('->'(fail, true), fail), failure].
    checkOnce("';'('->'(fail, true), fail).", false);
    //[';'('->'(true, X=1), X=2), [[X <-- 1]]].
    checkOnceVar("true->X=1;X=2.", "X", 1L);
    //[';'('->'(fail, X=1), X=2), [[X <-- 2]]].
    checkOnceVar("';'('->'(fail, X=1), X=2).", "X", 2L);
    //[';'('->'(true, ';'(X=1, X=2)), true), [[X <-- 1], [X <-- 2]]].
    checkOnceVar("';'('->'(true, ';'(X=1, X=2)), true).", "X", 1L, 2L);
    //[';'('->'(';'(X=1, X=2), true), true), [[X <-- 1]]].
    checkOnceVar("';'('->'(';'(X=1, X=2), true), true).", "X", 1L);
  }

  @Test
  void testIfThen() {
    //['->'(true, true), success].
    checkOnce("->(true, true).", true);
    //['->'(true, fail), failure].
    checkOnce("->(true, fail).", false);
    //['->'(fail, true), failure].
    checkOnce("->(fail, true).", false);
    //['->'(true, X=1), [[X <-- 1]]].
    checkOnceVar("true -> X=1.", "X", 1L);
    //['->'(';'(X=1, X=2), true), [[X <-- 1]]].
    checkOnceVar("->(';'(X=1, X=2), true).", "X", 1L);
    //['->'(true, ';'(X=1, X=2)), [[X <-- 1], [X <-- 2]]].
    checkOnceVar("->(true, ';'(X=1, X=2)).", "X", 1L, 2L);
  }

  @Test
  void testCut0() {
    checkOnce("p1 :- \\+ q1. q1 :- fail. q1 :- true. p2:- \\+ q2. q2 :- !, fail. q2 :- true.", "p1.", false);
    checkOnce("p1 :- \\+ q1. q1 :- fail. q1 :- true. p2:- \\+ q2. q2 :- !, fail. q2 :- true.", "p2.", true);

    //[!, success].
    checkOnce("!.", true);
    //[(!,fail;true), failure].
    checkOnce("(!,fail;true).", false);
    //[(call(!),fail;true), success].
    checkOnce("call(!),fail;true.", true);
  }

  @Test
  void testNot1() {
    checkOnce("not(2=4).", true);
    checkOnce("not(2=2).", false);
  }

}