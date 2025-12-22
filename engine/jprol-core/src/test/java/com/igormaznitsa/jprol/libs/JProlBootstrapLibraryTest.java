package com.igormaznitsa.jprol.libs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlSystemFlag;
import org.junit.jupiter.api.Test;

class JProlBootstrapLibraryTest extends AbstractJProlTest {

  @Test
  void testEvaluablePredicateProcessorNotCalled() {
    final JProlChoicePoint point = prepareGoal("round(abc).", "round(X).");
    assertNotNull(point.prove());
    assertEquals("abc", point.findVar("X").get().findGroundOrSame().getText());
  }

  @Test
  void testCurrentPrologFlag2() {
    final JProlChoicePoint point = prepareGoal(
        "current_prolog_flag(" + JProlSystemFlag.VERIFY.getNameTerm().getText() + ",X).");
    assertNotNull(point.prove());
    final TermVar xVar = point.findVar("X").get();
    assertTrue(JProlSystemFlag.VERIFY.getDefaultValue().unifyTo(xVar.getValue()));
    assertNull(point.prove());

    final JProlChoicePoint all = prepareGoal("current_prolog_flag(A,B).");
    for (final JProlSystemFlag f : JProlSystemFlag.VALUES) {
      assertNotNull(all.prove());
      assertTrue(f.getNameTerm().dryUnifyTo(all.findVar("A").get()));
      assertTrue(f.getDefaultValue().dryUnifyTo(all.findVar("B").get()));
    }
    assertNull(all.prove());

    assertProlException("current_prolog_flag(5, V).", ProlTypeErrorException.class);
    assertProlException("current_prolog_flag(some_unknown_flag_lalala, V).", ProlDomainErrorException.class);
  }

  @Test
  void testSetPrologFlag2() {
    final JProlChoicePoint point = prepareGoal("current_prolog_flag(verify,true), set_prolog_flag(verify,false), current_prolog_flag(verify,false).");
    assertNotNull(point.prove());
    assertNull(point.prove());
    assertEquals("false", point.getContext().getSystemFlag(JProlSystemFlag.VERIFY).getText());
    assertProlException("set_prolog_flag(someunknownlalala, true).", ProlDomainErrorException.class);
    assertProlException("set_prolog_flag(" + JProlSystemFlag.VERSION_DATA.getNameTerm().getText() + ", true).", ProlDomainErrorException.class);
  }

  @Test
  void testIs2() {
    //['is'(X,float(3)),[[X <-- 3.0]]].
    assertProlException("is(X,float(3)).", ProlTypeErrorException.class);

    //['is'(Result,3 + 11.0),[[Result <-- 14.0]]].
    checkVarValues("is(Result,3+11.0).", "Result", 14.0d);

    //[(X = 1 + 2, 'is'(Y, X * 3)),[[X <-- (1 + 2), Y <-- 9]]]. % error? 1+2
    JProlChoicePoint goal = proveGoal("X=1+2,is(Y,X*3).");
    assertEquals("1 + 2", getVarAsText(goal, "X"));
    assertEquals(9L, getVarAsNumber(goal, "Y"));
    assertNull(goal.prove());

    //['is'(foo,77), failure]. % error? foo
    checkOnce("'is'(foo,77).", false);

    //['is'(77, N), instantiation_error].
    assertProlException("is(77,N).", ProlInstantiationErrorException.class);
    //['is'(77, foo), type_error(evaluable, foo/0)].
    assertProlException("is(77,foo).", ProlTypeErrorException.class);
  }

  @Test
  void testRepeat0() {
    //[(repeat,!,fail), failure].
    checkOnce("repeat,!,fail.", false);
  }

  @Test
  void testTrue0() {
    checkOnce("true.", true);
  }

  @Test
  void testFail0() {
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
    checkVarValues("';'((X=1, !), X=2).", "X", 1L);
    //[';'(X=1, X=2), [[X <-- 1], [X <-- 2]]].
    checkVarValues("';'(X=1, X=2).", "X", 1L, 2L);
  }

  @Test
  void testAnd2() {
    //[','(X=1, var(X)), failure].
    checkOnce("','(X=1,var(X)).", false);
    //[','(var(X), X=1), [[X <-- 1]]].
    checkVarValues("','(var(X),X=1).", "X", 1L);
    //[','(fail, call(3)), failure].
    checkOnce("','(fail,call(3)).", false);
    //[','(X = true, call(X)), [[X <-- true]]].
    checkVarValues("','(X=true,call(X)).", "X", "true");
    //[','(nofoo(X), call(X)), existence_error(procedure, nofoo/1)].
    assertProlException("','(nofoo(X), call(X)).", ProlExistenceErrorException.class);
  }

  @Test
  void testIfThenElse2() {
    //[';'('->'(true, true), fail), success].
    checkOnce(";('->'(true, true), fail).", true);
    //[';'('->'(fail, true), true), success].
    checkOnce(";('->'(fail, true), true).", true);
    //[';'('->'(true, fail), fail), failure].
    checkOnce("';'('->'(true, fail), fail).", false);
    //[';'('->'(fail, true), fail), failure].
    checkOnce("';'('->'(fail, true), fail).", false);
    //[';'('->'(true, X=1), X=2), [[X <-- 1]]].
    checkVarValues("true->X=1;X=2.", "X", 1L);
    //[';'('->'(fail, X=1), X=2), [[X <-- 2]]].
    checkVarValues("';'('->'(fail, X=1), X=2).", "X", 2L);
    //[';'('->'(true, ';'(X=1, X=2)), true), [[X <-- 1], [X <-- 2]]].
    checkVarValues("';'('->'(true, ';'(X=1, X=2)), true).", "X", 1L, 2L);
    //[';'('->'(';'(X=1, X=2), true), true), [[X <-- 1]]].
    checkVarValues("';'('->'(';'(X=1, X=2), true), true).", "X", 1L);
  }

  @Test
  void testCut0() {
    checkOnce("p1 :- \\+ q1. q1 :- fail. q1 :- true. p2:- \\+ q2. q2 :- !, fail. q2 :- true.", "p1.", false);
    checkOnce("p1 :- \\+ q1. q1 :- fail. q1 :- true. p2:- \\+ q2. q2 :- !, fail. q2 :- true.", "p2.", true);

    consultAndCheckVar("some(1). some(2):-!. some(3).", "some(X).", "X", "1", "2");
    checkVarsAfterCall("b(1). b(2). b(3). c(1). c(2). c(3). a(X,Y):-b(X),!,c(Y).", "a(X,Y).", new String[][] {
        new String[] {"X", "Y"}, new String[] {"1", "1"},
        new String[] {"X", "Y"}, new String[] {"1", "2"},
        new String[] {"X", "Y"}, new String[] {"1", "3"},
    });

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