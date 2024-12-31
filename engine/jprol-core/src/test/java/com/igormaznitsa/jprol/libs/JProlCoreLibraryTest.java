package com.igormaznitsa.jprol.libs;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.TermVar;
import com.igormaznitsa.jprol.exceptions.ProlCustomErrorException;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlEvaluationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlExistenceErrorException;
import com.igormaznitsa.jprol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.exceptions.ProlRepresentationErrorException;
import com.igormaznitsa.jprol.exceptions.ProlTypeErrorException;
import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

class JProlCoreLibraryTest extends AbstractJProlTest {

  @Test
  void testAsserta1() {
    //[asserta((foo :- 4)), type_error(callable, 4)].
    assertProlException(":-dynamic(foo/0).", "asserta((foo :- 4)).", ProlTypeErrorException.class);

    consultAndCheckVarValues(":-dynamic(some1/1).",
        "asserta(some1(a)), asserta(some1(b)), some1(X).", "X", "'b'", "'a'");

    consultAndCheckVarValues(":-dynamic(bar/1).", "asserta((bar(X):-X is 3)), clause(bar(X),Y).",
        "Y", "X is 3");

    //[asserta(_), instantiation_error].
    assertProlException("asserta(_).", ProlInstantiationErrorException.class);

    //[asserta(4), type_error(callable, 4)].
    assertProlException("asserta(4).", ProlTypeErrorException.class);

    //[asserta((atom(_) :- true)), permission_error(modify,static_procedure,atom/1)].
    assertProlException("asserta((atom(_) :- true)).", ProlPermissionErrorException.class);

    //[(asserta((bar(X) :- X)), clause(bar(X), B)), [[B <-- call(X)]]].
    final JProlChoicePoint testCp = new JProlChoicePoint("asserta((bar(X):-X)), clause(bar(X),Y).",
        makeTestContext(":-dynamic(bar/1)."));
    assertNotNull(testCp.prove());
    final TermVar yVar = testCp.findVar("Y").get();
    assertSame(yVar.getThisValue().getTermType(), TermType.VAR);
    assertEquals("X", yVar.getThisValue().getText());
  }

  @Test
  void testAssertz1() {
    consultAndCheckVarValues(":-dynamic(some1/1).",
        "assertz(some1(a)), assertz(some1(b)), some1(X).", "X", "'a'", "'b'");
  }

  @Test
  void testRetracta1() {
    checkOnce(":-dynamic(someunknown/1).", "retract(someunknown(_)).", false);
    consultAndCheckVarValues(":-dynamic(some1/1).",
        "asserta(some1(a)), asserta(some1(b)), retracta(some1(_)), some1(X).", "X", "'a'");
  }

  @Test
  void testRetractz1() {
    consultAndCheckVarValues(":-dynamic(some1/1).",
        "asserta(some1(a)), asserta(some1(b)), retractz(some1(_)), some1(X).", "X", "'b'");
  }

  @Test
  void testRetractAll1() {
    checkOnce(":-dynamic(some1/1).",
        "asserta(some1(a)), asserta(some1(b)), retractall(some1(_)), not some1(_).", true);
  }

  @Test
  void testShiftL2() {
    checkVarValues("X is 1346 >> 3.", "X", "168");
    checkVarValues("X is 1346.423 >> 3.", "X", "168");
    checkVarValues("X is 1346 >> 0.", "X", "1346");
    assertProlException("X is 1346 >> a.", ProlTypeErrorException.class);
    assertProlException("X is a >> 3.", ProlTypeErrorException.class);
  }

  @Test
  void testShiftR2() {
    checkVarValues("X is 1346 << 3.", "X", "10768");
    checkVarValues("X is 1346.423 << 3.", "X", "10768");
    checkVarValues("X is 1346 << 0.", "X", "1346");
    checkVarValues("X is 1346 << 3.4.", "X", "10768");
    assertProlException("X is 1346 << a.", ProlTypeErrorException.class);
  }

  @Test
  void testIDiv2() {
    checkVarValues("X is 10 // 3.", "X", "3");
    checkVarValues("X is 10 // 2.", "X", "5");
    checkVarValues("X is -11 // 4.", "X", "-2");

    assertProlException("X is 10 // 0.", ProlEvaluationErrorException.class);
    assertProlException("X is 10 // 0.3.", ProlTypeErrorException.class);
    assertProlException("X is 10.5 // 3.", ProlTypeErrorException.class);
  }

  @Test
  void testDiv2() {
    checkVarValues("X is 10 / 3.", "X", "3");
    checkVarValues("X is 10 / 2.", "X", "5");
    checkVarValues("X is -11 / 4.", "X", "-2");
    checkVarValues("X is -10 / 2.5.", "X", "-4.0");
    checkVarValues("X is 2.5 / 2.5.", "X", "1.0");

    assertProlException("X is 10.3 / 0.", ProlEvaluationErrorException.class);
    assertProlException("X is 10 / 0.0.", ProlEvaluationErrorException.class);
  }

  @Test
  void testSort2() {
    checkVarsAfterCall("sort([B,A,1], [2,3,1]).", new String[][] {new String[] {"A", "B"}, new String[] {"3", "2"}});
    checkVarValues("sort([4,2,5,1,4,7,8,2,6,5,3,5,9,1,0],X).", "X", "[0,1,2,3,4,5,6,7,8,9]");
  }

  @Test
  void testPause1() {
    final long start = System.currentTimeMillis();
    checkVarValues("X=100,pause(X).", "X", "100");
    assertTrue(System.currentTimeMillis() - start >= 100L);
    assertProlException("pause(X).", ProlInstantiationErrorException.class);
    assertProlException("pause(a(1)).", ProlTypeErrorException.class);
  }

  @Test
  void testNeg1() {
    checkVarValues("C=-1, X is -C.", "X", "1");
    checkVarValues("C=1, X is -C.", "X", "-1");
    checkVarValues("C=1.5, X is -C.", "X", "-1.5");
    checkVarValues("C=-1.5, X is -C.", "X", "1.5");
    assertProlException("X is -C.", ProlInstantiationErrorException.class);
  }

  @Test
  void testPlus1() {
    checkVarValues("C=-1, X is +C.", "X", "-1");
    checkVarValues("C=1, X is +C.", "X", "1");
    checkVarValues("C=1.5, X is +C.", "X", "1.5");
    checkVarValues("C=-1.5, X is +C.", "X", "-1.5");
    assertProlException("X is +C.", ProlInstantiationErrorException.class);
  }

  @Test
  void testVar1() {
    checkOnce("var(C).", true);
    checkOnce("var(1).", false);
    checkOnce("C=A,var(C).", true);
    checkOnce("C=A,A=3,var(C).", false);
    checkOnce("var([]).", false);
    checkOnce("var(a(1,2)).s", false);
  }

  @Test
  void testRnd2() {
    checkOnce("rnd(5,X).", true);
    checkOnce("rnd([1,2,3],X).", true);
    checkVarValues("rnd([],X).", "X", "[]");
  }

  @Test
  void testFor3() {
    checkVarValues("for(X,0,1).", "X", "0", "1");
    checkOnce("for(100,100,101).", true);
    checkVarValues("for(X,4,1).", "X", "4", "3", "2", "1");
    checkOnce("for(5,1,4).", false);
    assertProlException("for(5,X,4).", ProlInstantiationErrorException.class);
    assertProlException("for(5,1,X).", ProlInstantiationErrorException.class);
    assertProlException("for(5,A,X).", ProlInstantiationErrorException.class);
  }

  @Test
  void testIfThen2() {
    //['->'(true, true), success].
    checkOnce("->(true, true).", true);
    //['->'(true, fail), failure].
    checkOnce("->(true, fail).", false);
    //['->'(fail, true), failure].
    checkOnce("->(fail, true).", false);
    //['->'(true, X=1), [[X <-- 1]]].
    checkVarValues("true -> X=1.", "X", 1L);
    //['->'(';'(X=1, X=2), true), [[X <-- 1]]].
    checkVarValues("->(';'(X=1, X=2), true).", "X", 1L);
    //['->'(true, ';'(X=1, X=2)), [[X <-- 1], [X <-- 2]]].
    checkVarValues("->(true, ';'(X=1, X=2)).", "X", 1L, 2L);

    consultAndCheckVar("max(X,Y,Z):-(X=<Y->Z=Y;Z=X).", "max(1,2,Z).", "Z", "2");
    consultAndCheckVar("max(X,Y,Z):-(X=<Y->Z=Y;Z=X).", "max(2,1,Z).", "Z", "2");
  }

  @Test
  void testTermAsList3() {
    checkVarValues("foo(hello, X)=..List.", "List", "['foo','hello',X]");
    checkVarValues("alpha=..List.", "List", "['alpha']");
    checkVarValues("[1,2,3]=..List.", "List", "[1,2,3]");
    checkVarValues("[]=..List.", "List", "[]");
    checkVarValues("Term=..[baz, foo(1)].", "Term", "baz(foo(1))");
    assertProlException("a()=..L.", ProlDomainErrorException.class);
  }

  @Test
  void testXor2() {
    checkVarValues("X is xor(123,334).", "X", String.valueOf(123 ^ 334));
    checkVarValues("X is xor(123,xor(334,4452234)).", "X", String.valueOf(123 ^ 334 ^ 4452234));
    assertProlException("X is xor(123,xor(334.24,4452234.21)).", ProlTypeErrorException.class);
  }

  @Test
  void testBitwiseNot1() {
    checkVarValues("X is \\ 1.", "X", String.valueOf(~1));
    assertProlException("X is \\ 1.445.", ProlTypeErrorException.class);
  }

  @Test
  void testBitwiseOr2() {
    checkVarValues("X is 3434 \\/ 2234123.", "X", String.valueOf(3434 | 2234123));
    assertProlException("X is 3434.3312 \\/ 2234123.", ProlTypeErrorException.class);
    assertProlException("X is 3434 \\/ 2234123.332.", ProlTypeErrorException.class);
    assertProlException("X is 3434.123 \\/ 2234123.332.", ProlTypeErrorException.class);
  }

  @Test
  void testBitwiseAnd2() {
    checkVarValues("X is 3434 /\\ 2234123.", "X", String.valueOf(3434 & 2234123));
    assertProlException("X is 3434.3312 /\\ 2234123.", ProlTypeErrorException.class);
    assertProlException("X is 3434 /\\ 2234123.332.", ProlTypeErrorException.class);
    assertProlException("X is 3434.123 /\\ 2234123.332.", ProlTypeErrorException.class);
  }

  @Test
  void testMod2() {
    checkVarValues("X is mod(16,8).", "X", String.valueOf(0));
    checkVarValues("X is mod(344,123).", "X", String.valueOf(344 % 123));
    assertProlException("X is mod(344.456,123.11).", ProlTypeErrorException.class);
    assertProlException("X is mod(344,0).", ProlEvaluationErrorException.class);
  }

  @Test
  void testRem2() {
    checkVarValues("X is rem(16,8).", "X", String.valueOf(0));
    checkVarValues("X is rem(344,123).", "X", String.valueOf(98));
    assertProlException("X is rem(344.456,123.11).", ProlTypeErrorException.class);
    assertProlException("X is rem(344,0).", ProlEvaluationErrorException.class);
    assertProlException("X is rem('a',12).", ProlTypeErrorException.class);
    assertProlException("X is rem(A,12).", ProlInstantiationErrorException.class);
    assertProlException("X is rem(12,B).", ProlInstantiationErrorException.class);
    assertProlException("X is rem(0,'b').", ProlTypeErrorException.class);
  }

  @Test
  void testPow2() {
    checkVarValues("X is 16**8.", "X", String.valueOf(Math.pow(16, 8)));
    checkVarValues("X is 16**0.", "X", String.valueOf(1.0));
    checkVarValues("X is 16**-2.", "X", String.valueOf(Math.pow(16, -2)));
    assertProlException("X is 'a'**-2.", ProlTypeErrorException.class);
    assertProlException("X is A**-2.", ProlInstantiationErrorException.class);
    assertProlException("X is 16**-'b'.", ProlTypeErrorException.class);
    assertProlException("X is 2**-'b'.", ProlTypeErrorException.class);
    assertProlException("X is A**-B.", ProlInstantiationErrorException.class);
  }

  @Test
  void testAdd2() {
    checkVarValues("X is 16+8.", "X", String.valueOf(24));
    checkVarValues("X is 16+0.", "X", String.valueOf(16));
    checkVarValues("X is 16+-2.", "X", String.valueOf(14));
    checkVarValues("X is 16+2.345.", "X", String.valueOf(18.345));
    checkVarValues("X is -16.223+2.345.", "X", String.valueOf(-13.877999999999998D));
  }

  @Test
  void testSin1() {
    checkVarValues("X is sin(3.14).", "X", String.valueOf(Math.sin(3.14D)));
    checkVarValues("X is sin(0).", "X", String.valueOf(Math.sin(0)));
  }

  @Test
  void testCos1() {
    checkVarValues("X is cos(3.14).", "X", String.valueOf(Math.cos(3.14D)));
    checkVarValues("X is cos(0).", "X", String.valueOf(Math.cos(0)));
  }

  @Test
  void testAtan1() {
    checkVarValues("X is atan(3.14).", "X", String.valueOf(Math.atan(3.14D)));
    checkVarValues("X is atan(0).", "X", String.valueOf(Math.atan(0)));
  }

  @Test
  void testExp1() {
    checkVarValues("X is exp(3.14).", "X", String.valueOf(Math.exp(3.14D)));
    checkVarValues("X is exp(0).", "X", String.valueOf(Math.exp(0)));
  }

  @Test
  void testLog1() {
    checkVarValues("X is log(3.14).", "X", String.valueOf(Math.log(3.14D)));
    checkVarValues("X is log(345).", "X", String.valueOf(Math.log(345)));
  }

  @Test
  void testSqrt1() {
    checkVarValues("X is sqrt(3.14).", "X", String.valueOf(Math.sqrt(3.14D)));
    checkVarValues("X is sqrt(0).", "X", String.valueOf(Math.sqrt(0)));
  }

  @Test
  void testAbs1() {
    checkVarValues("X is abs(3.14).", "X", String.valueOf(Math.abs(3.14D)));
    checkVarValues("X is abs(0).", "X", String.valueOf(0));
    checkVarValues("X is abs(-33.2).", "X", String.valueOf(Math.abs(-33.2D)));
  }

  @Test
  void testOp3() {
    checkOnce("op(334, xfx, some).", true);
    checkOnce("op(444, xf, [one,two,three]).", true);
    assertProlException("op(443, xfx, '(').", ProlPermissionErrorException.class);
  }

  @Test
  void testCurrentOp3() {
    final JProlContext context = makeContextAndConsult(":-op(1099, xf, some_op).");
    checkVarValues(context, "current_op(1099, xf, X).", "X", "'some_op'");
    checkVarValues(context, "current_op(X, xf, some_op).", "X", "1099");
    checkVarValues(context, "current_op(1099, X, some_op).", "X", "'xf'");
  }

  @Test
  void testSign1() {
    checkVarValues("X is sign(3.14).", "X", String.valueOf(1));
    checkVarValues("X is sign(0).", "X", String.valueOf(0));
    checkVarValues("X is sign(-33.2).", "X", String.valueOf(-1));
    checkVarValues("X is sign(45).", "X", String.valueOf(1));
    checkVarValues("X is sign(0).", "X", String.valueOf(0));
    checkVarValues("X is sign(-32).", "X", String.valueOf(-1));
  }

  @Test
  void testFloatIntegerPart1() {
    checkVarValues("X is float_integer_part(3.14).", "X", String.valueOf(3));
    checkVarValues("X is float_integer_part(4).", "X", String.valueOf(4));
  }

  @Test
  void testFloatFractionalPart1() {
    checkVarValues("X is float_fractional_part(3.14).", "X", String.valueOf(3.14D - 3));
    checkVarValues("X is float_fractional_part(4).", "X", String.valueOf(0.0d));
  }

  @Test
  void testFloor1() {
    checkVarValues("X is floor(3.14).", "X", String.valueOf((long) Math.floor(3.14D)));
    checkVarValues("X is floor(0.0).", "X", String.valueOf((long) Math.floor(0.0D)));
    checkVarValues("X is floor(14.9).", "X", String.valueOf((long) Math.floor(14.9D)));
  }

  @Test
  void testTruncate1() {
    checkVarValues("X is truncate(3.14).", "X", String.valueOf(3));
    checkVarValues("X is truncate(-3.14).", "X", String.valueOf(-3));
    checkVarValues("X is truncate(0).", "X", String.valueOf(0));
    checkVarValues("X is truncate(1.9).", "X", String.valueOf(1));
    checkVarValues("X is truncate(-1.9).", "X", String.valueOf(-1));
  }

  @Test
  void testCeiling1() {
    checkVarValues("X is ceiling(3.14).", "X", String.valueOf(4));
    checkVarValues("X is ceiling(-3.14).", "X", String.valueOf(-3));
    checkVarValues("X is ceiling(0).", "X", String.valueOf(0));
    checkVarValues("X is ceiling(1.9).", "X", String.valueOf(2));
    checkVarValues("X is ceiling(-1.9).", "X", String.valueOf(-1));
  }

  @Test
  void testRound1() {
    checkVarValues("X is round(3.14).", "X", String.valueOf(3));
    checkVarValues("X is round(-3.14).", "X", String.valueOf(-3));
    checkVarValues("X is round(0).", "X", String.valueOf(0));
    checkVarValues("X is round(1.9).", "X", String.valueOf(2));
    checkVarValues("X is round(-1.9).", "X", String.valueOf(-2));
  }

  @Test
  void testAbolish1() {
    //[(current_prolog_flag(max_arity,A), X is A + 1, abolish(foo/X)), representation_error(max_arity)].
    assertProlException("current_prolog_flag(max_arity,A), X is A + 1, abolish(foo/X).", ProlRepresentationErrorException.class);

    checkOnce(":-dynamic(test/1).", "assert(test(1)),test(X),X==1,abolish(test/1),\\+ test(_).",
        true);

    //[abolish(abolish/1), permission_error(modify,static_procedure,abolish/1)].
    assertProlException("abolish(abolish/1).", ProlPermissionErrorException.class);

    //[abolish(foo/a), type_error(integer,a)].
    assertProlException("abolish(foo/a).", ProlTypeErrorException.class);

    //[abolish(foo/(-1)), domain_error(not_less_than_zero,-1)].
    assertProlException("abolish(foo/(-1)).", ProlDomainErrorException.class);

    //[abolish(5/2), type_error(atom,5)].
    assertProlException("abolish(5/2).", ProlTypeErrorException.class);
  }

  @Test
  void testRetract1() {
    //[retract((atom(_) :- X == '[]')),permission_error(modify,static_procedure,atom/1)].
    assertProlException("retract((atom(_):-X=='[]')).", ProlPermissionErrorException.class);
  }

  @Test
  void testFail() {
    //[undef_pred, existence_error(procedure, undef_pred/0)]. % the value of flag 'unknown' is 'error'.
    assertProlException("undef_pred.", ProlExistenceErrorException.class);
    //[(set_prolog_flag(unknown, fail), undef_pred), failure].
    checkOnce("set_prolog_flag(unknown, fail), undef_pred.", false);
    //[(set_prolog_flag(unknown, warning), undef_pred), failure].
    checkOnce("set_prolog_flag(unknown, warning), undef_pred.", false);
    //[fail, failure].
    checkOnce("fail.", false);
  }

  @Test
  void testFindAll3() {
    checkVarValues("findall(X,(X=1;X=2),X).", "X", "[1,2]");
    //[findall(X,(X=1 ; X=2),S),[[S <-- [1,2]]]].
    checkVarValues("findall(X,(X=1;X=2),S).", "S", "[1,2]");
    //[findall(X+Y,(X=1),S),[[S <-- [1+_]]]].
    checkVarValues("findall(X+Y,(X=1),S).", "S", "[1 + Y]"); // changes
    //[findall(X,fail,L),[[L <-- []]]].
    checkVarValues("findall(X,fail,L).", "L", "[]");
    //[findall(X,(X=1 ; X=1),S),[[S <-- [1,1]]]].
    checkVarValues("findall(X,(X=1;X=1),S).", "S", "[1,1]");
    checkVarValues("findall(1, (Y = 1; Y = 2),L).", "L", "[1,1]");
    checkVarValues("findall(X,(X=Y;X=Z),L).", "L", "[Y,Z]");
    //[findall(X,(X=2 ; X=1),[1,2]), failure].
    checkOnce("findall(X,(X=2;X=1),[1,2]).", false);

    //[findall(X,(X=1 ; X=2),[X,Y]), [[X <-- 1, Y <-- 2]]].
    final JProlChoicePoint goal = proveGoal("findall(X,(X=1;X=2),[X1,Y1])."); // changed from original because at Prol all variables linked by their names inside a goal, so that it is not a bug, it is a feature
    assertEquals("1", getVarAsText(goal, "X1"));
    assertEquals("2", getVarAsText(goal, "Y1"));
    assertNull(goal.prove());

    //[findall(X,Goal,S),instantiation_error]. % Culprit Goal
    assertProlException("findall(X,Goal,S).", ProlInstantiationErrorException.class);
    //[findall(X,4,S),type_error(callable, 4)].
    assertProlException("findall(X,4,S).", ProlTypeErrorException.class);
    //[findall(X,call(1),S),type_error(callable, 1)].
    assertProlException("findall(X,call(1),S).", ProlTypeErrorException.class);
  }

  @Test
  void testSetOf3() {
    //[setof(X,(X=1;X=2),L), [[L <-- [1, 2]]]].
    checkVarValues("setof(X,(X=1;X=2),L).", "L", "[1,2]");
    //[setof(X,(X=1;X=2),X), [[X <-- [1, 2]]]].
    checkVarValues("setof(X,(X=1;X=2),X).", "X", "[1,2]");
    //[setof(X,(X=2;X=1),L), [[L <-- [1, 2]]]].
    checkVarValues("setof(X,(X=2;X=1),L).", "L", "[1,2]");
    //[setof(X,(X=2;X=2),L), [[L <-- [2]]]].
    checkVarValues("setof(X,(X=2;X=2),L).", "L", "[2]");
    //[setof(X,fail,L), failure].
    checkOnce("setof(X,fail,L).", false);

    //[setof(1,(Y=2;Y=1),L), [[L <-- [1], Y <-- 1], [L <-- [1], Y <-- 2]]].
    final JProlChoicePoint goal1 = prepareGoal("setof(1,(Y=2;Y=1),L).");
    assertNotNull(goal1.prove());
    assertEquals("[1]", getVarAsText(goal1, "L"));
    assertEquals("2", getVarAsText(goal1, "Y"));
    assertNotNull(goal1.prove());
    assertEquals("[1]", getVarAsText(goal1, "L"));
    assertEquals("1", getVarAsText(goal1, "Y"));
    assertNull(goal1.prove());

    //[setof(f(X,Y),(X=a;Y=b),L), [[L <-- [f(_, b), f(a, _)]]]].
    checkVarValues("setof(f(X,Y),(X=a;Y=b),L).", "L", "[f(X,'b'),f('a',Y)]");
    //[setof(X,Y^((X=1,Y=1);(X=2,Y=2)),S), [[S <-- [1, 2]]]].
    checkVarValues("setof(X,Y^((X=1,Y=1);(X=2,Y=2)),S).", "S", "[1,2]");
    //[setof(X,Y^((X=1;Y=1);(X=2,Y=2)),S), [[S <-- [_, 1, 2]]]].
    checkVarValues("setof(X,Y^((X=1;Y=1);(X=2,Y=2)),S).", "S", "[X,1,2]");
    //[(set_prolog_flag(unknown, warning), setof(X,(Y^(X=1;Y=1);X=3),S)), [[S <-- [3]]]].
    checkVarValues("setof(X,(Y^(X=1;Y=1);X=3),S).", "S", "[3]");
    //[(set_prolog_flag(unknown, warning), setof(X,Y^(X=1;Y=1;X=3),S)), [[S <-- [_, 1,3]]]].
    checkVarValues("setof(X,Y^(X=1;Y=1;X=3),S).", "S", "[X,1,3]");
    //[setof(X,(X=Y;X=Z;Y=1),L), [[L <-- [Y, Z]], [L <-- [_], Y <-- 1]]].
    final JProlChoicePoint goal2 = prepareGoal("setof(X,(X=Y;X=Z;Y=1),L).");
    assertNotNull(goal2.prove());
    assertEquals("[Y,Z]", getVarAsText(goal2, "L"));
    assertNull(getVarAsText(goal2, "Y"));
    assertNotNull(goal2.prove());
    assertEquals("[X]", getVarAsText(goal2, "L"));
    assertEquals("1", getVarAsText(goal2, "Y"));
    assertNull(goal2.prove());
    //[setof(X, X^(true; 4),L), type_error(callable,(true;4))].
    assertProlException("setof(X, X^(true; 4),L).", ProlTypeErrorException.class);
    //[setof(X,1,L), type_error(callable,1)].
    assertProlException("setof(X,1,L).", ProlTypeErrorException.class);
  }

  @Test
  void testBagOf3() {
    //[bagof(X, (X = 1; X = 2),L), [[L<-- [1, 2]]]].
    checkVarValues("bagof(X,(X=1;X=2),L).", "L", "[1,2]");
    //[bagof(X, (X = 1; X = 2),X), [[X<-- [1, 2]]]].
    checkVarValues("bagof(X,(X=1;X=2),X).", "X", "[1,2]");

    //[bagof(X, (X = Y; X = Z),L), [[L<-- [Y, Z]]]].
    checkVarValues("bagof(X,(X=Y;X=Z),L).", "L", "[Y,Z]");

    //[bagof(1, (Y = 1; Y = 2),L), [[L<-- [1], Y<-- 1], [L<-- [1], Y<-- 2]]].
    final JProlChoicePoint goal = prepareGoal("bagof(1,(Y = 1; Y = 2),L).");
    assertNotNull(goal.prove());
    assertEquals("[1]", getVarAsText(goal, "L"));
    assertEquals("1", getVarAsText(goal, "Y"));
    assertNotNull(goal.prove());
    assertEquals("[1]", getVarAsText(goal, "L"));
    assertEquals("2", getVarAsText(goal, "Y"));
    assertNull(goal.prove());

    //[bagof(X, fail, L), failure].
    checkOnce("bagof(X, fail, L).", false);

    //[bagof(X, (X = Y; X = Z; Y = 1),L), [[L<-- [Y, Z]], [L<-- [_], Y<-- 1]]].
    final JProlChoicePoint goal2 = prepareGoal("bagof(X, (X = Y; X = Z; Y = 1),L).");
    assertNotNull(goal2.prove());
    assertEquals("[Y,Z]", getVarAsText(goal2, "L"));
    assertNotNull(goal2.prove());
    assertEquals("[X]", getVarAsText(goal2, "L"));
    assertEquals("1", getVarAsText(goal2, "Y"));
    assertNull(goal2.prove());

    //[(set_prolog_flag(unknown, warning),bagof(X, (Y ^ (X = 1;Y = 1);X = 3),S)), [[S<-- [3]]]].
    final JProlChoicePoint goal3 = prepareGoal("bagof(X, (Y ^ (X = 1;Y = 1);X = 3),S).");
    assertNotNull(goal3.prove());
    assertEquals("[3]", getVarAsText(goal3, "S"));
    assertNull(goal3.prove());

    //[bagof(X, Y ^ ((X = 1; Y = 1);(X = 2,Y = 2)),S), [[S<-- [1, _, 2]]]].
    final JProlChoicePoint goal4 = prepareGoal("bagof(X, Y ^ ((X = 1; Y = 1);(X = 2,Y = 2)),S).");
    assertNotNull(goal4.prove());
    assertEquals("[1,2,X]", getVarAsText(goal4, "S"));
    assertNull(goal4.prove());

    //[bagof(X, Y ^ ((X = 1, Y = 1);(X = 2,Y = 2)),S), [[S<-- [1, 2]]]].
    final JProlChoicePoint goal5 = prepareGoal("bagof(X, Y ^ ((X = 1, Y = 1);(X = 2,Y = 2)),S).");
    assertNotNull(goal5.prove());
    assertEquals("[1,2]", getVarAsText(goal5, "S"));
    assertNull(goal5.prove());

    //[bagof(f(X, Y), (X = a; Y = b),L), [[L<-- [f(a, _), f(_, b)]]]].
    final JProlChoicePoint goal6 = prepareGoal("bagof(f(X, Y), (X = a; Y = b),L).");
    assertNotNull(goal6.prove());
    assertEquals("[f('a',Y),f(X,'b')]", getVarAsText(goal6, "L"));
    assertNull(goal6.prove());

    //[bagof(X, Y ^ Z, L), instantiation_error].
    assertProlException("bagof(X, Y ^ Z, L).", ProlInstantiationErrorException.class);

    //[bagof(X, 1, L), type_error(callable, 1)].
    assertProlException("bagof(X, 1, L).", ProlTypeErrorException.class);
  }

  @Test
  void testAssertZ1() {
    //[assertz((foo(X) :- X -> call(X))), success].
    checkOnce(":-dynamic(foo/1).", "assertz((foo(X):-X->call(X))).", true);
    //[assertz(_), instantiation_error].
    assertProlException("assertz(_).", ProlInstantiationErrorException.class);
    //[assertz(4), type_error(callable, 4)].
    assertProlException("assertz(4).", ProlTypeErrorException.class);

    //[assertz((foo :- 4)), type_error(callable, 4)].
    //checkException("assertz((foo:-4)).");// prol doesn't check the term fully

    //[assertz((atom(_) :- true)), permission_error(modify,static_procedure,atom/1)].
    assertProlException("assertz((atom(_):-true)).", ProlPermissionErrorException.class);
  }

  @Test
  void testAssertA1() {
    //[(asserta((bar(X) :- X)), clause(bar(X), B)), [[B <-- call(X)]]].
    consultAndCheckVarValues(":-dynamic([bar/1]).", "asserta(bar(X):-call(X)),clause(bar(X),B).",
        "B", "call(X)");

    //[asserta(_), instantiation_error].
    assertProlException("asserta(_).", ProlInstantiationErrorException.class);

    //[asserta(4), type_error(callable, 4)].
    assertProlException("asserta(4).", ProlTypeErrorException.class);

    //[asserta((foo :- 4)), type_error(callable, 4)].
    assertProlException(":-dynamic(foo/0).", "asserta((foo:-4)).", ProlTypeErrorException.class);

    //[asserta((atom(_) :- true)), permission_error(modify,static_procedure,atom/1)].
    assertProlException("asserta((atom(_):-true)).", ProlPermissionErrorException.class);
  }

  @Test
  void testNumberCodes2() {
    //[number_codes(A,[0' ,0'3]), [[A <-- 3]]]. !!! SPACE IS NOT SUPPORTED IN JPROL
    checkVarValues("number_codes(A,[0' ,0'3]).", "A", "' 3'");

    //[number_codes(33.0,[0'3,0'.,0'3,0'E,0'+,0'0,0'1]), success].
    checkOnce("number_codes(33.0,[0'3,0'.,0'3,0'E,0'+,0'0,0'1]).", true);
    //[number_codes(A,[0'-,0'2,0'5]), [[A <-- (-25)]]].
    checkVarValues("number_codes(A,[0'-,0'2,0'5]).", "A", "-25");
    //[number_codes(A,[0'0,0'x,0'f]), [[A <-- 15]]].
    checkVarValues("number_codes(A,[0'0,0'x,0'f]).", "A", "15");
    //[number_codes(A,[0'0,39,0'a]), [[A <-- 97]]].
    checkVarValues("number_codes(A,[0'0,39,0'a]).", "A", "'0\\'a'");
    //[number_codes(A,[0'4,0'.,0'2]), [[A <-- 4.2]]].
    checkVarValues("number_codes(A,[0'4,0'.,0'2]).", "A", "4.2");
    //[number_codes(A,[0'4,0'2,0'.,0'0,0'e,0'-,0'1]), [[A <-- 4.2]]].
    checkVarValues("number_codes(A,[0'4,0'2,0'.,0'0,0'e,0'-,0'1]).", "A", "4.2");

    //[number_codes(A,[ 0'1, 0'2, 1000]), representation_error(character_code)]. % 1000 not a code
    assertProlException("number_codes(A,[ 0'1, 0'2, 100000]).", ProlRepresentationErrorException.class);

    //[number_codes(A,L), instantiation_error].
    assertProlException("number_codes(A,L).", ProlInstantiationErrorException.class);
    //[number_codes(a,L), type_error(number,a)].
    assertProlException("number_codes(a,L).", ProlTypeErrorException.class);
    //[number_codes(A,4), type_error(list,4)].
    assertProlException("number_codes(A,4).", ProlTypeErrorException.class);

    //[number_codes(33,L), [[L <-- [0'3,0'3]]]].
    checkVarValues("number_codes(33,L).", "L", "[" + (int) '3' + "," + (int) '3' + "]");
    //[number_codes(33,[0'3,0'3]), success].
    checkOnce("number_codes(33,[51,51]).", true);
    //[number_codes(33.0,L), [[L <-- [51,51,46,48]]]].
    checkVarValues("number_codes(33.0,L).", "L", "[51,51,46,48]");
    checkOnce("number_codes(33.0,[" + (int) '3' + ',' + (int) '.' + ',' + (int) '3' + ',' + (int) 'E' + ',' + (int) '0' + ',' + (int) '1' + "]).", true);
    checkVarValues("number_codes(A,[" + (int) '-' + ',' + (int) '2' + ',' + (int) '5' + "]).", "A", "-25");
    checkVarValues("number_codes(A,[" + (int) ' ' + ',' + (int) '3' + "]).", "A", "' 3'");
    checkVarValues("number_codes(A,[" + (int) '0' + ',' + (int) 'x' + ',' + (int) 'f' + "]).", "A", "15");
    checkVarValues("number_codes(A,[" + (int) '4' + ',' + (int) '.' + ',' + (int) '2' + "]).", "A", "4.2");
    //       checkOnceVar("number_codes(A,["+(int)'4'+','+(int)'2'+','+(int)'.'+','+(int)'0'+','+(int)'e'+','+(int)'-'+','+(int)'1'+"]).","A","4.2"); // prol returns 42.0e-1 because it is based on Java number output
  }

  @Test
  void testTermLtEq2() {
    //['@=<'(1.0,1), success].
    checkOnce("1.0@=<1.", true);
    //['@=<'(aardvark,zebra), success].
    checkOnce("aardvark@=<zebra.", true);
    //['@=<'(short,short), success].
    checkOnce("short@=<short.", true);
    //['@=<'(short,shorter), success].
    checkOnce("short@=<shorter.", true);
    //['@=<'(foo(b),foo(a)), failure].
    checkOnce("foo(b)@=<foo(a).", false);
    //['@=<'(X,X), success].
    checkOnce("X@=<X.", true);
    //['@=<'(foo(a,X),foo(b,Y)), success].
    checkOnce("foo(a,X)@=<foo(b,Y).", true);
  }

  @Test
  void testTermLt2() {
    //['@<'(1.0,1), success].
    checkOnce("1.0@<1.", false);// ???
    //['@<'(aardvark,zebra), success].
    checkOnce("aardvark@<zebra.", true);
    //['@<'(short,short), failure].
    checkOnce("short@<short.", false);
    //['@<'(short,shorter), success].
    checkOnce("short@<shorter.", true);
    //['@<'(foo(b),foo(a)), failure].
    checkOnce("foo(b)@<foo(a).", false);
    //['@<'(X,X), failure].
    checkOnce("X@<X.", false);
    //['@<'(foo(a,X),foo(b,Y)), success].
    checkOnce("foo(a,X)@<foo(b,Y).", true);
  }

  @Test
  void testTermGtEqu2() {
    //['@>='(1.0,1), failure].
    checkOnce("1.0@>=1.", true);// ???
    //['@>='(aardvark,zebra), failure].
    checkOnce("aardvark@>=zebra.", false);
    //['@>='(short,short), success].
    checkOnce("short@>=short.", true);
    //['@>='(short,shorter), failure].
    checkOnce("short@>=shorter.", false);
    //['@>='(foo(b),foo(a)), success].
    checkOnce("foo(b)@>=foo(a).", true);
    //['@>='(X,X), success].
    checkOnce("X@>=X.", true);
    //['@>='(foo(a,X),foo(b,Y)), failure].
    checkOnce("foo(a,X)@>=foo(b,Y).", false);
  }

  @Test
  void testTermGt2() {
    //['@>'(1.0,1), failure].
    checkOnce("1.0@>1.", false);
    //['@>'(aardvark,zebra), failure].
    checkOnce("aardvark@>zebra.", false);
    //['@>'(short,short), failure].
    checkOnce("short@>short.", false);
    //['@>'(short,shorter), failure].
    checkOnce("short@>shorter.", false);
    //['@>'(foo(b),foo(a)), success].
    checkOnce("foo(b)@>foo(a).", true);
    //['@>'(X,X), failure].
    checkOnce("X@>X.", false);
    //['@>'(foo(a,X),foo(b,Y)), failure].
    checkOnce("foo(a,X)@>foo(b,Y).", false);
  }

  @Test
  void testLtEqu2() {
    //['=<'(0,1), success].
    checkOnce("0=<1.", true);
    //['=<'(1.0,1), success].
    checkOnce("1.0=<1.", true);
    //['=<'(3*2,7-1), success].
    checkOnce("3*2=<7-1.", true);
    //['=<'(X,5), instantiation_error].
    assertProlException("X=<5.", ProlInstantiationErrorException.class);
    //['=<'(2 + floot(1),5), type_error(evaluable, floot/1)].
    assertProlException("2+floot(1)=<5.", ProlTypeErrorException.class);
  }

  @Test
  void testLt2() {
    //['<'(0,1), success].
    checkOnce("0<1.", true);
    //['<'(1.0,1), failure].
    checkOnce("1.0<1.", false);
    //['<'(3*2,7-1), failure].
    checkOnce("3*2<7-1.", false);
    //['<'(X,5), instantiation_error].
    assertProlException("X<5.", ProlInstantiationErrorException.class);
    //['<'(2 + floot(1),5), type_error(evaluable, floot/1)].
    assertProlException("2+floot(1)<5.", ProlTypeErrorException.class);
  }

  @Test
  void testGtEqu2() {
    //['>='(0,1), failure].
    checkOnce("0>=1.", false);
    //['>='(1.0,1), success].
    checkOnce("1.0>=1.", true);
    //['>='(3*2,7-1), success].
    checkOnce("3*2>=7-1.", true);
    //['>='(X,5), instantiation_error].
    assertProlException("X>=5.", ProlInstantiationErrorException.class);
    //['>='(2 + floot(1),5), type_error(evaluable, floot/1)].
    assertProlException("2+floot(1)>=5.", ProlTypeErrorException.class);
  }

  @Test
  void testGt2() {
    //['>'(0,1), failure].
    checkOnce("0>1.", false);
    //['>'(1.0,1), failure].
    checkOnce("1.0>1.", false);
    //['>'(3*2,7-1), failure].
    checkOnce("3*2>7-1.", false);
    //['>'(X,5), instantiation_error].
    assertProlException("X>5.", ProlInstantiationErrorException.class);
    //['>'(2 + floot(1),5), type_error(evaluable, floot/1)].
    assertProlException("2+floot(1)>5.", ProlTypeErrorException.class);
  }

  @Test
  void testArithEq2() {
    //['=:='(0,1), failure].
    checkOnce("0=:=1.", false);
    //['=:='(1.0,1), success].
    checkOnce("1.0=:=1.", true);
    //['=:='(3 * 2,7 - 1), success].
    checkOnce("3*2=:=7-1.", true);
    //['=:='(N,5), instantiation_error].
    assertProlException("N=:=5.", ProlInstantiationErrorException.class);
    //['=:='(floot(1),5), type_error(evaluable, floot/1)].
    assertProlException("floot(1)=:=5.", ProlTypeErrorException.class);
    //[0.333 =:= 1/3, failure].
    checkOnce("0.333=:=1/3.", false);
  }

  @Test
  void testArithDiff2() {
    //['=\\='(0,1), success].
    checkOnce("0=\\=1.", true);
    //['=\\='(1.0,1), failure].
    checkOnce("1.0=\\=1.", false);
    //['=\\='(3 * 2,7 - 1), failure].
    checkOnce("3*2=\\=7-1.", false);
    //['=\\='(N,5), instantiation_error].
    assertProlException("N=\\=5.", ProlInstantiationErrorException.class);
    //['=\\='(floot(1),5), type_error(evaluable, floot/1)].
    assertProlException("floot(1)=\\=5.", ProlTypeErrorException.class);
  }

  @Test
  void testCopyTerm2() {
    //[copy_term(X,Y), success].
    checkOnce("copy_term(X,Y).", true);
    //[copy_term(X,3), success].
    checkOnce("copy_term(X,3).", true);
    //[copy_term(_,a), success].
    checkOnce("copy_term(_,a).", true);
    //[copy_term(a+X,X+b),[[X <-- a]]].
    checkVarValues("copy_term(a+X,X+b).", "X", "'a'");
    //[copy_term(_,_), success].
    checkOnce("copy_term(_,_).", true);

    //[copy_term(a,a), success].
    checkOnce("copy_term(a,a).", true);

    //[copy_term(a,b), failure].
    checkOnce("copy_term(a,b).", false);

    //[copy_term(f(a),f(X)),[[X <-- a]]].
    checkVarValues("copy_term(f(a),f(X)).", "X", "'a'");

    //[(copy_term(a+X,X+b),copy_term(a+X,X+b)), failure].
    checkOnce("copy_term(a+X,X+b),copy_term(a+X,X+b).", false);

    //[copy_term(X+X+Y,A+B+B),[[B <-- A]]].
    checkVarValues("copy_term(X+X+Y,A+B+B), A = 666.", "B", "666");
  }

  @Test
  void testClause2() {
    //[clause(x,Body), failure].
    checkOnce("clause(x,Body).", false);
    //[clause(_,B), instantiation_error].
    assertProlException("clause(_,B).", ProlInstantiationErrorException.class);
    //[clause(4,B), type_error(callable,4)].
    assertProlException("clause(4,B).", ProlTypeErrorException.class);
    //[clause(f(_),5), type_error(callable,5)].
    assertProlException("clause(f(_),5).", ProlTypeErrorException.class);
    //[clause(atom(_),Body), permission_error(access,private_procedure,atom/1)].
    assertProlException("clause(atom(_),Body).", ProlPermissionErrorException.class);
  }

  @Test
  void testCatchAndThrow4() {
    //[(catch(true, C, write('something')), throw(blabla)), system_error].
    try {
      checkOnce("catch(true, C, write('something')), throw(blabla).", true);
      fail();
    } catch (ProlCustomErrorException ex) {
      assertEquals("blabla", ex.getErrorTerm().getText());
    }

    //[catch(number_chars(A,L), error(instantiation_error, _), fail), failure].
    checkOnce("catch(number_chars(A,L), error(instantiation_error, _), fail).", false);
  }

  @Test
  void testCall1() {
    assertProlException("call((write(3),X)).", ProlInstantiationErrorException.class);

    //[call(!),success].
    checkOnce("call(!).", true);
    //[call(fail), failure].
    checkOnce("call(fail).", false);
    //[call((fail, X)), failure].
    checkOnce("call((fail,X)).", false);
    //[call((fail, call(1))), failure].
    checkOnce("call((fail,call(1))).", false);

    //[call((write(3), X)), instantiation_error].
    assertProlException("call((write(3),X)).", ProlInstantiationErrorException.class);
    //[call((write(3), call(1))), type_error(callable,1)].
    assertProlException("call((write(3),call(1))).", ProlTypeErrorException.class);
    //[call((write(3), 1)), type_error(callable,(write(3), 1))].
    assertProlException("call((write(3),1)).", ProlTypeErrorException.class);
    //[call((1; true)), type_error(callable,(1; true))].
    assertProlException("call((1;true)).", ProlTypeErrorException.class);

    //[call(X), instantiation_error].
    assertProlException("call(X).", ProlInstantiationErrorException.class);
    //[call(1), type_error(callable,1)].
    assertProlException("call(1).", ProlTypeErrorException.class);

    //[call((fail, 1)), type_error(callable,(fail,1))].
    //assertProlException("call((fail,1)).", ProlTypeErrorException.class); // it doesn't throw error in jprol because fail,1 is valid callable construction and jprol doesn't make pre-check of arguments.

    assertProlException("call([fail]).", ProlTypeErrorException.class);
  }

  @Test
  void testSubAtom5() {
    //[sub_atom(abracadabra, 0, 5, _, S2), [[S2 <-- 'abrac']]].
    checkVarsAfterCall("sub_atom(abracadabra, 0, 5, _, S2).", new String[][] {new String[] {"S2"}, new String[] {"abrac"}});

    //[sub_atom(abracadabra, _, 5, 0, S2), [[S2 <-- 'dabra']]].
    checkVarsAfterCall("sub_atom(abracadabra, _, 5, 0, S2).", new String[][] {new String[] {"S2"}, new String[] {"dabra"}});

    //[sub_atom(abracadabra, 3, Length, 3, S2), [[Length <-- 5, S2 <-- 'acada']]].
    checkVarsAfterCall("sub_atom(abracadabra, 3, Length, 3, S2).",
        new String[][] {
            new String[] {"Length", "S2"}, new String[] {"5", "acada"}
        });

    //[sub_atom(abracadabra, Before, 2, After, ab),[[Before <-- 0, After <-- 9],[Before <-- 7, After <-- 2]]].
    checkVarsAfterCall("sub_atom(abracadabra, Before, 2, After, ab).", new String[][] {
        new String[] {"Before", "After"}, new String[] {"0", "9"},
        new String[] {"Before", "After"}, new String[] {"7", "2"}
    });

    //[sub_atom('Banana', 3, 2, _, S2), [[S2 <-- 'an']]].
    checkVarsAfterCall("sub_atom('Banana', 3, 2, _, S2).", new String[][] {
        new String[] {"S2"}, new String[] {"an"}
    });

    //[sub_atom('charity', _, 3, _, S2), [[S2 <-- 'cha'],[S2 <-- 'har'],[S2 <-- 'ari'],[S2 <-- 'rit'],[S2 <-- 'ity']]].
    checkVarsAfterCall("sub_atom('charity', _, 3, _, S2).", new String[][] {
        new String[] {"S2"}, new String[] {"cha"},
        new String[] {"S2"}, new String[] {"har"},
        new String[] {"S2"}, new String[] {"ari"},
        new String[] {"S2"}, new String[] {"rit"},
        new String[] {"S2"}, new String[] {"ity"}
    });

    //[sub_atom('ab', Before, Length, After, Sub_atom),[
    // [Before <-- 0, Length <-- 0, Sub_atom <-- ''],
    // [Before <-- 0, Length <-- 1, Sub_atom <-- 'a'],
    // [Before <-- 0, Length <-- 2, Sub_atom <-- 'ab'],
    // [Before <-- 1, Length <-- 0, Sub_atom <-- ''],
    // [Before <-- 1, Length <-- 1, Sub_atom <-- 'b'],
    // [Before <-- 2, Length <-- 0, Sub_atom <-- '']]].
    checkVarsAfterCall("sub_atom('ab', Before, Length, After, Sub_atom).", new String[][] {
        new String[] {"Before", "Length", "After", "Sub_atom"}, new String[] {"0", "0", "2", ""},
        new String[] {"Before", "Length", "After", "Sub_atom"}, new String[] {"0", "1", "1", "a"},
        new String[] {"Before", "Length", "After", "Sub_atom"}, new String[] {"0", "2", "0", "ab"},
        new String[] {"Before", "Length", "After", "Sub_atom"}, new String[] {"1", "0", "1", ""},
        new String[] {"Before", "Length", "After", "Sub_atom"}, new String[] {"1", "1", "0", "b"},
        new String[] {"Before", "Length", "After", "Sub_atom"}, new String[] {"2", "0", "0", ""},
    });

    //[sub_atom(Banana, 3, 2, _, S2), instantiation_error].
    assertProlException("sub_atom(Banana, 3, 2, _, S2).", ProlInstantiationErrorException.class);

    //[sub_atom(f(a), 2, 2, _, S2), type_error(atom,f(a))].
    assertProlException("sub_atom(f(a), 2, 2, _, S2).", ProlTypeErrorException.class);

    //[sub_atom('Banana', 4, 2, _, 2), type_error(atom,2)].
    assertProlException("sub_atom('Banana', 4, 2, _, 2).", ProlTypeErrorException.class);

    //[sub_atom('Banana', a, 2, _, S2), type_error(integer,a)].
    assertProlException("sub_atom('Banana', a, 2, _, S2).", ProlTypeErrorException.class);

    //[sub_atom('Banana', 4, n, _, S2), type_error(integer,n)].
    assertProlException("sub_atom('Banana', 4, n, _, S2).", ProlTypeErrorException.class);

    //[sub_atom('Banana', 4, _, m, S2), type_error(integer,m)].
    assertProlException("sub_atom('Banana', 4, _, m, S2).", ProlTypeErrorException.class);
  }

  @Test
  void testCurrentPredicateAndCurrentPredicateAll1() {
    //[current_predicate(current_predicate/1), failure].
    checkOnce("current_predicate(current_predicate/1).", false);

    final JProlChoicePoint goal = prepareGoal("some(). some(huzzaa).", "current_predicate(some/X).");
    assertNotNull(goal.prove());
    assertEquals(0L, getVarAsNumber(goal, "X"));
    assertNotNull(goal.prove());
    assertEquals(1L, getVarAsNumber(goal, "X"));
    assertNull(goal.prove());

    //[current_predicate(run_tests/1), success].
    checkOnce("current_predicate_all(atom/1).", true);
    checkVarValues("current_predicate_all(dispose/X).", "X", "0", "1");

    //[current_predicate(4), type_error(predicate_indicator, 4)].
    assertProlException("current_predicate(4).", ProlTypeErrorException.class);
    //[current_predicate(dog), type_error(predicate_indicator, dog)].
    assertProlException("current_predicate(dog).", ProlTypeErrorException.class);
    //[current_predicate(0/dog), type_error(predicate_indicator, 0/dog)].
    assertProlException("current_predicate(0/dog).", ProlTypeErrorException.class);

    checkOnce("current_predicate(current_predicate/3).", false);
  }

  @Test
  void testAtomConcat3() {
    //[atom_concat('hello',' world',A), [[A <-- 'hello world']]].
    checkVarValues("atom_concat('hello',' world',A).", "A", "'hello world'");
    //[atom_concat(T,' world','small world'), [[T <-- 'small']]].
    checkVarValues("atom_concat(T,' world','small world').", "T", "'small'");

    checkOnce("atom_concat('world',X,'small world').", false);
    checkVarValues("atom_concat('small ', X,'small world').", "X", "'world'");

    //[atom_concat('hello',' world','small world'), failure].
    checkOnce("atom_concat('hello',' world','small world').", false);
    checkOnce("atom_concat('small',' world','small world').", true);

    //[atom_concat(T1,T2,'hello'), [[T1 <-- '',T2 <-- 'hello'],[T1 <-- 'h',T2 <-- 'ello'],[T1 <-- 'he',T2 <-- 'llo'],[T1 <-- 'hel',T2 <-- 'lo'],[T1 <-- 'hell',T2 <-- 'o'],[T1 <-- 'hello',T2 <-- '']]].
    final JProlChoicePoint goal = prepareGoal("atom_concat(T1,T2,'hello').");
    assertNotNull(goal.prove());
    assertEquals("''", getVarAsText(goal, "T1"));
    assertEquals("'hello'", getVarAsText(goal, "T2"));
    assertNotNull(goal.prove());
    assertEquals("'h'", getVarAsText(goal, "T1"));
    assertEquals("'ello'", getVarAsText(goal, "T2"));
    assertNotNull(goal.prove());
    assertEquals("'he'", getVarAsText(goal, "T1"));
    assertEquals("'llo'", getVarAsText(goal, "T2"));
    assertNotNull(goal.prove());
    assertEquals("'hel'", getVarAsText(goal, "T1"));
    assertEquals("'lo'", getVarAsText(goal, "T2"));
    assertNotNull(goal.prove());
    assertEquals("'hell'", getVarAsText(goal, "T1"));
    assertEquals("'o'", getVarAsText(goal, "T2"));
    assertNotNull(goal.prove());
    assertEquals("'hello'", getVarAsText(goal, "T1"));
    assertEquals("''", getVarAsText(goal, "T2"));
    assertNull(goal.prove());

    final JProlChoicePoint goal2 = prepareGoal("atom_concat(T1,T2,'').");
    assertNotNull(goal2.prove());
    assertEquals("''", getVarAsText(goal2, "T1"));
    assertEquals("''", getVarAsText(goal2, "T2"));
    assertNull(goal2.prove());

    //[atom_concat(A1,'iso',A3), instantiation_error].
    assertProlException("atom_concat(A1,'iso',A3).", ProlInstantiationErrorException.class);

    //[atom_concat('iso',A2,A3), instantiation_error].
    assertProlException("atom_concat('iso',A2,A3).", ProlInstantiationErrorException.class);

    //[atom_concat(f(a),'iso',A3), type_error(atom,f(a))].
    assertProlException("atom_concat(f(a),'iso',A3).", ProlTypeErrorException.class);

    //[atom_concat('iso',f(a),A3), type_error(atom,f(a))].
    assertProlException("atom_concat('iso',f(a),A3).", ProlTypeErrorException.class);

    //[atom_concat(A1,A2,f(a)), type_error(atom,f(a))].
    assertProlException("atom_concat(A1,A2,f(a)).", ProlTypeErrorException.class);
  }

  @Test
  void testDispose1() {
    //todo improve test
    //[dispose, impl_defined].
//    checkException("dispose.");
    //[dispose(1), impl_defined].
//    checkException("dispose(1).");
    //[dispose(a), type_error(integer, a)].
    assertProlException("dispose(a).", ProlTypeErrorException.class);
  }

  @Test
  void testAtomCodes2() {
    //[atom_codes('',L), [[L <-- []]]].
    checkVarValues("atom_codes('',L).", "L", "[]");
    //[atom_codes([],L), [[L <-- [ 0'[, 0'] ]]]].
    checkVarValues("atom_codes([],L).", "L", "[" + (int) '[' + ',' + (int) ']' + "]");

    //[atom_codes('''',L), [[L <-- [ 39 ]]]].
    checkVarValues("atom_codes('\\'',L).", "L", "[39]");

    //[atom_codes('iso',L), [[L <-- [ 0'i, 0's, 0'o ]]]].
    checkVarValues("atom_codes('iso',L).", "L", "[" + (int) 'i' + ',' + (int) 's' + ',' + (int) 'o' + ']');

    //[atom_codes(A,[ 0'p, 0'r, 0'o, 0'l, 0'o, 0'g]), [[A <-- 'prolog']]].
    checkVarValues("atom_codes(A,[" + (int) 'p' + ',' + (int) 'r' + ',' + (int) 'o' + ',' + (int) 'l' + ',' + (int) 'o' + ',' + (int) 'g' + "]).", "A", "'prolog'");
    //[atom_codes('North',[0'N | L]), [[L <-- [0'o, 0'r, 0't, 0'h]]]].
    checkVarValues("atom_codes('North',[" + (int) 'N' + '|' + "L]).", "L", "[" + (int) 'o' + ',' + (int) 'r' + ',' + (int) 't' + ',' + (int) 'h' + ']');
    //[atom_codes('iso',[0'i, 0's]), failure].
    checkOnce("atom_codes('iso',[" + (int) 's' + ',' + (int) 'o' + "]).", false);

    //[atom_codes(A,L), instantiation_error].
    assertProlException("atom_codes(A,L).", ProlInstantiationErrorException.class);
    //[atom_codes(f(a),L), type_error(atom,f(a))].
    assertProlException("atom_codes(f(a),L).", ProlTypeErrorException.class);
    //[atom_codes(A, 0'x), type_error(list,0'x)].
    assertProlException("atom_codes(A," + (int) 'x' + ").", ProlTypeErrorException.class);

    //[atom_codes(A,[ 0'i, 0's, 1000]), representation_error(character_code)]. % 1000 not a code
    assertProlException("atom_codes(A,[" + (int) 'i' + ',' + (int) 's' + ",1.1]).", ProlRepresentationErrorException.class);
  }

  @Test
  void testCharCode2() {
    //[char_code(Char,0'c),[[Char <-- c]]].
    checkVarValues("char_code(Char,0'c).", "Char", "'c'");
    //[char_code(Char,163),[[Char <-- '\xa3\']]].
    checkVarValues("char_code(Char,163).", "Char", "'\u00A3'");
    //[char_code(a,Code),[[Code <-- 0'a]]].
    checkVarValues("char_code(a,Code).", "Code", (long) 'a');
    //[char_code(Char,99),[[Char <-- c]]].
    checkVarValues("char_code(Char,99).", "Char", "'c'");
    //[char_code(b,98),success].
    checkOnce("char_code(b,98).", true);
    //[char_code(b,4),failure].
    checkOnce("char_code(b,4).", false);
    //[char_code('ab',Code),type_error(character, 'ab')].
    assertProlException("char_code('ab',Code).", ProlTypeErrorException.class);
    //[char_code(a,x),type_error(integer, x)].
    assertProlException("char_code(a,x).", ProlTypeErrorException.class);
    //[char_code(Char,Code),instantiation_error].
    assertProlException("char_code(Char,Code).", ProlInstantiationErrorException.class);
    //[char_code(Char,-2),representation_error(character_code)].
    assertProlException("char_code(Char,-2).", ProlRepresentationErrorException.class);
  }

  @Test
  void testAtomChars2() {
    //[atom_chars('''',L), [[L <-- ['''']]]].
    checkVarValues("atom_chars('\\'',L).", "L", "['\\'']");

    //[atom_chars('',L), [[L <-- []]]].
    checkVarValues("atom_chars('',L).", "L", "[]");
    //[atom_chars([],L), [[L <-- ['[',']']]]].
    checkVarValues("atom_chars([],L).", "L", "['[',']']");
    //[atom_chars('iso',L), [[L <-- ['i','s','o']]]].
    checkVarValues("atom_chars('iso',L).", "L", "['i','s','o']");
    //[atom_chars(A,['p','r','o','l','o','g']), [[A <-- 'prolog']]].
    checkVarValues("atom_chars(A,['p','r','o','l','o','g']).", "A", "'prolog'");
    //[atom_chars('North',['N'|X]), [[X <-- ['o','r','t','h']]]].
    checkVarValues("atom_chars('North',['N'|X]).", "X", "['o','r','t','h']");

    //[atom_chars('iso',['i','s']), failure].
    checkOnce("atom_chars('iso',['i','s']).", false);

    //[atom_chars(A,L), instantiation_error].
    assertProlException("atom_chars(A,L).", ProlInstantiationErrorException.class);
    //[atom_chars(A,[a,E,c]), instantiation_error].
    assertProlException("atom_chars(A,[a,E,c]).", ProlInstantiationErrorException.class);
    //[atom_chars(A,[a,b|L]), instantiation_error].
    assertProlException("atom_chars(A,[a,b|L]).", ProlInstantiationErrorException.class);
    //[atom_chars(f(a),L), type_error(atom,f(a))].
    assertProlException("atom_chars(f(a),L).", ProlTypeErrorException.class);
    //[atom_chars(A,iso), type_error(list,iso)].
    assertProlException("atom_chars(A,iso).", ProlTypeErrorException.class);
    //[atom_chars(A,[a,f(b)]), type_error(character,f(b))].
    assertProlException("atom_chars(A,[a,f(b)]).", ProlTypeErrorException.class);
    //[(atom_chars(X,['1','2']), Y is X + 1), type_error(evaluable, '12'/0)].
    assertProlException("atom_chars(X,['1','2']),Y is X+1.", ProlTypeErrorException.class);
  }

  @Test
  void testFunctor3_Bis3() {
    //[functor(foo(a,b,c),foo,3),success].
    checkOnce("functor(foo(a,b,c),foo,3).", true);

    //[functor(foo(a,b,c),X,Y),[[X <-- foo, Y <-- 3]]].
    final JProlChoicePoint goal = proveGoal("functor(foo(a,b,c),X,Y).");
    assertEquals("'foo'", getVarAsText(goal, "X"));
    assertEquals(3L, getVarAsNumber(goal, "Y"));
    assertNull(goal.prove());

    //[functor(X,foo,3), [[X <-- foo(A,B,C)]]].
    checkVarValues("functor(X,foo,3).", "X", "foo(_,_,_)");

    //[functor(X,foo,0), [[X <-- foo]]].
    checkVarValues("functor(X,foo,0).", "X", "foo");

    //[functor(mats(A,B),A,B), [[A <-- mats,B <-- 2]]].
    final JProlChoicePoint goal2 = proveGoal("functor(mats(A,B),A,B).");
    assertEquals("'mats'", getVarAsText(goal2, "A"));
    assertEquals(2L, getVarAsNumber(goal2, "B"));
    assertNull(goal2.prove());

    //[functor(foo(a),foo,2), success]. % Must fail
    checkOnce("functor(foo(a),foo,2).", false);

    //[functor(foo(a),fo,1), failure].
    checkOnce("functor(foo(a),fo,1).", false);

    //[functor(1,X,Y), [[X <-- 1,Y <-- 0]]].
    final JProlChoicePoint goal3 = proveGoal("functor(1,X,Y).");
    assertEquals("1", getVarAsText(goal3, "X"), "1");
    assertEquals(0L, getVarAsNumber(goal3, "Y"));
    assertNull(goal3.prove());

    //[functor(X,1.1,0), [[X <-- 1.1]]].
    final JProlChoicePoint goal4 = proveGoal("functor(X,1.1,0).");
    assertEquals(1.1d, getVarAsNumber(goal4, "X"));
    assertNull(goal4.prove());

    //[functor([_|_],'.',2), failure]. % Must succeed
    checkOnce("functor([_|_],'.',2).", true);

    //[functor([],[],0), success].
    checkOnce("functor([],[],0).", true);

    //[functor(X, Y, 3), instantiation_error].
    assertProlException("functor(X, Y, 3).", ProlInstantiationErrorException.class);

    //[functor(X, foo, N), instantiation_error].
    assertProlException("functor(X, foo, N).", ProlInstantiationErrorException.class);

    //[functor(X, foo, a), type_error(integer,a)].
    assertProlException("functor(X, foo, a).", ProlTypeErrorException.class);

    //[functor(F, 1.5, 1), type_error(atom,1.5)].
    assertProlException("functor(F, 1.5, 1).", ProlTypeErrorException.class);

    //[functor(F,foo(a),1), type_error(atomic,foo(a))].
    assertProlException("functor(F,foo(a),1).", ProlTypeErrorException.class);

    //[(current_prolog_flag(max_arity,A),X is A + 1,functor(T, foo, X)),representation_error(max_arity)].
    assertProlException("current_prolog_flag(max_arity,A),X is A + 1,functor(T, foo, X).", ProlRepresentationErrorException.class);

    //[functor(T, foo, -1), domain_error(not_less_than_zero,-1)].
    assertProlException("functor(T, foo, -1).", ProlDomainErrorException.class);
  }

  @Test
  void testFunctor3() {
    //[(current_prolog_flag(max_arity,A), X is A + 1, functor(T, foo, X)),representation_error(max_arity)].

    //[functor(foo(a,b,c),foo,3), success].
    checkOnce("functor(foo(a,b,c),foo,3).", true);

    //[functor(foo(a,b,c),X,Y), [[X <-- foo, Y <-- 3]]].
    JProlChoicePoint goal = proveGoal("functor(foo(a,b,c),X,Y).");
    assertEquals(getVarAsText(goal, "X"), "'foo'");
    assertEquals(getVarAsNumber(goal, "Y"), 3L);
    assertNull(goal.prove());

    //[functor(X,foo,3), [[X <-- foo(A,B,C)]]].  % A, B and C are 3 new variables
    checkVarValues("functor(X,foo,3).", "X", "foo(_,_,_)");
    //[functor(X,foo,0), [[X <-- foo]]].
    checkVarValues("functor(X,foo,0).", "X", "foo");

    //[functor(mats(A,B),A,B), [[A <-- mats,B <-- 2]]].
    goal = proveGoal("functor(mats(A,B),A,B).");
    assertEquals(getVarAsText(goal, "A"), "'mats'");
    assertEquals(getVarAsNumber(goal, "B"), 2L);
    assertNull(goal.prove());

    //[functor(foo(a),foo,2), failure].
    checkOnce("functor(foo(a),foo,2).", false);
    //[functor(foo(a),fo,1), failure].
    checkOnce("functor(foo(a),fo,1).", false);

    //[functor(1,X,Y), [[X <-- 1,Y <-- 0]]].
    goal = proveGoal("functor(1,X,Y).");
    assertEquals(getVarAsNumber(goal, "X"), 1L);
    assertEquals(getVarAsNumber(goal, "Y"), 0L);
    assertNull(goal.prove());

    //[functor(X,1.1,0), [[X <-- 1.1]]].
    checkVarValues("functor(X,1.1,0).", "X", "1.1");
    //[functor([_|_],'.',2), success].
    checkOnce("functor([_|_],'.',2).", true);
    //[functor([],[],0), success].
    checkOnce("functor([],[],0).", true);

    //[functor(X, Y, 3), instantiation_error].
    assertProlException("functor(X,Y,3).", ProlInstantiationErrorException.class);
    //[functor(X, foo, N), instantiation_error].
    assertProlException("functor(X,foo,N).", ProlInstantiationErrorException.class);
    //[functor(X, foo, a), type_error(integer,a)].
    assertProlException("functor(X,foo,a).", ProlTypeErrorException.class);
    //[functor(F, 1.5, 1), type_error(atom,1.5)].
    assertProlException("functor(F,1.5,1).", ProlTypeErrorException.class);
    //[functor(F,foo(a),1), type_error(atomic,foo(a))].
    assertProlException("functor(F,foo(a),1).", ProlTypeErrorException.class);
    //[functor(T, foo, -1), domain_error(not_less_than_zero,-1)].
    assertProlException("functor(T,foo,-1).", ProlDomainErrorException.class);
  }

  @Test
  void testCutAndFail() {
    checkOnce("\\+((!,fail)).", true);
  }

  @Test
  void testNotProvable() {
    //[\+(true), failure].
    checkOnce("\\+(true).", false);
    //[\+(!), failure].
    checkOnce("\\+(!).", false);
    //[\+((!,fail)), success].
    checkOnce("\\+((!,fail)).", true);
    //[((X=1;X=2), \+((!,fail))), [[X <-- 1],[X <-- 2]]].
    checkVarValues("((X=1;X=2),\\+((!,fail))).", "X", 1L, 2L);
    //[\+(4 = 5), success].
    checkOnce("\\+(4=5).", true);
    //[\+(3), type_error(callable, 3)].
    assertProlException("\\+(3).", ProlTypeErrorException.class);
    //[\+(X), instantiation_error]. % Culprit X
    assertProlException("\\+(X).", ProlInstantiationErrorException.class);
  }

  @Test
  void testOnce1() {
    //[once(!), success].
    checkOnce("once(!).", true);
    //[(once(!), (X=1; X=2)), [[X <-- 1],[X <-- 2]]].
    checkVarValues("once(!),(X=1;X=2).", "X", 1L, 2L);
    //[once(repeat), success].
    checkOnce("once(repeat).", true);
    //[once(fail), failure].
    checkOnce("once(fail).", false);
    //[once(3), type_error(callable, 3)].
    assertProlException("once(3).", ProlTypeErrorException.class);
    //[once(X), instantiation_error]. % Culprit X
    assertProlException("once(X).", ProlInstantiationErrorException.class);
  }

  @Test
  void testArg3() {
    //[arg(1,foo(a,b),a), success].
    checkOnce("arg(1,foo(a,b),a).", true);
    //[arg(1,foo(X,b),a), [[X <-- a]]].
    checkVarValues("arg(1,foo(X,b),a).", "X", "'a'");
    //[arg(1,foo(a,b),X), [[X <-- a]]].
    checkVarValues("arg(1,foo(a,b),X).", "X", "'a'");

    //[arg(2,foo(a, f(X,b), c), f(a, Y)), [[X <-- a, Y <-- b]]].
    final JProlChoicePoint goal = proveGoal("arg(2,foo(a, f(X,b), c), f(a, Y)).");
    assertEquals(getVarAsText(goal, "X"), "'a'");
    assertEquals(getVarAsText(goal, "Y"), "'b'");
    assertNull(goal.prove());

    //[arg(1,foo(X,b),Y), [[Y <-- X]]].
    checkVarValues("arg(1,foo(X,b),Y).", "Y", "X");

    //[arg(1,foo(a,b),b), failure].
    checkOnce("arg(1,foo(a,b),b).", false);
    //[arg(0,foo(a,b),foo), failure].
    checkOnce("arg(0,foo(a,b),foo).", false);
    //[arg(3,foo(3,4),N), failure].
    checkOnce("arg(3,foo(3,4),N).", false);

    //[arg(X,foo(a,b),a), instantiation_error].
    assertProlException("arg(X,foo(a,b),a).", ProlInstantiationErrorException.class);
    //[arg(1,X,a), instantiation_error].
    assertProlException("arg(1,X,a).", ProlInstantiationErrorException.class);
    //[arg(0,atom,A), type_error(compound, atom)].
    assertProlException("arg(0,atom,a).", ProlTypeErrorException.class);
    //[arg(0,3,A), type_error(compound, 3)].
    assertProlException("arg(0,3,A).", ProlTypeErrorException.class);
    //[arg(-3,foo(a,b),A), domain_error(not_less_than_zero,-3)].
    assertProlException("arg(-3,foo(a,b),A).", ProlDomainErrorException.class);
    //[arg(a,foo(a,b),X), type_error(integer, a)].
    assertProlException("arg(a,foo(a,b),X).", ProlTypeErrorException.class);
  }

  @Test
  void testAtomLength2() {
    //[atom_length('enchanted evening', N), [[N <-- 17]]].
    checkVarValues("atom_length('enchanted evening', N).", "N", 17L);
    //[atom_length('', N), [[N <-- 0]]].
    checkVarValues("atom_length('', N).", "N", 0L);
    //[atom_length(Atom, 4), instantiation_error]. % Culprit Atom
    assertProlException("atom_length(Atom, 4).", ProlInstantiationErrorException.class);
    //[atom_length('scarlet', 5), failure].
    checkOnce("atom_length('scarlet',5).", false);
    //[atom_length(1.23, 4), type_error(atom, 1.23)].
    assertProlException("atom_length(1.23,4).", ProlTypeErrorException.class);
    //[atom_length(atom, '4'), type_error(integer, '4')].
    assertProlException("atom_length(atom,'4').", ProlTypeErrorException.class);
  }

  @Test
  void testFloat1() {
    //[float(3.3), success].
    checkOnce("float(3.3).", true);
    //[float(-3.3), success].
    checkOnce("float(-3.3).", true);
    //[float(3), failure].
    checkOnce("float(3).", false);
    //[float(atom), failure].
    checkOnce("float(atom).", false);
    //[float(X), failure].
    checkOnce("float(X).", false);
  }

  @Test
  void testUnify2() {
    //['='(1,1), success].
    checkOnce("'='(1,1).", true);
    //['='(X,1),[[X <-- 1]]].
    checkVarValues("'='(X,1).", "X", 1L);
    //['='(X,Y),[[Y <-- X]]].
    checkVarValues("Y=3,'='(X,Y).", "X", 3L);

    //[('='(X,Y),'='(X,abc)),[[X <-- abc, Y <-- abc]]].
    JProlChoicePoint goal = proveGoal("X=Y,'='(X,abc).");
    assertEquals(getVarAsText(goal, "X"), "'abc'");
    assertEquals(getVarAsText(goal, "Y"), "'abc'");
    assertNull(goal.prove());

    checkVarValues("X=Y,'='(X,abc).", "Y", "'abc'");

    //['='(f(X,def),f(def,Y)), [[X <-- def, Y <-- def]]].
    goal = proveGoal("'='(f(X,def),f(def,Y)).");
    assertEquals(getVarAsText(goal, "X"), "'def'");
    assertEquals(getVarAsText(goal, "Y"), "'def'");
    assertNull(goal.prove());

    //['='(1,2), failure].
    checkOnce("'='(1,2).", false);
    //['='(1,1.0), failure].
    checkOnce("'='(1,1.000001).", false);
    //['='(g(X),f(f(X))), failure].
    checkOnce("'='(g(X),f(f(X))).", false);
    checkOnce("'='(g(X),f(f(X))).", false);
    //['='(f(X,1),f(a(X))), failure].
    checkOnce("'='(f(X,1),f(a(X))).", false);
    //['='(f(X,Y,X),f(a(X),a(Y),Y,2)), failure].
    checkOnce("'='(f(X,Y,X),f(a(X),a(Y),Y,2)).", false);

    checkOnce("'='(f(X,Y,X),f(a(X),a(Y),Y,2)).", false);

    //['='(f(A,B,C),f(g(B,B),g(C,C),g(D,D))),[[A <-- g(g(g(D,D),g(D,D)),g(g(D,D),g(D,D))),B <-- g(g(D,D),g(D,D)),C <-- g(D,D)]]].
    goal = proveGoal("'='(f(A,B,C),f(g(B,B),g(C,C),g(D,D))).");
    assertEquals("g(D,D)", getVarAsText(goal, "C"));
    assertEquals("g(g(D,D),g(D,D))", getVarAsText(goal, "B"));
    assertEquals("g(g(g(D,D),g(D,D)),g(g(D,D),g(D,D)))", getVarAsText(goal, "A"));
    assertNull(goal.prove());
  }

  @Test
  void testNonVar1() {
    //[nonvar(33.3), success].
    checkOnce("nonvar(33.3).", true);
    //[nonvar(foo), success].
    checkOnce("nonvar(false).", true);
    //[nonvar(Foo), failure].
    checkOnce("nonvar(Foo).", false);
    //[(foo=Foo,nonvar(Foo)),[[Foo <-- foo]]].
    checkVarValues("foo=Foo,nonvar(Foo).", "Foo", "'foo'");
    //[nonvar(_), failure].
    checkOnce("nonvar(_).", false);
    //[nonvar(a(b)), success].
    checkOnce("nonvar(a(b)).", true);
  }

  @Test
  void testCompound1() {
    //[compound(33.3), failure].
    checkOnce("compound(33.3).", false);
    //[compound(-33.3), failure].
    checkOnce("compound(-33.3).", false);
    //[compound(-a), success].
    checkOnce("compound(-a).", true);
    //[compound(_), failure].
    checkOnce("compound(_).", false);
    //[compound(a), failure].
    checkOnce("compound(a).", false);
    //[compound(a(b)), success].
    checkOnce("compound(a(b)).", true);

    checkOnce("compound([]).", false);

    //[compound([a]),success].
    checkOnce("compound([a]).", true);
  }

  @Test
  void testAtomic1() {
    //[atomic(atom), success].
    checkOnce("atomic(atom).", true);
    //[atomic(a(b)), failure].
    checkOnce("atomic(a(b)).", false);
    //[atomic(TermVar), failure].
    checkOnce("atomic(TermVar).", false);
    //[atomic([]), success].
    checkOnce("atomic([]).", true);
    //[atomic(6), success].
    checkOnce("atomic(6).", true);
    //[atomic(3.3), success].
    checkOnce("atomic(3.3).", true);
  }

  @Test
  void testAtom1() {
    //[atom(atom), success].
    checkOnce("atom(atom).", true);
    //[atom('string'), success].
    checkOnce("atom('string').", true);
    //[atom(a(b)), failure].
    checkOnce("atom(a(b)).", false);
    //[atom(TermVar), failure].
    checkOnce("atom(TermVar).", false);
    //[atom([]), success].
    checkOnce("atom([]).", true);
    //[atom(6), failure].
    checkOnce("atom(6).", false);
    //[atom(3.3), failure].
    checkOnce("atom(3.3).", false);
  }

  @Test
  void testInteger1() {
    //[integer(3), success].
    checkOnce("integer(3).", true);
    //[integer(-3), success].
    checkOnce("integer(-3).", true);
    //[integer(3.3), failure].
    checkOnce("integer(3.3).", false);
    //[integer(X), failure].
    checkOnce("integer(X).", false);
    //[integer(atom), failure].
    checkOnce("integer(atom).", false);
  }

  @Test
  void testNumber1() {
    //[number(3), success].
    checkOnce("number(3).", true);
    //[number(3.3), success].
    checkOnce("number(3.3).", true);
    //[number(-3), success].
    checkOnce("number(-3).", true);
    //[number(a), failure].
    checkOnce("number(a).", false);
    //[number(X), failure].
    checkOnce("number(X).", false);
  }

  @Test
  void testNumberChars2() {
    checkVarValues("number_chars(A,['0','\\'','A']).", "A", 65);

    //[number_chars(33,['3','3']), success].
    checkOnce("number_chars(33,['3','3']).", true);

    //[number_chars(33,L), [[L <-- ['3','3']]]].
    checkVarValues("number_chars(33,L).", "L", "['3','3']");

    //[number_chars(33.0,L), [[L <-- ['3','3','.','0']]]].
    checkVarValues("number_chars(33.0,L).", "L", "['3','3','.','0']");
    //[number_chars(X,['3','.','3','E','+','0']), [[X <-- 3.3]]].
    checkVarValues("number_chars(X,['3','.','3','E','+','0']).", "X", 3.3d);
    //[number_chars(3.3,['3','.','3','E','+','0']), success].
    checkOnce("number_chars(3.3,['3','.','3','E','+','0']).", true);
    //[number_chars(A,['-','2','5']), [[A <-- (-25)]]].
    checkVarValues("number_chars(A,['-','2','5']).", "A", -25L);
    //[number_chars(A,['\n',' ','3']), [[A <-- 3]]].
    checkVarValues("number_chars(A,['\\n',' ','3']).", "A", 3L);

    //[number_chars(A,['3',' ']), syntax_error(_)].
    assertProlException("number_chars(A,['3',' ']).", ProlCustomErrorException.class);

    //[number_chars(A,['0','''','A']), [[A <-- 65]]].
    checkVarValues("number_chars(A,['0','\\'','A']).", "A", 65);

    //[number_chars(a,L), type_error(number, a)].
    assertProlException("number_chars(a,L).", ProlTypeErrorException.class);
    //[number_chars(A,4), type_error(list, 4)].
    assertProlException("number_chars(A,4).", ProlTypeErrorException.class);
    //[number_chars(A,['4',2]), type_error(character, 2)].
    assertProlException("number_chars(A,['4',2]).", ProlTypeErrorException.class);

    //[number_chars(A,['4','.','2']), [[A <-- 4.2]]].
    checkVarValues("number_chars(A,['4','.','2']).", "A", 4.2d);
    //[number_chars(A,['4','2','.','0','e','-','1']), [[A <-- 4.2]]].
    checkVarValues("number_chars(A,['4','2','.','0','e','-','1']).", "A", 4.2d);
    //[number_chars(A,['0',x,f]), [[A <-- 15]]].
    checkVarValues("number_chars(A,['0',x,f]).", "A", 15L);

    //[number_chars(A,L), instantiation_error].
    assertProlException("number_chars(A,L).", ProlInstantiationErrorException.class);

  }

}