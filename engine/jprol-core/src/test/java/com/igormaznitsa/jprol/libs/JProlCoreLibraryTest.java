package com.igormaznitsa.jprol.libs;

import com.igormaznitsa.jprol.exceptions.ProlCustomErrorException;
import com.igormaznitsa.jprol.it.AbstractJProlTest;
import com.igormaznitsa.jprol.logic.ChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JProlCoreLibraryTest extends AbstractJProlTest {

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
    checkOnce("for(X,4,1).", false);
    checkOnce("for(5,1,4).", false);
    checkException("for(5,X,4).");
    checkException("for(5,1,X).");
    checkException("for(5,A,X).");
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
    checkVarValues("Term=..[baz, foo(1)].", "Term", "baz(foo(1))");
    checkException("a()=..L.");
  }

  @Test
  void testXor1() {
    checkVarValues("X is xor(123,334).", "X", String.valueOf(123 ^ 334));
    checkVarValues("X is xor(123,xor(334,4452234)).", "X", String.valueOf(123 ^ 334 ^ 4452234));
    checkException("X is xor(123,xor(334.24,4452234.21)).");
  }

  @Test
  void testBitwiseNot1() {
    checkVarValues("X is \\ 1.", "X", String.valueOf(~1));
    checkException("X is \\ 1.445.");
  }

  @Test
  void testBitwiseOr2() {
    checkVarValues("X is 3434 \\/ 2234123.", "X", String.valueOf(3434 | 2234123));
    checkException("X is 3434.3312 \\/ 2234123.");
    checkException("X is 3434 \\/ 2234123.332.");
    checkException("X is 3434.123 \\/ 2234123.332.");
  }

  @Test
  void testBitwiseAnd2() {
    checkVarValues("X is 3434 /\\ 2234123.", "X", String.valueOf(3434 & 2234123));
    checkException("X is 3434.3312 /\\ 2234123.");
    checkException("X is 3434 /\\ 2234123.332.");
    checkException("X is 3434.123 /\\ 2234123.332.");
  }

  @Test
  void testMod2() {
    checkVarValues("X is mod(16,8).", "X", String.valueOf(0));
    checkVarValues("X is mod(344,123).", "X", String.valueOf(344 % 123));
    checkException("X is mod(344.456,123.11).");
    checkException("X is mod(344,0).");
  }

  @Test
  void testRem2() {
    checkVarValues("X is rem(16,8).", "X", String.valueOf(0));
    checkVarValues("X is rem(344,123).", "X", String.valueOf(98));
    checkException("X is rem(344.456,123.11).");
    checkException("X is rem(344,0).");
    checkException("X is rem('a',12).");
    checkException("X is rem(A,12).");
    checkException("X is rem(12,B).");
    checkException("X is rem(0,'b').");
  }

  @Test
  void testPow2() {
    checkVarValues("X is 16**8.", "X", String.valueOf(Math.pow(16, 8)));
    checkVarValues("X is 16**0.", "X", String.valueOf(1.0));
    checkVarValues("X is 16**-2.", "X", String.valueOf(Math.pow(16, -2)));
    checkException("X is 'a'**-2.");
    checkException("X is A**-2.");
    checkException("X is 16**-'b'.");
    checkException("X is 2**-'b'.");
    checkException("X is A**-B.");
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
    checkVarValues("X is abs(0).", "X", String.valueOf(Math.abs(0)));
    checkVarValues("X is abs(-33.2).", "X", String.valueOf(Math.abs(-33.2D)));
  }

  @Test
  void testOp3() {
    checkOnce("op(334, xfx, some).", true);
    checkOnce("op(444, xf, [one,two,three]).", true);
    checkException("op(443, xfx, '(').");
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
    checkOnce("assert(test(1)),test(X),X==1,abolish(test/1),\\+ test(_).", true);

    //[abolish(abolish/1), permission_error(modify,static_procedure,abolish/1)].
    checkException("abolish(abolish/1).");

    //[abolish(foo/a), type_error(integer,a)].
    checkException("abolish(foo/a).");

    //[abolish(foo/(-1)), domain_error(not_less_than_zero,-1)].
    //checkException("abolish(foo/(-1))."); // prol doesn't check the right part

    //[abolish(5/2), type_error(atom,5)].
    //checkException("abolish(5/2)."); // prol just trying to remove record at vocabulary for such signature
  }

  @Test
  void testRetract() throws Exception {
    //[retract((4 :- X)), type_error(callable, 4)].
    checkException("retract((4:-X)).");
    //[retract((atom(_) :- X == '[]')),permission_error(modify,static_procedure,atom/1)].
    checkException("retract((atom(_):-X=='[]')).");
  }

  @Test
  void testFail() throws Exception {
    //[undef_pred, existence_error(procedure, undef_pred/0)]. % the value of flag 'unknown' is 'error'.
    //[(set_prolog_flag(unknown, fail), undef_pred), failure].
    //[(set_prolog_flag(unknown, warning), undef_pred), failure].

    //[fail, failure].
    checkOnce("fail.", false);
  }

  @Test
  void testFindAll() throws Exception {

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
    final ChoicePoint goal = proveGoal("findall(X,(X=1;X=2),[X1,Y1])."); // changed from original because at Prol all variables linked by their names inside a goal, so that it is not a bug, it is a feature
    assertEquals("1", goal.getVarAsText("X1"));
    assertEquals("2", goal.getVarAsText("Y1"));
    assertNull(goal.next());

    //[findall(X,Goal,S),instantiation_error]. % Culprit Goal
    checkException("findall(X,Goal,S).");
    //[findall(X,4,S),type_error(callable, 4)].
    checkException("findall(X,4,S).");
    //[findall(X,call(1),S),type_error(callable, 1)].
    checkException("findall(X,call(1),S).");
  }

  @Test
  void testSetOf() throws Exception {
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
    final ChoicePoint goal1 = prepareGoal("setof(1,(Y=2;Y=1),L).");
    assertNotNull(goal1.next());
    assertEquals("[1]", goal1.getVarAsText("L"));
    assertEquals("2", goal1.getVarAsText("Y"));
    assertNotNull(goal1.next());
    assertEquals("[1]", goal1.getVarAsText("L"));
    assertEquals("1", goal1.getVarAsText("Y"));
    assertNull(goal1.next());

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
    final ChoicePoint goal2 = prepareGoal("setof(X,(X=Y;X=Z;Y=1),L).");
    assertNotNull(goal2.next());
    assertEquals("[Y,Z]", goal2.getVarAsText("L"));
    assertNull(goal2.getVarAsText("Y"));
    assertNotNull(goal2.next());
    assertEquals("[X]", goal2.getVarAsText("L"));
    assertEquals("1", goal2.getVarAsText("Y"));
    assertNull(goal2.next());
    //[setof(X, X^(true; 4),L), type_error(callable,(true;4))].
    checkException("setof(X, X^(true; 4),L).");
    //[setof(X,1,L), type_error(callable,1)].
    checkException("setof(X,1,L).");
  }

  @Test
  void testBagOf() throws Exception {
    //[bagof(X, (X = 1; X = 2),L), [[L<-- [1, 2]]]].
    checkVarValues("bagof(X,(X=1;X=2),L).", "L", "[1,2]");
    //[bagof(X, (X = 1; X = 2),X), [[X<-- [1, 2]]]].
    checkVarValues("bagof(X,(X=1;X=2),X).", "X", "[1,2]");

    //[bagof(X, (X = Y; X = Z),L), [[L<-- [Y, Z]]]].
    checkVarValues("bagof(X,(X=Y;X=Z),L).", "L", "[Y,Z]");

    //[bagof(1, (Y = 1; Y = 2),L), [[L<-- [1], Y<-- 1], [L<-- [1], Y<-- 2]]].
    final ChoicePoint goal = prepareGoal("bagof(1,(Y = 1; Y = 2),L).");
    assertNotNull(goal.next());
    assertEquals("[1]", goal.getVarAsText("L"));
    assertEquals("1", goal.getVarAsText("Y"));
    assertNotNull(goal.next());
    assertEquals("[1]", goal.getVarAsText("L"));
    assertEquals("2", goal.getVarAsText("Y"));
    assertNull(goal.next());

    //[bagof(X, fail, L), failure].
    checkOnce("bagof(X, fail, L).", false);

    //[bagof(X, (X = Y; X = Z; Y = 1),L), [[L<-- [Y, Z]], [L<-- [_], Y<-- 1]]].
    final ChoicePoint goal2 = prepareGoal("bagof(X, (X = Y; X = Z; Y = 1),L).");
    assertNotNull(goal2.next());
    assertEquals("[Y,Z]", goal2.getVarAsText("L"));
    assertNotNull(goal2.next());
    assertEquals("[X]", goal2.getVarAsText("L"));
    assertEquals("1", goal2.getVarAsText("Y"));
    assertNull(goal2.next());

    //[(set_prolog_flag(unknown, warning),bagof(X, (Y ^ (X = 1;Y = 1);X = 3),S)), [[S<-- [3]]]].
    final ChoicePoint goal3 = prepareGoal("bagof(X, (Y ^ (X = 1;Y = 1);X = 3),S).");
    assertNotNull(goal3.next());
    assertEquals("[3]", goal3.getVarAsText("S"));
    assertNull(goal3.next());

    //[bagof(X, Y ^ ((X = 1; Y = 1);(X = 2,Y = 2)),S), [[S<-- [1, _, 2]]]].
    final ChoicePoint goal4 = prepareGoal("bagof(X, Y ^ ((X = 1; Y = 1);(X = 2,Y = 2)),S).");
    assertNotNull(goal4.next());
    assertEquals("[1,2,X]", goal4.getVarAsText("S"));
    assertNull(goal4.next());

    //[bagof(X, Y ^ ((X = 1, Y = 1);(X = 2,Y = 2)),S), [[S<-- [1, 2]]]].
    final ChoicePoint goal5 = prepareGoal("bagof(X, Y ^ ((X = 1, Y = 1);(X = 2,Y = 2)),S).");
    assertNotNull(goal5.next());
    assertEquals("[1,2]", goal5.getVarAsText("S"));
    assertNull(goal5.next());

    //[bagof(f(X, Y), (X = a; Y = b),L), [[L<-- [f(a, _), f(_, b)]]]].
    final ChoicePoint goal6 = prepareGoal("bagof(f(X, Y), (X = a; Y = b),L).");
    assertNotNull(goal6.next());
    assertEquals("[f('a',Y),f(X,'b')]", goal6.getVarAsText("L"));
    assertNull(goal6.next());

    //[bagof(X, Y ^ Z, L), instantiation_error].
    checkException("bagof(X, Y ^ Z, L).");

    //[bagof(X, 1, L), type_error(callable, 1)].
    checkException("bagof(X, 1, L).");
  }

  @Test
  void testAssertZ() throws Exception {
    //[assertz((foo(X) :- X -> call(X))), success].
    checkOnce("assertz((foo(X):-X->call(X))).", true);
    //[assertz(_), instantiation_error].
    checkException("assertz(_).");
    //[assertz(4), type_error(callable, 4)].
    checkException("assertz(4).");

    //[assertz((foo :- 4)), type_error(callable, 4)].
    //checkException("assertz((foo:-4)).");// prol doesn't check the term fully

    //[assertz((atom(_) :- true)), permission_error(modify,static_procedure,atom/1)].
    checkException("assertz((atom(_):-true)).");
  }

  @Test
  void testAssertA() throws Exception {
    //[(asserta((bar(X) :- X)), clause(bar(X), B)), [[B <-- call(X)]]].
    checkVarValues("asserta(bar(X):-call(X)),clause(bar(X),B).", "B", "call(X)");

    //[asserta(_), instantiation_error].
    checkException("asserta(_).");

    //[asserta(4), type_error(callable, 4)].
    checkException("asserta(4).");

    //[asserta((foo :- 4)), type_error(callable, 4)].
    //checkException("asserta((foo:-4))."); //!!jprol allows

    //[asserta((atom(_) :- true)), permission_error(modify,static_procedure,atom/1)].
    checkException("asserta((atom(_):-true)).");
  }

  @Test
  void testNumberCodes() throws Exception {
    //[number_codes(33.0,[0'3,0'.,0'3,0'E,0'+,0'0,0'1]), success].
    //[number_codes(A,[0'-,0'2,0'5]), [[A <-- (-25)]]].
    //[number_codes(A,[0' ,0'3]), [[A <-- 3]]].
    //[number_codes(A,[0'0,0'x,0'f]), [[A <-- 15]]].
    //[number_codes(A,[0'0,39,0'a]), [[A <-- 97]]].
    //[number_codes(A,[0'4,0'.,0'2]), [[A <-- 4.2]]].
    //[number_codes(A,[0'4,0'2,0'.,0'0,0'e,0'-,0'1]), [[A <-- 4.2]]].

    //[number_codes(A,[ 0'1, 0'2, 1000]), representation_error(character_code)]. % 1000 not a code

    //[number_codes(A,L), instantiation_error].
    checkException("number_codes(A,L).");
    //[number_codes(a,L), type_error(number,a)].
    checkException("number_codes(a,L).");
    //[number_codes(A,4), type_error(list,4)].
    checkException("number_codes(A,4).");

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
  void testTermLtEq() throws Exception {
    //['@=<'(1.0,1), success].
    checkOnce("'@=<'(1.0,1).", true);
    //['@=<'(aardvark,zebra), success].
    checkOnce("'@=<'(aardvark,zebra).", true);
    //['@=<'(short,short), success].
    checkOnce("'@=<'(short,short).", true);
    //['@=<'(short,shorter), success].
    checkOnce("'@=<'(short,shorter).", true);
    //['@=<'(foo(b),foo(a)), failure].
    checkOnce("'@=<'(foo(b),foo(a)).", false);
    //['@=<'(X,X), success].
    checkOnce("'@=<'(X,X).", true);
    //['@=<'(foo(a,X),foo(b,Y)), success].
    checkOnce("'@=<'(foo(a,X),foo(b,Y)).", true);
  }

  @Test
  void testTermLt() throws Exception {
    //['@<'(1.0,1), success].
    checkOnce("'@<'(1.0,1).", false);// ???
    //['@<'(aardvark,zebra), success].
    checkOnce("'@<'(aardvark,zebra).", true);
    //['@<'(short,short), failure].
    checkOnce("'@<'(short,short).", false);
    //['@<'(short,shorter), success].
    checkOnce("'@<'(short,shorter).", true);
    //['@<'(foo(b),foo(a)), failure].
    checkOnce("'@<'(foo(b),foo(a)).", false);
    //['@<'(X,X), failure].
    checkOnce("'@<'(X,X).", false);
    //['@<'(foo(a,X),foo(b,Y)), success].
    checkOnce("'@<'(foo(a,X),foo(b,Y)).", true);
  }

  @Test
  void testTermGtEqu() throws Exception {
    //['@>='(1.0,1), failure].
    checkOnce("'@>='(1.0,1).", true);// ???
    //['@>='(aardvark,zebra), failure].
    checkOnce("'@>='(aardvark,zebra).", false);
    //['@>='(short,short), success].
    checkOnce("'@>='(short,short).", true);
    //['@>='(short,shorter), failure].
    checkOnce("'@>='(short,shorter).", false);
    //['@>='(foo(b),foo(a)), success].
    checkOnce("'@>='(foo(b),foo(a)).", true);
    //['@>='(X,X), success].
    checkOnce("'@>='(X,X).", true);
    //['@>='(foo(a,X),foo(b,Y)), failure].
    checkOnce("'@>='(foo(a,X),foo(b,Y)).", false);
  }

  @Test
  void testTermGt() throws Exception {
    //['@>'(1.0,1), failure].
    checkOnce("'@>'(1.0,1).", false);
    //['@>'(aardvark,zebra), failure].
    checkOnce("'@>'(aardvark,zebra).", false);
    //['@>'(short,short), failure].
    checkOnce("'@>'(short,short).", false);
    //['@>'(short,shorter), failure].
    checkOnce("'@>'(short,shorter).", false);
    //['@>'(foo(b),foo(a)), success].
    checkOnce("'@>'(foo(b),foo(a)).", true);
    //['@>'(X,X), failure].
    checkOnce("'@>'(X,X).", false);
    //['@>'(foo(a,X),foo(b,Y)), failure].
    checkOnce("'@>'(foo(a,X),foo(b,Y)).", false);
  }

  @Test
  void testLtEqu() throws Exception {
    //['=<'(0,1), success].
    checkOnce("'=<'(0,1).", true);
    //['=<'(1.0,1), success].
    checkOnce("'=<'(1.0,1).", true);
    //['=<'(3*2,7-1), success].
    checkOnce("'=<'(3*2,7-1).", true);
    //['=<'(X,5), instantiation_error].
    checkException("'=<'(X,5).");
    //['=<'(2 + floot(1),5), type_error(evaluable, floot/1)].
    checkException("'=<'(2+floot(1),5).");
  }

  @Test
  void testLt() throws Exception {
    //['<'(0,1), success].
    checkOnce("'<'(0,1).", true);
    //['<'(1.0,1), failure].
    checkOnce("'<'(1.0,1).", false);
    //['<'(3*2,7-1), failure].
    checkOnce("'<'(3*2,7-1).", false);
    //['<'(X,5), instantiation_error].
    checkException("'<'(X,5).");
    //['<'(2 + floot(1),5), type_error(evaluable, floot/1)].
    checkException("'<'(2+floot(1),5).");
  }

  @Test
  void testGtEqu() throws Exception {
    //['>='(0,1), failure].
    checkOnce("'>='(0,1).", false);
    //['>='(1.0,1), success].
    checkOnce("'>='(1.0,1).", true);
    //['>='(3*2,7-1), success].
    checkOnce("'>='(3*2,7-1).", true);
    //['>='(X,5), instantiation_error].
    checkException("'>='(X,5).");
    //['>='(2 + floot(1),5), type_error(evaluable, floot/1)].
    checkException("'>='(2+floot(1),5).");
  }

  @Test
  void testGt() throws Exception {
    //['>'(0,1), failure].
    checkOnce("'>'(0,1).", false);
    //['>'(1.0,1), failure].
    checkOnce("'>'(1.0,1).", false);
    //['>'(3*2,7-1), failure].
    checkOnce("'>'(3*2,7-1).", false);
    //['>'(X,5), instantiation_error].
    checkException("'>'(X,5).");
    //['>'(2 + floot(1),5), type_error(evaluable, floot/1)].
    checkException("'>'(2+floot(1),5).");
  }

  @Test
  void testArithEq() throws Exception {
    //['=:='(0,1), failure].
    checkOnce("'=:='(0,1).", false);
    //['=:='(1.0,1), success].
    checkOnce("'=:='(1.0,1).", true);
    //['=:='(3 * 2,7 - 1), success].
    checkOnce("'=:='(3*2,7-1).", true);
    //['=:='(N,5), instantiation_error].
    checkException("'=:='(N,5).");
    //['=:='(floot(1),5), type_error(evaluable, floot/1)].
    checkException("'=:='(floot(1),5).");
    //[0.333 =:= 1/3, failure].
    checkOnce("0.333=:=1/3.", false);
  }

  @Test
  void testArithDiff() throws Exception {
    //['=\\='(0,1), success].
    checkOnce("'=\\\\='(0,1).", true);
    //['=\\='(1.0,1), failure].
    checkOnce("'=\\\\='(1.0,1).", false);
    //['=\\='(3 * 2,7 - 1), failure].
    checkOnce("'=\\\\='(3*2,7-1).", false);
    //['=\\='(N,5), instantiation_error].
    checkException("'=\\\\='(N,5).");
    //['=\\='(floot(1),5), type_error(evaluable, floot/1)].
    checkException("'=\\\\='(floot(1),5).");
  }

  @Test
  void testCopyTerm() throws Exception {
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
    //checkOnceVar("copy_term(X+X+Y,A+B+B).","B","A"); // prol links variables as objects so it is no so easy to say which variable is parent
  }

  @Test
  void testClause() throws Exception {
    //[clause(x,Body), failure].
    checkOnce("clause(x,Body).", false);
    //[clause(_,B), instantiation_error].
    checkException("clause(_,B).");
    //[clause(4,B), type_error(callable,4)].
    checkException("clause(4,B).");
    //[clause(f(_),5), type_error(callable,5)].
    checkException("clause(f(_),5).");
    //[clause(atom(_),Body), permission_error(access,private_procedure,atom/1)].
    checkException("clause(atom(_),Body).");
  }

  @Test
  void testCatchAndThrow() throws Exception {
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
  void testCall() throws Exception {
    //[call(!),success].
    checkOnce("call(!).", true);
    //[call(fail), failure].
    checkOnce("call(fail).", false);
    //[call((fail, X)), failure].
    checkOnce("call((fail,X)).", false);
    //[call((fail, call(1))), failure].
    checkOnce("call((fail,call(1))).", false);

    //[call((write(3), X)), instantiation_error].
    checkException("call((write(3),X)).");
    //[call((write(3), call(1))), type_error(callable,1)].
    checkException("call((write(3),call(1))).");
    //[call((write(3), 1)), type_error(callable,(write(3), 1))].
    checkException("call((write(3),1)).");
    //[call((1; true)), type_error(callable,(1; true))].
    checkException("call((1;true)).");

    //[call(X), instantiation_error].
    checkException("call(X).");
    //[call(1), type_error(callable,1)].
    checkException("call(1).");

    //[call((fail, 1)), type_error(callable,(fail,1))].
    //checkException("call((fail,1))."); // it is not working at proll because prol checks sequentially and fail will be the first one

    checkException("call([fail]).");
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
    checkException("sub_atom(Banana, 3, 2, _, S2).");

    //[sub_atom(f(a), 2, 2, _, S2), type_error(atom,f(a))].
    checkException("sub_atom(f(a), 2, 2, _, S2).");

    //[sub_atom('Banana', 4, 2, _, 2), type_error(atom,2)].
    checkException("sub_atom('Banana', 4, 2, _, 2).");

    //[sub_atom('Banana', a, 2, _, S2), type_error(integer,a)].
    checkException("sub_atom('Banana', a, 2, _, S2).");

    //[sub_atom('Banana', 4, n, _, S2), type_error(integer,n)].
    checkException("sub_atom('Banana', 4, n, _, S2).");

    //[sub_atom('Banana', 4, _, m, S2), type_error(integer,m)].
    checkException("sub_atom('Banana', 4, _, m, S2).");
  }

  @Test
  void testCurrentPredicateAndCurrentPredicateAll() throws Exception {
    //[current_predicate(current_predicate/1), failure].
    checkOnce("current_predicate(current_predicate/1).", false);

    final ChoicePoint goal = prepareGoal("some(). some(huzzaa).", "current_predicate(some/X).");
    assertNotNull(goal.next());
    assertEquals(0L, goal.getVarAsNumber("X"));
    assertNotNull(goal.next());
    assertEquals(1L, goal.getVarAsNumber("X"));
    assertNull(goal.next());

    //[current_predicate(run_tests/1), success].
    checkOnce("current_predicate_all(atom/1).", true);
    checkVarValues("current_predicate_all(dispose/X).", "X", "0", "1");

    //[current_predicate(4), type_error(predicate_indicator, 4)].
    checkException("current_predicate(4).");
    //[current_predicate(dog), type_error(predicate_indicator, dog)].
    checkException("current_predicate(dog).");
    //[current_predicate(0/dog), type_error(predicate_indicator, 0/dog)].
    checkException("current_predicate(0/dog).");

    checkOnce("current_predicate(current_predicate/3).", false);
  }

  @Test
  void testAtomConcat() throws Exception {
    //[atom_concat('hello',' world',A), [[A <-- 'hello world']]].
    checkVarValues("atom_concat('hello',' world',A).", "A", "'hello world'");
    //[atom_concat(T,' world','small world'), [[T <-- 'small']]].
    checkVarValues("atom_concat(T,' world','small world').", "T", "'small'");
    //[atom_concat('hello',' world','small world'), failure].
    checkOnce("atom_concat('hello',' world','small world').", false);
    checkOnce("atom_concat('small',' world','small world').", true);

    //[atom_concat(T1,T2,'hello'), [[T1 <-- '',T2 <-- 'hello'],[T1 <-- 'h',T2 <-- 'ello'],[T1 <-- 'he',T2 <-- 'llo'],[T1 <-- 'hel',T2 <-- 'lo'],[T1 <-- 'hell',T2 <-- 'o'],[T1 <-- 'hello',T2 <-- '']]].
    final ChoicePoint goal = prepareGoal("atom_concat(T1,T2,'hello').");
    assertNotNull(goal.next());
    assertEquals("''", goal.getVarAsText("T1"));
    assertEquals("'hello'", goal.getVarAsText("T2"));
    assertNotNull(goal.next());
    assertEquals("'h'", goal.getVarAsText("T1"));
    assertEquals("'ello'", goal.getVarAsText("T2"));
    assertNotNull(goal.next());
    assertEquals("'he'", goal.getVarAsText("T1"));
    assertEquals("'llo'", goal.getVarAsText("T2"));
    assertNotNull(goal.next());
    assertEquals("'hel'", goal.getVarAsText("T1"));
    assertEquals("'lo'", goal.getVarAsText("T2"));
    assertNotNull(goal.next());
    assertEquals("'hell'", goal.getVarAsText("T1"));
    assertEquals("'o'", goal.getVarAsText("T2"));
    assertNotNull(goal.next());
    assertEquals("'hello'", goal.getVarAsText("T1"));
    assertEquals("''", goal.getVarAsText("T2"));
    assertNull(goal.next());

    final ChoicePoint goal2 = prepareGoal("atom_concat(T1,T2,'').");
    assertNotNull(goal2.next());
    assertEquals("''", goal2.getVarAsText("T1"));
    assertEquals("''", goal2.getVarAsText("T2"));
    assertNull(goal2.next());

    //[atom_concat(A1,'iso',A3), instantiation_error].
    checkException("atom_concat(A1,'iso',A3).");

    //[atom_concat('iso',A2,A3), instantiation_error].
    checkException("atom_concat('iso',A2,A3).");

    //[atom_concat(f(a),'iso',A3), type_error(atom,f(a))].
    checkException("atom_concat(f(a),'iso',A3).");

    //[atom_concat('iso',f(a),A3), type_error(atom,f(a))].
    checkException("atom_concat('iso',f(a),A3).");

    //[atom_concat(A1,A2,f(a)), type_error(atom,f(a))].
    checkException("atom_concat(A1,A2,f(a)).");
  }

  @Test
  void testHalt() throws Exception {
    //[dispose, impl_defined].
//    checkException("dispose.");
    //[dispose(1), impl_defined].
//    checkException("dispose(1).");
    //[dispose(a), type_error(integer, a)].
    checkException("dispose(a).");
  }

  @Test
  void testAtomCodes() throws Exception {

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
    checkException("atom_codes(A,L).");
    //[atom_codes(f(a),L), type_error(atom,f(a))].
    checkException("atom_codes(f(a),L).");
    //[atom_codes(A, 0'x), type_error(list,0'x)].
    checkException("atom_codes(A," + (int) 'x' + ").");

    //[atom_codes(A,[ 0'i, 0's, 1000]), representation_error(character_code)]. % 1000 not a code
    checkException("atom_codes(A,[" + (int) 'i' + ',' + (int) 's' + ",1.1]).");
  }

  @Test
  void testCharCode() throws Exception {
    //[char_code(Char,0'c),[[Char <-- c]]].
    //[char_code(Char,163),[[Char <-- '\xa3\']]].

    //[char_code(a,Code),[[Code <-- 0'a]]].
    checkVarValues("char_code(a,Code).", "Code", (long) 'a');
    //[char_code(Char,99),[[Char <-- c]]].
    checkVarValues("char_code(Char,99).", "Char", "'c'");
    //[char_code(b,98),success].
    checkOnce("char_code(b,98).", true);
    //[char_code(b,4),failure].
    checkOnce("char_code(b,4).", false);
    //[char_code('ab',Code),type_error(character, 'ab')].
    checkException("char_code('ab',Code).");
    //[char_code(a,x),type_error(integer, x)].
    checkException("char_code(a,x).");
    //[char_code(Char,Code),instantiation_error].
    checkException("char_code(Char,Code).");
    //[char_code(Char,-2),representation_error(character_code)].
    checkException("char_code(Char,-2).");
  }

  @Test
  void testAtomChars() throws Exception {

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
    checkException("atom_chars(A,L).");
    //[atom_chars(A,[a,E,c]), instantiation_error].
    checkException("atom_chars(A,[a,E,c]).");
    //[atom_chars(A,[a,b|L]), instantiation_error].
    checkException("atom_chars(A,[a,b|L]).");
    //[atom_chars(f(a),L), type_error(atom,f(a))].
    checkException("atom_chars(f(a),L).");
    //[atom_chars(A,iso), type_error(list,iso)].
    checkException("atom_chars(A,iso).");
    //[atom_chars(A,[a,f(b)]), type_error(character,f(b))].
    checkException("atom_chars(A,[a,f(b)]).");
    //[(atom_chars(X,['1','2']), Y is X + 1), type_error(evaluable, '12'/0)].
    checkException("atom_chars(X,['1','2']),Y is X+1.");
  }

  @Test
  void testFunctor_Bis() throws Exception {
    //[functor(foo(a,b,c),foo,3),success].
    checkOnce("functor(foo(a,b,c),foo,3).", true);

    //[functor(foo(a,b,c),X,Y),[[X <-- foo, Y <-- 3]]].
    final ChoicePoint goal = proveGoal("functor(foo(a,b,c),X,Y).");
    assertEquals("'foo'", goal.getVarAsText("X"));
    assertEquals(3L, goal.getVarAsNumber("Y"));
    assertNull(goal.next());

    //[functor(X,foo,3), [[X <-- foo(A,B,C)]]].
    checkVarValues("functor(X,foo,3).", "X", "foo(_,_,_)");

    //[functor(X,foo,0), [[X <-- foo]]].
    checkVarValues("functor(X,foo,0).", "X", "foo");

    //[functor(mats(A,B),A,B), [[A <-- mats,B <-- 2]]].
    final ChoicePoint goal2 = proveGoal("functor(mats(A,B),A,B).");
    assertEquals("'mats'", goal2.getVarAsText("A"));
    assertEquals(2L, goal2.getVarAsNumber("B"));
    assertNull(goal2.next());

    //[functor(foo(a),foo,2), success]. % Must fail
    checkOnce("functor(foo(a),foo,2).", false);

    //[functor(foo(a),fo,1), failure].
    checkOnce("functor(foo(a),fo,1).", false);

    //[functor(1,X,Y), [[X <-- 1,Y <-- 0]]].
    final ChoicePoint goal3 = proveGoal("functor(1,X,Y).");
    assertEquals("1", goal3.getVarAsText("X"), "1");
    assertEquals(0L, goal3.getVarAsNumber("Y"));
    assertNull(goal3.next());

    //[functor(X,1.1,0), [[X <-- 1.1]]].
    final ChoicePoint goal4 = proveGoal("functor(X,1.1,0).");
    assertEquals(1.1d, goal4.getVarAsNumber("X"));
    assertNull(goal4.next());

    //[functor([_|_],'.',2), failure]. % Must succeed
    checkOnce("functor([_|_],'.',2).", true);

    //[functor([],[],0), success].
    checkOnce("functor([],[],0).", true);

    //[functor(X, Y, 3), instantiation_error].
    checkException("functor(X, Y, 3).");

    //[functor(X, foo, N), instantiation_error].
    checkException("functor(X, foo, N).");

    //[functor(X, foo, a), failure].
    checkException("functor(X, foo, a).");

    //[functor(F, 1.5, 1), type_error(atom,1.5)].
    checkException("functor(F, 1.5, 1).");

    //[functor(F,foo(a),1), type_error(atomic,foo(a))].
    checkException("functor(F,foo(a),1).");

    //[(current_prolog_flag(max_arity,A),X is A + 1,functor(T, foo, X)),representation_error(max_arity)].
    checkException("functor(T, foo, X).");

    //[functor(T, foo, -1), domain_error(not_less_than_zero,-1)].
    checkException("functor(T, foo, -1).");
  }

  @Test
  void testFunctor() throws Exception {
    //[(current_prolog_flag(max_arity,A), X is A + 1, functor(T, foo, X)),representation_error(max_arity)].

    //[functor(foo(a,b,c),foo,3), success].
    checkOnce("functor(foo(a,b,c),foo,3).", true);

    //[functor(foo(a,b,c),X,Y), [[X <-- foo, Y <-- 3]]].
    ChoicePoint goal = proveGoal("functor(foo(a,b,c),X,Y).");
    assertEquals(goal.getVarAsText("X"), "'foo'");
    assertEquals(goal.getVarAsNumber("Y"), 3L);
    assertNull(goal.next());

    //[functor(X,foo,3), [[X <-- foo(A,B,C)]]].  % A, B and C are 3 new variables
    checkVarValues("functor(X,foo,3).", "X", "foo(_,_,_)");
    //[functor(X,foo,0), [[X <-- foo]]].
    checkVarValues("functor(X,foo,0).", "X", "foo");

    //[functor(mats(A,B),A,B), [[A <-- mats,B <-- 2]]].
    goal = proveGoal("functor(mats(A,B),A,B).");
    assertEquals(goal.getVarAsText("A"), "'mats'");
    assertEquals(goal.getVarAsNumber("B"), 2L);
    assertNull(goal.next());

    //[functor(foo(a),foo,2), failure].
    checkOnce("functor(foo(a),foo,2).", false);
    //[functor(foo(a),fo,1), failure].
    checkOnce("functor(foo(a),fo,1).", false);

    //[functor(1,X,Y), [[X <-- 1,Y <-- 0]]].
    goal = proveGoal("functor(1,X,Y).");
    assertEquals(goal.getVarAsNumber("X"), 1L);
    assertEquals(goal.getVarAsNumber("Y"), 0L);
    assertNull(goal.next());

    //[functor(X,1.1,0), [[X <-- 1.1]]].
    checkVarValues("functor(X,1.1,0).", "X", "1.1");
    //[functor([_|_],'.',2), success].
    checkOnce("functor([_|_],'.',2).", true);
    //[functor([],[],0), success].
    checkOnce("functor([],[],0).", true);

    //[functor(X, Y, 3), instantiation_error].
    checkException("functor(X,Y,3).");
    //[functor(X, foo, N), instantiation_error].
    checkException("functor(X,foo,N).");
    //[functor(X, foo, a), type_error(integer,a)].
    checkException("functor(X,foo,a).");
    //[functor(F, 1.5, 1), type_error(atom,1.5)].
    checkException("functor(F,1.5,1).");
    //[functor(F,foo(a),1), type_error(atomic,foo(a))].
    checkException("functor(F,foo(a),1).");
    //[functor(T, foo, -1), domain_error(not_less_than_zero,-1)].
    checkException("functor(T,foo,-1).");
  }

  @Test
  void testCautAndFail() throws Exception {
    checkOnce("\\+((!,fail)).", true);
  }

  @Test
  void testNotProvable() throws Exception {
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
    checkException("\\+(3).");
    //[\+(X), instantiation_error]. % Culprit X
    checkException("\\+(X).");
  }

  @Test
  void testOnce() throws Exception {
    //[once(!), success].
    checkOnce("once(!).", true);
    //[(once(!), (X=1; X=2)), [[X <-- 1],[X <-- 2]]].
    checkVarValues("once(!),(X=1;X=2).", "X", 1L, 2L);
    //[once(repeat), success].
    checkOnce("once(repeat).", true);
    //[once(fail), failure].
    checkOnce("once(fail).", false);
    //[once(3), type_error(callable, 3)].
    checkException("once(3).");
    //[once(X), instantiation_error]. % Culprit X
    checkException("once(X).");
  }

  @Test
  void testArg() throws Exception {
    //[arg(1,foo(a,b),a), success].
    checkOnce("arg(1,foo(a,b),a).", true);
    //[arg(1,foo(X,b),a), [[X <-- a]]].
    checkVarValues("arg(1,foo(X,b),a).", "X", "'a'");
    //[arg(1,foo(a,b),X), [[X <-- a]]].
    checkVarValues("arg(1,foo(a,b),X).", "X", "'a'");

    //[arg(2,foo(a, f(X,b), c), f(a, Y)), [[X <-- a, Y <-- b]]].
    final ChoicePoint goal = proveGoal("arg(2,foo(a, f(X,b), c), f(a, Y)).");
    assertEquals(goal.getVarAsText("X"), "'a'");
    assertEquals(goal.getVarAsText("Y"), "'b'");
    assertNull(goal.next());

    //[arg(1,foo(X,b),Y), [[Y <-- X]]].
    checkVarValues("arg(1,foo(X,b),Y).", "Y", "X");

    //[arg(1,foo(a,b),b), failure].
    checkOnce("arg(1,foo(a,b),b).", false);
    //[arg(0,foo(a,b),foo), failure].
    checkOnce("arg(0,foo(a,b),foo).", false);
    //[arg(3,foo(3,4),N), failure].
    checkOnce("arg(3,foo(3,4),N).", false);

    //[arg(X,foo(a,b),a), instantiation_error].
    checkException("arg(X,foo(a,b),a).");
    //[arg(1,X,a), instantiation_error].
    checkException("arg(1,X,a).");
    //[arg(0,atom,A), type_error(compound, atom)].
    checkException("arg(0,atom,a).");
    //[arg(0,3,A), type_error(compound, 3)].
    checkException("arg(0,3,A).");
    //[arg(-3,foo(a,b),A), domain_error(not_less_than_zero,-3)].
    checkException("arg(-3,foo(a,b),A).");
    //[arg(a,foo(a,b),X), type_error(integer, a)].
    checkException("arg(a,foo(a,b),X).");
  }

  @Test
  void testAtomLength() throws Exception {
    //[atom_length('enchanted evening', N), [[N <-- 17]]].
    checkVarValues("atom_length('enchanted evening', N).", "N", 17L);
    //[atom_length('', N), [[N <-- 0]]].
    checkVarValues("atom_length('', N).", "N", 0L);
    //[atom_length(Atom, 4), instantiation_error]. % Culprit Atom
    checkException("atom_length(Atom, 4).");
    //[atom_length('scarlet', 5), failure].
    checkOnce("atom_length('scarlet',5).", false);
    //[atom_length(1.23, 4), type_error(atom, 1.23)].
    checkException("atom_length(1.23,4).");
    //[atom_length(atom, '4'), type_error(integer, '4')].
    checkException("atom_length(atom,'4').");
  }

  @Test
  void testFloat() throws Exception {
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
  void testUnify() throws Exception {
    //['='(1,1), success].
    checkOnce("'='(1,1).", true);
    //['='(X,1),[[X <-- 1]]].
    checkVarValues("'='(X,1).", "X", 1L);
    //['='(X,Y),[[Y <-- X]]].
    checkVarValues("Y=3,'='(X,Y).", "X", 3L);

    //[('='(X,Y),'='(X,abc)),[[X <-- abc, Y <-- abc]]].
    ChoicePoint goal = proveGoal("X=Y,'='(X,abc).");
    assertEquals(goal.getVarAsText("X"), "'abc'");
    assertEquals(goal.getVarAsText("Y"), "'abc'");
    assertNull(goal.next());

    checkVarValues("X=Y,'='(X,abc).", "Y", "'abc'");

    //['='(f(X,def),f(def,Y)), [[X <-- def, Y <-- def]]].
    goal = proveGoal("'='(f(X,def),f(def,Y)).");
    assertEquals(goal.getVarAsText("X"), "'def'");
    assertEquals(goal.getVarAsText("Y"), "'def'");
    assertNull(goal.next());

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
    assertEquals("g(D,D)", goal.getVarAsText("C"));
    assertEquals("g(g(D,D),g(D,D))", goal.getVarAsText("B"));
    assertEquals("g(g(g(D,D),g(D,D)),g(g(D,D),g(D,D)))", goal.getVarAsText("A"));
    assertNull(goal.next());
  }

  @Test
  void testNonVar() throws Exception {
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
  void testCompound() throws Exception {

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
  void testAtomic() throws Exception {
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
  void testAtom() throws Exception {
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
  void testInteger() throws Exception {
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
  void testNumber() throws Exception {
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
  void testNumberChars() throws Exception {
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
    checkException("number_chars(A,['3',' ']).");

    //[number_chars(A,['0','''','A']), [[A <-- 65]]].

    //[number_chars(a,L), type_error(number, a)].
    //[number_chars(A,4), type_error(list, 4)].
    //[number_chars(A,['4',2]), type_error(character, 2)].

    //[number_chars(A,['4','.','2']), [[A <-- 4.2]]].
    checkVarValues("number_chars(A,['4','.','2']).", "A", 4.2d);
    //[number_chars(A,['4','2','.','0','e','-','1']), [[A <-- 4.2]]].
    checkVarValues("number_chars(A,['4','2','.','0','e','-','1']).", "A", 4.2d);
    //[number_chars(A,['0',x,f]), [[A <-- 15]]].
    checkVarValues("number_chars(A,['0',x,f]).", "A", 15L);

    //[number_chars(A,L), instantiation_error].
    checkException("number_chars(A,L).");

    //[number_chars(A,['0','''','A']), [[A <-- 65]]].
//    checkOnceVar("number_chars(A,['0','\\'','A']).", "A", 65);
  }

}