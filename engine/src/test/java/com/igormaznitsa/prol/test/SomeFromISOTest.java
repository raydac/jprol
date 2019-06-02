package com.igormaznitsa.prol.test;

import com.igormaznitsa.prol.exceptions.ProlCustomErrorException;
import com.igormaznitsa.prol.exceptions.ProlException;
import com.igormaznitsa.prol.exceptions.ProlInstantiationErrorException;
import com.igormaznitsa.prol.io.DefaultProlStreamManagerImpl;
import com.igormaznitsa.prol.logic.Goal;
import com.igormaznitsa.prol.logic.ProlContext;
import com.igormaznitsa.prol.parser.ProlConsult;
import junit.framework.TestCase;
import org.junit.Test;

public class SomeFromISOTest extends TestCase {

  @Test
  public void testAbolish() throws Exception {
//[abolish(abolish/1), permission_error(modify,static_procedure,abolish/1)].
//[abolish(foo/a), type_error(integer,a)].
//[abolish(foo/(-1)), domain_error(not_less_than_zero,-1)].
//[(current_prolog_flag(max_arity,A), X is A + 1, abolish(foo/X)), representation_error(max_arity)].
//[abolish(5/2), type_error(atom,5)].

    checkOnce("assert(test(1)),test(X),X==1,abolish(test/1),\\+ test(_).", true);
    checkException("abolish(abolish/1).");
    checkException("abolish(foo/a).");
    //checkException("abolish(foo/(-1))."); // prol doesn't check the right part
    //checkException("abolish(5/2)."); // prol just remove record at vocabulary for such signature
  }

  @Test
  public void testRetract() throws Exception {
//[retract((4 :- X)), type_error(callable, 4)].
//[retract((atom(_) :- X == '[]')),permission_error(modify,static_procedure,atom/1)].
    checkException("retract((4:-X)).");
    checkException("retract((atom(_):-X=='[]')).");
  }

  @Test
  public void testFindAll() throws Exception {
//[findall(X,(X=1 ; X=2),S),[[S <-- [1,2]]]].
//[findall(X+Y,(X=1),S),[[S <-- [1+_]]]].
//[findall(X,fail,L),[[L <-- []]]].
//[findall(X,(X=1 ; X=1),S),[[S <-- [1,1]]]].
//[findall(X,(X=2 ; X=1),[1,2]), failure].
//[findall(X,(X=1 ; X=2),[X,Y]), [[X <-- 1, Y <-- 2]]].
//[findall(X,Goal,S),instantiation_error]. % Culprit Goal
//[findall(X,4,S),type_error(callable, 4)].
//[findall(X,call(1),S),type_error(callable, 1)].

    checkOnceVar("findall(X,(X=1;X=2),X).", "X", "[1,2]");
    checkOnceVar("findall(X,(X=1;X=2),S).", "S", "[1,2]");
    checkOnceVar("findall(X+Y,(X=1),S).", "S", "[1 + Y]"); // changes
    checkOnceVar("findall(X,fail,L).", "L", "[]");
    checkOnceVar("findall(X,(X=1;X=1),S).", "S", "[1,1]");
    checkOnceVar("findall(1, (Y = 1; Y = 2),L).", "L", "[1,1]");
    checkOnceVar("findall(X,(X=Y;X=Z),L).", "L", "[Y,Z]");
    checkOnce("findall(X,(X=2;X=1),[1,2]).", false);

    final Goal goal = proveGoal("findall(X,(X=1;X=2),[X1,Y1])."); // changed from original because at Prol all variables linked by their names inside a goal, so that it is not a bug, it is a feature
    assertEquals(goal.getVarAsText("X1"), "1");
    assertEquals(goal.getVarAsText("Y1"), "2");
    assertNull(goal.solve());

    checkException("findall(X,Goal,S).");
    checkException("findall(X,4,S).");
    checkException("findall(X,call(1),S).");
  }

  @Test
  public void testBagOf() throws Exception {
//[bagof(X, (X = 1; X = 2),L), [[L<-- [1, 2]]]].
//[bagof(X, (X = 1; X = 2),X), [[X<-- [1, 2]]]].
//[bagof(X, (X = Y; X = Z),L), [[L<-- [Y, Z]]]].
//[bagof(X, fail, L), failure].
//[bagof(1, (Y = 1; Y = 2),L), [[L<-- [1], Y<-- 1], [L<-- [1], Y<-- 2]]].
//[bagof(f(X, Y), (X = a; Y = b),L), [[L<-- [f(a, _), f(_, b)]]]].
//[bagof(X, Y ^ ((X = 1, Y = 1);(X = 2,Y = 2)),S), [[S<-- [1, 2]]]].
//[bagof(X, Y ^ ((X = 1; Y = 1);(X = 2,Y = 2)),S), [[S<-- [1, _, 2]]]].
//[(set_prolog_flag(unknown, warning),bagof(X, (Y ^ (X = 1;Y = 1);X = 3),S)), [[S<-- [3]]]].
//[bagof(X, (X = Y; X = Z; Y = 1),L), [[L<-- [Y, Z]], [L<-- [_], Y<-- 1]]].
//[bagof(X, Y ^ Z, L), instantiation_error].
//[bagof(X, 1, L), type_error(callable, 1)].

    checkOnceVar("bagof(X,(X=1;X=2),L).", "L", "[1,2]");
    checkOnceVar("bagof(X,(X=1;X=2),X).", "X", "[1,2]");
    checkOnceVar("bagof(X,(X=Y;X=Z),L).", "L", "[Y,Z]");

    final Goal goal = prepareGoal("bagof(1,(Y = 1; Y = 2),L).");
    assertNotNull(goal.solve());
    assertEquals("[1]",goal.getVarAsText("L"));
    assertEquals("1",goal.getVarAsText("Y"));
    assertNotNull(goal.solve());
    assertEquals("[1]", goal.getVarAsText("L"));
    assertEquals("2", goal.getVarAsText("Y"));
    assertNull(goal.solve());
    
    checkOnce("bagof(X, fail, L).", false);

    final Goal goal2 = prepareGoal("bagof(X, (X = Y; X = Z; Y = 1),L).");
    assertNotNull(goal2.solve());
    assertEquals("[Y,Z]",goal2.getVarAsText("L"));
    assertNotNull(goal2.solve());
    assertEquals("[X]",goal2.getVarAsText("L"));
    assertEquals("1",goal2.getVarAsText("Y"));
    assertNull(goal2.solve());

    final Goal goal3 = prepareGoal("bagof(X, (Y ^ (X = 1;Y = 1);X = 3),S).");
    assertNotNull(goal3.solve());
    assertEquals("[3]", goal3.getVarAsText("S"));
    assertNull(goal3.solve());
    
    final Goal goal4 = prepareGoal("bagof(X, Y ^ ((X = 1; Y = 1);(X = 2,Y = 2)),S).");
    assertNotNull(goal4.solve());
    assertEquals("[1,2,X]", goal4.getVarAsText("S"));
    assertNull(goal4.solve());
    
    final Goal goal5 = prepareGoal("bagof(X, Y ^ ((X = 1, Y = 1);(X = 2,Y = 2)),S).");
    assertNotNull(goal5.solve());
    assertEquals("[1,2]", goal5.getVarAsText("S"));
    assertNull(goal5.solve());
    
    final Goal goal6 = prepareGoal("bagof(f(X, Y), (X = a; Y = b),L).");
    assertNotNull(goal6.solve());
    assertEquals("[f('a',Y),f(X,'b')]", goal6.getVarAsText("L"));
    assertNull(goal6.solve());
    
    checkException("bagof(X, Y ^ Z, L).");
    checkException("bagof(X, 1, L).");
  }

  @Test
  public void testAssertZ() throws Exception {
//[assertz((foo(X) :- X -> call(X))), success].
//[assertz(_), instantiation_error].
//[assertz(4), type_error(callable, 4)].
//[assertz((foo :- 4)), type_error(callable, 4)].
//[assertz((atom(_) :- true)), permission_error(modify,static_procedure,atom/1)].
    checkOnce("assertz((foo(X):-X->call(X))).", true);
    checkException("assertz(_).");
    checkException("assertz(4).");
    //checkException("assertz((foo:-4)).");// prol doesn't check the term fully
    checkException("assertz((atom(_):-true)).");
  }

  @Test
  public void testAssertA() throws Exception {
//[(asserta((bar(X) :- X)), clause(bar(X), B)), [[B <-- call(X)]]].
//[asserta(_), instantiation_error].
//[asserta(4), type_error(callable, 4)].
//[asserta((foo :- 4)), type_error(callable, 4)].
//[asserta((atom(_) :- true)), permission_error(modify,static_procedure,atom/1)].
    checkOnceVar("asserta(bar(X):-call(X)),clause(bar(X),B).", "B", "call(X)");
    checkException("asserta(_).");
    checkException("asserta(4).");
    //checkException("asserta((foo:-4)).");
    checkException("asserta((atom(_):-true)).");
  }

  @Test
  public void testNumberCodes() throws Exception {
//[number_codes(33,L), [[L <-- [0'3,0'3]]]].
//[number_codes(33,[0'3,0'3]), success].
//[number_codes(33.0,L), [[L <-- [51,51,46,48]]]].
//[number_codes(33.0,[0'3,0'.,0'3,0'E,0'+,0'0,0'1]), success].
//[number_codes(A,[0'-,0'2,0'5]), [[A <-- (-25)]]].
//[number_codes(A,[0' ,0'3]), [[A <-- 3]]].
//[number_codes(A,[0'0,0'x,0'f]), [[A <-- 15]]].
//[number_codes(A,[0'0,39,0'a]), [[A <-- 97]]].
//[number_codes(A,[0'4,0'.,0'2]), [[A <-- 4.2]]].
//[number_codes(A,[0'4,0'2,0'.,0'0,0'e,0'-,0'1]), [[A <-- 4.2]]].
//
//[number_codes(A,L), instantiation_error].
//[number_codes(a,L), type_error(number,a)].
//[number_codes(A,4), type_error(list,4)].
//[number_codes(A,[ 0'1, 0'2, 1000]), representation_error(character_code)]. % 1000 not a code
    checkOnceVar("number_codes(33,L).", "L", "[" + (int) '3' + "," + (int) '3' + "]");
    checkOnce("number_codes(33,[51,51]).", true);
    checkOnceVar("number_codes(33.0,L).", "L", "[51,51,46,48]");
    checkOnce("number_codes(33.0,[" + (int) '3' + ',' + (int) '.' + ',' + (int) '3' + ',' + (int) 'E' + ',' + (int) '0' + ',' + (int) '1' + "]).", true);
    checkOnceVar("number_codes(A,[" + (int) '-' + ',' + (int) '2' + ',' + (int) '5' + "]).", "A", "-25");
    checkOnceVar("number_codes(A,[" + (int) ' ' + ',' + (int) '3' + "]).", "A", "' 3'");
    checkOnceVar("number_codes(A,[" + (int) '0' + ',' + (int) 'x' + ',' + (int) 'f' + "]).", "A", "15");
    checkOnceVar("number_codes(A,[" + (int) '4' + ',' + (int) '.' + ',' + (int) '2' + "]).", "A", "4.2");
    //       checkOnceVar("number_codes(A,["+(int)'4'+','+(int)'2'+','+(int)'.'+','+(int)'0'+','+(int)'e'+','+(int)'-'+','+(int)'1'+"]).","A","4.2"); // prol returns 42.0e-1 because it is based on Java number output
  }

  @Test
  public void testTermLtEq() throws Exception {
//['@=<'(1.0,1), success].
//['@=<'(aardvark,zebra), success].
//['@=<'(short,short), success].
//['@=<'(short,shorter), success].
//['@=<'(foo(b),foo(a)), failure].
//['@=<'(X,X), success].
//['@=<'(foo(a,X),foo(b,Y)), success].
    checkOnce("'@=<'(1.0,1).", true);
    checkOnce("'@=<'(aardvark,zebra).", true);
    checkOnce("'@=<'(short,short).", true);
    checkOnce("'@=<'(short,shorter).", true);
    checkOnce("'@=<'(foo(b),foo(a)).", false);
    checkOnce("'@=<'(X,X).", true);
    checkOnce("'@=<'(foo(a,X),foo(b,Y)).", true);
  }

  @Test
  public void testTermLt() throws Exception {
//['@<'(1.0,1), success].
//['@<'(aardvark,zebra), success].
//['@<'(short,short), failure].
//['@<'(short,shorter), success].
//['@<'(foo(b),foo(a)), failure].
//['@<'(X,X), failure].
//['@<'(foo(a,X),foo(b,Y)), success].
    checkOnce("'@<'(1.0,1).", false);// ???
    checkOnce("'@<'(aardvark,zebra).", true);
    checkOnce("'@<'(short,short).", false);
    checkOnce("'@<'(short,shorter).", true);
    checkOnce("'@<'(foo(b),foo(a)).", false);
    checkOnce("'@<'(X,X).", false);
    checkOnce("'@<'(foo(a,X),foo(b,Y)).", true);
  }

  @Test
  public void testTermGtEqu() throws Exception {
//['@>='(1.0,1), failure].
//['@>='(aardvark,zebra), failure].
//['@>='(short,short), success].
//['@>='(short,shorter), failure].
//['@>='(foo(b),foo(a)), success].
//['@>='(X,X), success].
//['@>='(foo(a,X),foo(b,Y)), failure].
    checkOnce("'@>='(1.0,1).", true);// ???
    checkOnce("'@>='(aardvark,zebra).", false);
    checkOnce("'@>='(short,short).", true);
    checkOnce("'@>='(short,shorter).", false);
    checkOnce("'@>='(foo(b),foo(a)).", true);
    checkOnce("'@>='(X,X).", true);
    checkOnce("'@>='(foo(a,X),foo(b,Y)).", false);
  }

  @Test
  public void testTermGt() throws Exception {
//['@>'(1.0,1), failure].
//['@>'(aardvark,zebra), failure].
//['@>'(short,short), failure].
//['@>'(short,shorter), failure].
//['@>'(foo(b),foo(a)), success].
//['@>'(X,X), failure].
//['@>'(foo(a,X),foo(b,Y)), failure].
    checkOnce("'@>'(1.0,1).", false);
    checkOnce("'@>'(aardvark,zebra).", false);
    checkOnce("'@>'(short,short).", false);
    checkOnce("'@>'(short,shorter).", false);
    checkOnce("'@>'(foo(b),foo(a)).", true);
    checkOnce("'@>'(X,X).", false);
    checkOnce("'@>'(foo(a,X),foo(b,Y)).", false);
  }

  @Test
  public void testEqu() throws Exception {
//['=='(1,1), success].
//['=='(X,X), success].
//['=='(1,2), failure].
//['=='(X,1), failure].
//['=='(X,Y), failure].
//['=='(_,_), failure].
//['=='(X,a(X)), failure].
//['=='(f(a),f(a)), success].

    checkOnce("'=='(1,1).", true);
    checkOnce("'=='(X,X).", true);
    checkOnce("'=='(1,2).", false);
    checkOnce("'=='(X,1).", false);
    checkOnce("'=='(X,Y).", false);
    checkOnce("'=='(_,_).", false);
    checkOnce("'=='(X,a(X)).", false);
    checkOnce("'=='(f(a),f(a)).", true);
  }

  @Test
  public void testDiff() throws Exception {
//['\\=='(1,1), failure].
//['\\=='(X,X), failure].
//['\\=='(1,2), success].
//['\\=='(X,1), success].
//['\\=='(X,Y), success].
//['\\=='(_,_), success].
//['\\=='(X,a(X)), success].
//['\\=='(f(a),f(a)), failure].

    checkOnce("'\\\\=='(1,1).", false);
    checkOnce("'\\\\=='(X,X).", false);
    checkOnce("'\\\\=='(1,2).", true);
    checkOnce("'\\\\=='(X,1).", true);
    checkOnce("'\\\\=='(X,Y).", true);
    checkOnce("'\\\\=='(_,_).", true);
    checkOnce("'\\\\=='(X,a(X)).", true);
    checkOnce("'\\\\=='(f(a),f(a)).", false);
  }

  @Test
  public void testLtEqu() throws Exception {
//['=<'(0,1), success].
//['=<'(1.0,1), success].
//['=<'(3*2,7-1), success].
//['=<'(X,5), instantiation_error].
//['=<'(2 + floot(1),5), type_error(evaluable, floot/1)].
    checkOnce("'=<'(0,1).", true);
    checkOnce("'=<'(1.0,1).", true);
    checkOnce("'=<'(3*2,7-1).", true);
    checkException("'=<'(X,5).");
    checkException("'=<'(2+floot(1),5).");
  }

  @Test
  public void testLt() throws Exception {
//['<'(0,1), success].
//['<'(1.0,1), failure].
//['<'(3*2,7-1), failure].
//['<'(X,5), instantiation_error].
//['<'(2 + floot(1),5), type_error(evaluable, floot/1)].
    checkOnce("'<'(0,1).", true);
    checkOnce("'<'(1.0,1).", false);
    checkOnce("'<'(3*2,7-1).", false);
    checkException("'<'(X,5).");
    checkException("'<'(2+floot(1),5).");
  }

  @Test
  public void testGtEqu() throws Exception {
//['>='(0,1), failure].
//['>='(1.0,1), success].
//['>='(3*2,7-1), success].
//['>='(X,5), instantiation_error].
//['>='(2 + floot(1),5), type_error(evaluable, floot/1)].
    checkOnce("'>='(0,1).", false);
    checkOnce("'>='(1.0,1).", true);
    checkOnce("'>='(3*2,7-1).", true);
    checkException("'>='(X,5).");
    checkException("'>='(2+floot(1),5).");
  }

  @Test
  public void testGt() throws Exception {
//['>'(0,1), failure].
//['>'(1.0,1), failure].
//['>'(3*2,7-1), failure].
//['>'(X,5), instantiation_error].
//['>'(2 + floot(1),5), type_error(evaluable, floot/1)].
    checkOnce("'>'(0,1).", false);
    checkOnce("'>'(1.0,1).", false);
    checkOnce("'>'(3*2,7-1).", false);
    checkException("'>'(X,5).");
    checkException("'>'(2+floot(1),5).");
  }

  @Test
  public void testArithEq() throws Exception {
//['=:='(0,1), failure].
//['=:='(1.0,1), success].
//['=:='(3 * 2,7 - 1), success].
//['=:='(N,5), instantiation_error].
//['=:='(floot(1),5), type_error(evaluable, floot/1)].
//[0.333 =:= 1/3, failure].
    checkOnce("'=:='(0,1).", false);
    checkOnce("'=:='(1.0,1).", true);
    checkOnce("'=:='(3*2,7-1).", true);
    checkException("'=:='(N,5).");
    checkException("'=:='(floot(1),5).");
    checkOnce("0.333=:=1/3.", false);
  }

  @Test
  public void testArithDiff() throws Exception {
//['=\\='(0,1), success].
//['=\\='(1.0,1), failure].
//['=\\='(3 * 2,7 - 1), failure].
//['=\\='(N,5), instantiation_error].
//['=\\='(floot(1),5), type_error(evaluable, floot/1)].
    checkOnce("'=\\\\='(0,1).", true);
    checkOnce("'=\\\\='(1.0,1).", false);
    checkOnce("'=\\\\='(3*2,7-1).", false);
    checkException("'=\\\\='(N,5).");
    checkException("'=\\\\='(floot(1),5).");
  }

  @Test
  public void testCopyTerm() throws Exception {
//[copy_term(X,Y), success].
//[copy_term(X,3), success].
//[copy_term(_,a), success].
//[copy_term(a+X,X+b),[[X <-- a]]].
//[copy_term(_,_), success].
//[copy_term(X+X+Y,A+B+B),[[B <-- A]]].
//[copy_term(a,a), success].
//[copy_term(a,b), failure].
//[copy_term(f(a),f(X)),[[X <-- a]]].
//[(copy_term(a+X,X+b),copy_term(a+X,X+b)), failure].
    checkOnce("copy_term(X,Y).", true);
    checkOnce("copy_term(X,3).", true);
    checkOnce("copy_term(_,3).", true);
    checkOnceVar("copy_term(a+X,X+b).", "X", "'a'");
    checkOnce("copy_term(_,_).", true);
    //checkOnceVar("copy_term(X+X+Y,A+B+B).","B","A"); // prol links variables as objects so it is no so easy to say which variable is parent
    checkOnce("copy_term(a,a).", true);
    checkOnce("copy_term(a,b).", false);
    checkOnceVar("copy_term(f(a),f(X)).", "X", "'a'");
    checkOnce("copy_term(a+X,X+b),copy_term(a+X,X+b).", false);
  }

  @Test
  public void testClause() throws Exception {
//[clause(x,Body), failure].
//[clause(_,B), instantiation_error].
//[clause(4,B), type_error(callable,4)].
//[clause(f(_),5), type_error(callable,5)].
//[clause(atom(_),Body), permission_error(access,private_procedure,atom/1)].
    checkOnce("clause(x,Body).", false);
    checkException("clause(_,B).");
    checkException("clause(4,B).");
    checkException("clause(f(_),5).");
    checkException("clause(atom(_),Body).");
  }

  @Test
  public void testCall() throws Exception {
//[call(!),success].
//[call(fail), failure].
//[call((fail, X)), failure].
//[call((fail, call(1))), failure].
//[call((write(3), X)), instantiation_error].
//[call((write(3), call(1))), type_error(callable,1)].
//[call(X), instantiation_error].
//[call(1), type_error(callable,1)].
//[call((fail, 1)), type_error(callable,(fail,1))].
//[call((write(3), 1)), type_error(callable,(write(3), 1))].
//[call((1; true)), type_error(callable,(1; true))].

    checkOnce("call(!).", true);
    checkOnce("call(fail).", false);
    checkOnce("call((fail,X)).", false);
    checkOnce("call((fail,call(1))).", false);
    checkException("call((write(3),X)).");
    checkException("call((write(3),call(1))).");
    checkException("call(X).");
    checkException("call(1).");
//        checkException("call((fail,1))."); // it iw not working at proll because prol checks sequentially and fail will be the first one
    checkException("call([fail]).");
    checkException("call((write(3),1)).");
    checkException("call((1;true)).");
  }

  @Test
  public void testAtomCodes() throws Exception {
    checkOnceVar("atom_codes('',L).", "L", "[]");
    checkOnceVar("atom_codes([],L).", "L", "[" + (int) '[' + ',' + (int) ']' + "]");
    checkOnceVar("atom_codes('\\'',L).", "L", "[39]");
    checkOnceVar("atom_codes('iso',L).", "L", "[" + (int) 'i' + ',' + (int) 's' + ',' + (int) 'o' + ']');
    checkOnceVar("atom_codes(A,[" + (int) 'p' + ',' + (int) 'r' + ',' + (int) 'o' + ',' + (int) 'l' + ',' + (int) 'o' + ',' + (int) 'g' + "]).", "A", "'prolog'");
    checkOnceVar("atom_codes('North',[" + (int) 'N' + '|' + "L]).", "L", "[" + (int) 'o' + ',' + (int) 'r' + ',' + (int) 't' + ',' + (int) 'h' + ']');
    checkOnce("atom_codes('iso',[" + (int) 's' + ',' + (int) 'o' + "]).", false);
    checkException("atom_codes(A,L).");
    checkException("atom_codes(f(a),L).");
    checkException("atom_codes(A," + (int) 'x' + ").");
    checkException("atom_codes(A,[" + (int) 'i' + ',' + (int) 's' + ",1.1]).");
  }

  @Test
  public void testCharCode() throws Exception {
//[char_code(Char,0'c),[[Char <-- c]]].
//[char_code(Char,163),[[Char <-- '\xa3\']]].
    checkOnceVar("char_code(a,Code).", "Code", (int) 'a');
    checkOnceVar("char_code(Char,99).", "Char", "'c'");
    checkOnce("char_code(b,98).", true);
    checkOnce("char_code(b,4).", false);
    checkException("char_code('ab',Code).");
    checkException("char_code(a,x).");
    checkException("char_code(Char,Code).");
    checkException("char_code(Char,-2).");
  }

  @Test
  public void testAtomChars() throws Exception {

    checkOnceVar("atom_chars('',L).", "L", "[]");
    checkOnceVar("atom_chars([],L).", "L", "['[',']']");
    checkOnceVar("atom_chars('iso',L).", "L", "['i','s','o']");
    checkOnceVar("atom_chars(A,['p','r','o','l','o','g']).", "A", "'prolog'");
    checkOnceVar("atom_chars('North',['N'|X]).", "X", "['o','r','t','h']");

    checkOnce("atom_chars('iso',['i','s']).", false);
    checkException("atom_chars(A,L).");
    checkException("atom_chars(A,[a,E,c]).");
    checkException("atom_chars(A,[a,b|L]).");
    checkException("atom_chars(f(a),L).");
    checkException("atom_chars(A,iso).");
    checkException("atom_chars(A,[a,f(b)]).");
    checkException("atom_chars(X,['1','2']),Y is X+1.");
  }

  @Test
  public void testFunctor() throws Exception {
    checkOnce("functor(foo(a,b,c),3).", true);

    Goal goal = proveGoal("functor(foo(a,b,c),X,Y).");
    assertEquals(goal.getVarAsText("X"), "'foo'");
    assertEquals(goal.getVarAsNumber("Y"), 3);
    assertNull(goal.solve());

    checkOnceVar("functor(X,foo,3).", "X", "foo(_,_,_)");
    checkOnceVar("functor(X,foo,0).", "X", "foo");

    goal = proveGoal("functor(mats(A,B),A,B).");
    assertEquals(goal.getVarAsText("A"), "'mats'");
    assertEquals(goal.getVarAsNumber("B"), 2);
    assertNull(goal.solve());

    checkOnce("functor(foo(a),foo,2).", false);
    checkOnce("functor(foo(a),fo,1).", false);

    goal = proveGoal("functor(1,X,Y).");
    assertEquals(goal.getVarAsNumber("X"), 1);
    assertEquals(goal.getVarAsNumber("Y"), 0);
    assertNull(goal.solve());

    checkOnceVar("functor(X,1.1,0).", "X", "1.1");
    checkOnce("functor([_|_],'.',2).", true);
    checkOnce("functor([],[],0).", true);

    checkException("functor(X,Y,3).");
    checkException("functor(X,foo,N).");
    checkException("functor(X,foo,a).");
    checkException("functor(F,1.5,1).");
    checkException("functor(F,foo(a),1).");
    checkException("functor(T,foo,-1).");
  }

  @Test
  public void testNotProvable() throws Exception {
    checkOnce("\\+(true).", false);
    checkOnce("\\+(!).", false);
    checkOnce("\\+((!,fail)).", true);
    checkOnceVar("((X=1;X=2),\\+((!,fail))).", "X", 1, 2);
    checkOnce("\\+(4=5).", true);
    checkException("\\+(3).");
    checkException("\\+(X).");
  }

  @Test
  public void testOnce() throws Exception {
    checkOnce("once(!).", true);
    checkOnceVar("once(!),(X=1;X=2).", "X", 1, 2);
    checkOnce("once(repeat).", true);
    checkOnce("once(fail).", false);
    checkException("once(3).");
    checkException("once(X).");
  }

  @Test
  public void testArg() throws Exception {
    checkOnce("arg(1,foo(a,b),a).", true);
    checkOnceVar("arg(1,foo(X,b),a).", "X", "'a'");
    checkOnceVar("arg(1,foo(a,b),X).", "X", "'a'");

    final Goal goal = proveGoal("arg(2,foo(a, f(X,b), c), f(a, Y)).");
    assertEquals(goal.getVarAsText("X"), "'a'");
    assertEquals(goal.getVarAsText("Y"), "'b'");
    assertNull(goal.solve());

    checkOnceVar("arg(1,foo(a(X),b),Y).", "Y", "a(X)");

    checkOnce("arg(1,foo(a,b),b).", false);
    checkOnce("arg(0,foo(a,b),foo).", false);
    checkOnce("arg(3,foo(3,4),N).", false);

    checkException("arg(X,foo(a,b),a).");
    checkException("arg(1,X,a).");
    checkException("arg(0,atom,a).");
    checkException("arg(0,3,A).");
    checkException("arg(-3,foo(a,b),A).");
    checkException("arg(a,foo(a,b),X).");
  }

  @Test
  public void testIs() throws Exception {
    checkOnceVar("'is'(Result,3+11.0).", "Result", 14.0f);
    Goal goal = proveGoal("X=1+2,'is'(Y,X*3).");
    assertEquals(goal.getVarAsText("X"), "1 + 2");
    assertEquals(goal.getVarAsNumber("Y"), 9);
    assertNull(goal.solve());

    checkException("'is'(foo,77).");
    checkException("'is'(77,N).");
    checkException("'is'(77,foo).");
  }

  @Test
  public void testAtomLength() throws Exception {
    checkOnceVar("atom_length('enchanted evening', N).", "N", 17);
    checkOnceVar("atom_length('', N).", "N", 0);
    checkException("atom_length(Atom, 4).");
    checkOnce("atom_length('scarlet',5).", false);
    checkException("atom_length(1.23,4).");
    checkException("atom_length(atom,'4').");
  }

  @Test
  public void testFloat() throws Exception {
    checkOnce("float(3.3).", true);
    checkOnce("float(-3.3).", true);
    checkOnce("float(3).", false);
    checkOnce("float(atom).", false);
    checkOnce("float(X).", false);
  }

  @Test
  public void testUnify() throws Exception {
    checkOnce("'='(1,1).", true);
    checkOnceVar("'='(X,1).", "X", 1);
    checkOnceVar("Y=3,'='(X,Y).", "X", 3);

    Goal goal = proveGoal("X=Y,'='(X,abc).");
    assertEquals(goal.getVarAsText("X"), "'abc'");
    assertEquals(goal.getVarAsText("Y"), "'abc'");
    assertNull(goal.solve());

    checkOnceVar("X=Y,'='(X,abc).", "Y", "'abc'");

    goal = proveGoal("'='(f(X,def),f(def,Y)).");
    assertEquals(goal.getVarAsText("X"), "'def'");
    assertEquals(goal.getVarAsText("Y"), "'def'");
    assertNull(goal.solve());

    checkOnce("'='(1,2).", false);
    checkOnce("'='(1,1.000001).", false);
    checkOnce("'='(g(X),f(f(X))).", false);
    checkOnce("'='(g(X),f(f(X))).", false);
    checkOnce("'='(f(X,1),f(a(X))).", false);
    checkOnce("'='(f(X,Y,X),f(a(X),a(Y),Y,2)).", false);

    checkOnce("'='(f(X,Y,X),f(a(X),a(Y),Y,2)).", false);

    goal = proveGoal("'='(f(A,B,C),f(g(B,B),g(C,C),g(D,D))).");
    assertEquals(goal.getVarAsText("A"), "g(g(g(D,D),g(D,D)),g(g(D,D),g(D,D)))");
    assertEquals(goal.getVarAsText("B"), "g(g(D,D),g(D,D))");
    assertEquals(goal.getVarAsText("C"), "g(D,D)");
    assertNull(goal.solve());
  }

  @Test
  public void testTrueFail() throws Exception {
    checkOnce("true.", true);
    checkOnce("fail.", false);
  }

  @Test
  public void testRepeat() throws Exception {
    checkOnce("repeat,!,fail.", false);
  }

  @Test
  public void testNotUnify() throws Exception {
    checkOnce("'\\\\='(1,1).", false);
    checkOnce("'\\\\='(X,1).", false);
    checkOnce("'\\\\='(X,Y).", false);
    checkOnce("'\\\\='(X,Y),'\\\\='(X,abc).", false);
    checkOnce("'\\\\='(f(X,def),f(def,Y)).", false);
    checkOnce("'\\\\='(1,2).", true);
    checkOnce("'\\\\='(1,1.00001).", true);
    checkOnce("'\\\\='(g(X),f(f(X))).", true);
    checkOnce("'\\\\='(f(X,1),f(a(X))).", true);
    checkOnce("'\\\\='(f(X,Y,X),f(a(X),a(Y),Y,2)).", true);
  }

  @Test
  public void testNonVar() throws Exception {
    checkOnce("nonvar(33.3).", true);
    checkOnce("nonvar(false).", true);
    checkOnce("nonvar(Foo).", false);
    checkOnceVar("foo=Foo,nonvar(Foo).", "Foo", "'foo'");
    checkOnce("nonvar(_).", false);
    checkOnce("nonvar(a(b)).", true);
  }

  @Test
  public void testCompound() throws Exception {
    checkOnce("compound(33.3).", false);
    checkOnce("compound(-33.3).", false);
    checkOnce("compound(-a).", true);
    checkOnce("compound(_).", false);
    checkOnce("compound(a).", false);
    checkOnce("compound(a(b)).", true);
    checkOnce("compound([]).", false);
    checkOnce("compound([a]).", true);
  }

  @Test
  public void testAtomic() throws Exception {
    checkOnce("atomic(atom).", true);
    checkOnce("atomic(a(b)).", false);
    checkOnce("atomic(Var).", false);
    checkOnce("atomic([]).", true);
    checkOnce("atomic(6).", true);
    checkOnce("atomic(3.3).", true);
  }

  @Test
  public void testAtom() throws Exception {
    checkOnce("atom(atom).", true);
    checkOnce("atom('string').", true);
    checkOnce("atom(a(b)).", false);
    checkOnce("atom(Var).", false);
    checkOnce("atom([]).", true);
    checkOnce("atom(6).", false);
    checkOnce("atom(3.3).", false);
  }

  @Test
  public void testInteger() throws Exception {
    checkOnce("integer(3).", true);
    checkOnce("integer(-3).", true);
    checkOnce("integer(3.3).", false);
    checkOnce("integer(X).", false);
    checkOnce("integer(atom).", false);
  }

  @Test
  public void testOr() throws Exception {
    checkOnce("';'(true, fail).", true);
    checkOnce("';'((!, fail), true).", false);
    checkOnce("';'(!, call(3)).", true);
    checkOnceVar("';'((X=1, !), X=2).", "X", 1);
    checkOnceVar("';'(X=1, X=2).", "X", 1, 2);
  }

  @Test
  public void testAnd() throws Exception {
    checkOnce("','(X=1,var(X)).", false);
    checkOnceVar("','(var(X),X=1).", "X", 1);
    checkOnce("','(fail,call(3)).", false);
    checkOnceVar("','(X=true,call(X)).", "X", "true");
    checkOnce("','(nofoo(X), call(X)).", false);
  }

  @Test
  public void testIfThenElse() throws Exception {
    checkOnce("';'('->'(true, true), fail).", true);
    checkOnce("';'('->'(fail, true), true).", true);
    checkOnce("';'('->'(true, fail), fail).", false);
    checkOnce("';'('->'(fail, true), fail).", false);
    checkOnceVar("true->X=1;X=2.", "X", 1);
    checkOnceVar("';'('->'(fail, X=1), X=2).", "X", 2);
    checkOnceVar("';'('->'(true, ';'(X=1, X=2)), true).", "X", 1, 2);
    checkOnceVar("';'('->'(';'(X=1, X=2), true), true).", "X", 1);
  }

  @Test
  public void testIfThen() throws Exception {
    checkOnce("'->'(true, true).", true);
    checkOnce("'->'(true, fail).", false);
    checkOnceVar("true -> X=1.", "X", 1);
    checkOnceVar("'->'(';'(X=1, X=2), true).", "X", 1);
    checkOnceVar("'->'(true, ';'(X=1, X=2)).", "X", 1, 2);
  }

  @Test
  public void testCut() throws Exception {
    checkOnce("!.", true);
    checkOnce("(!,fail;true).", false);
    checkOnce("call(!),fail;true.", true);
  }

  @Test
  public void testNumber() throws Exception {
    checkOnce("number(3).", true);
    checkOnce("number(3.3).", true);
    checkOnce("number(-3).", true);
    checkOnce("number(a).", false);
    checkOnce("number(X).", false);
  }

  @Test
  public void testNumberChars() throws Exception {
    checkOnceVar("number_chars(33,L).", "L", "['3','3']");
    checkOnceVar("number_chars(33.0,L).", "L", "['3','3','.','0']");
    checkOnceVar("number_chars(X,['3','.','3','E','+','0']).", "X", 3.3f);
    checkOnce("number_chars(3.3,['3','.','3','E','+','0']).", true);
    checkOnceVar("number_chars(A,['-','2','5']).", "A", -25);
    checkOnceVar("number_chars(A,['\n',' ','3']).", "A", 3);

    try {
      checkOnce("number_chars(A,['3',' ']).", true);
    } catch (ProlCustomErrorException ex) {
      assertEquals(ex.getAsStruct().getFunctor().getText(), "syntax_error");
    }

    checkOnceVar("number_chars(A,['4','.','2']).", "A", 4.2f);
    checkOnceVar("number_chars(A,['4','2','.','0','e','-','1']).", "A", 4.2f);
    checkOnceVar("number_chars(A,['0',x,f]).", "A", 15);

    try {
      checkOnce("number_chars(A,L).", true);
      fail();
    } catch (ProlInstantiationErrorException ex) {
      assertTrue(true);
    } catch (Throwable ex) {
      fail();
    }

    //[number_chars(A,['0','''','A']), [[A <-- 65]]].
  }

  private void checkOnce(String goal, boolean result) throws Exception {
    final ProlContext context = makeContext(goal);
    final Goal thisGoal = new Goal(goal, context);
    if (result) {
      assertNotNull(thisGoal.solve());
      assertNull(thisGoal.solve());
    } else {
      assertNull(thisGoal.solve());
    }
  }

  private void checkException(String goal) throws Exception {
    final ProlContext context = makeContext(goal);
    final Goal thisGoal = new Goal(goal, context);
    try {
      thisGoal.solve();
      fail();
    } catch (ProlException ex) {
      assertTrue(ex.toString(), true);

    }
  }

  private Goal prepareGoal(String goal) throws Exception {
    final ProlContext context = makeContext(goal);
    final Goal thisGoal = new Goal(goal, context);
    return thisGoal;
  }

  private Goal proveGoal(String goal) throws Exception {
    final Goal thisGoal = this.prepareGoal(goal);
    assertNotNull(thisGoal.solve());
    return thisGoal;
  }

  private void checkOnceVar(String goal, String var, Object... result) throws Exception {
    final ProlContext context = makeContext(goal);
    final Goal thisGoal = new Goal(goal, context);

    for (Object res : result) {
      assertNotNull(thisGoal.solve());
      if (res instanceof Number) {
        if (res instanceof Float) {
          assertTrue(Float.compare(thisGoal.getVarAsNumber(var).floatValue(), (Float) res) == 0);
        } else {
          assertEquals(thisGoal.getVarAsNumber(var), res);
        }

      } else {
        assertEquals(res.toString(), thisGoal.getVarAsText(var));
      }
    }
    assertNull(thisGoal.solve());
  }

  private ProlContext makeContext(final String knowledgeBase) throws Exception {
    final ProlContext context = new ProlContext("PreparedGoal test", DefaultProlStreamManagerImpl.getInstance());
    final ProlConsult consult = new ProlConsult(knowledgeBase, context);
    consult.consult();
    return context;
  }
}
