![JPROL logo](art/github_logo.png)   
[![License Apache 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Java 1.8+](https://img.shields.io/badge/java-1.8%2b-green.svg)](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
[![PayPal donation](https://img.shields.io/badge/donation-PayPal-cyan.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AHWJHJFBAWGL2)
[![YooMoney donation](https://img.shields.io/badge/donation-Yoo.money-blue.svg)](https://yoomoney.ru/to/41001158080699)


# Introduction
In 1990 (it was the USSR), I bought a book titled as "Prolog Programming for Artificial Intelligence" (written by Ivan Bratko) but didn't have enough time to read for about 20 years. In 2009 I got enough free time to read the book and was amazed by the power of the computer language. To learn it better I wrote small Java based Prolog embeddable engine and called it JProl.   

JProl is a small Prolog engine developed with several libraries provided out of the box. It supports Edinburgh Prolog style and its GUI version provides way to learn Prolog and develop interactive programs and experiments. The engine is written in not very optimal way and I can't recommend it to use in hi-performance or very complex cases but if you need to resolve easy learning purposes or implementation small Prolog-based DSL solutions then it can help.   

In 2014 I was asked by [Yuki Katsura](http://iprolog.appstor.io/) to open sources of the engine (he made iPad version) and the project was published as OSS under Apache License 2.0

# Use in maven projects
Since 2020, the engine core has been published in the maven central and can be [easily injected into Java projects](https://search.maven.org/artifact/com.igormaznitsa/jprol-core/2.0.0/jar), for instance below example for Maven
```xml
<dependency>
    <groupId>com.igormaznitsa</groupId>
    <artifactId>jprol-core</artifactId>
    <version>2.0.0</version>
</dependency>
```
Keep in mind that as parser of Prolog sources, the engine uses [my another project](https://github.com/raydac/java-prolog-parser) whichh also published in the maven central. Inintally the engina had embedded parser but then I decided to split it to two projects.

# Prebuilt versions
There are published pre-built versions for main OSes (I can test it only for Linux, so that notify me about issues under Windows and MacOS). [Executable files can be downloaded from the latest release page](https://github.com/raydac/jprol/releases/latest)

# Embedded GUI editor
The engine has very small end restricted embedded GUI editor to create, edit and execute one Prolog program.
![GUIEditor](https://github.com/raydac/jprol/blob/master/jprolguieditor.png)

# How to define a predicate in Java
It allows communicate with Java methods and use them as predicates through special annotations.
For instance, below I show how implemented the `<</2` predicate of the core JProl library. As you can see, the method is just marked by `@JProlPredicate` annotation. Of course it is impossible to mark just any method because a marked method has to have special signature among arguments: `JProlChoicePoint` and `TermStruct`.
```Java
@JProlPredicate(evaluable = true, signature = "<</2", args = {
  "+evaluable,+evaluable"}, reference = "Bitwise left shift")
public static Term predicateSHIFTL2(final JProlChoicePoint goal, final TermStruct predicate) {
  final NumericTerm left = calculatEvaluable(goal, predicate.getElement(0).findNonVarOrSame());
  final NumericTerm right = calculatEvaluable(goal, predicate.getElement(1).findNonVarOrSame());

  final long value = left.toNumber().longValue();
  final long shift = right.toNumber().longValue();

  return Terms.newLong(value << shift);
}
```
# How to define new operators in library
There is way to define more than one operator in your library, for such purposes you should use the `@ProlOperators` annotation.
```Java
@JProlOperators(Operators = {
    @JProlOperator(priority = 700, type = OpAssoc.OPTYPE_XFX, name = "is"),
    @JProlOperator(priority = 700, type = OpAssoc.OPTYPE_XFX, name = "="),
    @JProlOperator(priority = 1000, type = OpAssoc.OPTYPE_XFY, name = ","),
    @JProlOperator(priority = 1050, type = OpAssoc.OPTYPE_XFY, name = "->"),
....
    @JProlOperator(priority = 300, type = OpAssoc.OPTYPE_XFX, name = "mod"),
    @JProlOperator(priority = 200, type = OpAssoc.OPTYPE_FY, name = "\\"),
    @JProlOperator(priority = 200, type = OpAssoc.OPTYPE_XFX, name = "**")
})
public class JProlCoreLibrary extends AbstractJProlLibrary {
...
```
# Call the Engine from Java
Example below shows how easily define Eight Queens puzzle and find all its solutions.
```Java
JProlContext context = new JProlContext(
    "test-context",
    new JProlCoreLibrary()
);
context.consult(new StringReader(
    "solution([]). solution([X/Y|Others]):-solution(Others),member(Y,[1,2,3,4,5,6,7,8]),notattack(X/Y,Others). notattack(_,[]). notattack(X/Y,[X1/Y1 | Others]):- Y=\\=Y1, Y1-Y=\\=X1-X, Y1-Y=\\=X-X1, notattack(X/Y,Others). member(Item,[Item|Rest]). member(Item,[First|Rest]):-member(Item,Rest). template([1/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8])."));

JProlChoicePoint goal = new JProlChoicePoint(
    "solution([1/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8]),Res = [Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8].",
    context);

Term result;
while((result = goal.prove()) != null) {
    TermList solution  = (TermList) goal.findVar("Res").get().getValue();
    System.out.println(solution.toSrcString());
}
```

## Flags

The bootstrap library of the engine has two predicates `set_prolog_flag/2` and `get_prolog_flag/2`. They can be used to tune work of the engine or get extra info. Not all of them still supported.
- Flags:
    - __address_bit__ address size of the hosting machine. Typically 32 or 64.
    - __allow_variable_name_as_functor__ 
    - __os.name__
    - __os.arch__
    - __bounded__ if true, integer representation is bound by min_integer and max_integer
    - __debug__ switch debugging mode on/off
    - __dialect__ fixed to jprol
    - __encoding__ default encoding used for opening files in text mode. 
    - __gc__ if true (default), the garbage collector is active.
    - __max_arity__  flag describing there is no maximum arity to compound terms
    - __max_integer__ maximum integer value if integers are bounded
    - __min_integer__ minimum integer value if integers are bounded
    - __cpu_count__ number of physical CPUs or cores in the system
    - __unknown__ determines the behaviour if an undefined procedure is encountered. If fail, the predicate fails silently. Allowed `error`,`fail` or `warning` by default `error`
    - __home__ notion of the home directory
    - __verify__
    - __version_data__

## Prolog and Graphics
JProl GUI editor module has predicates to work with graphics and even can save images. As an example, below you can find a program to draw a Koch snowflake fractal.
```Prolog
kochsnowflake(N,L) :- settitle('Koch snowflake'), brushcolor(white), graphics(300,300), pencolor(red), time((fractal(N,30,80,L,0,X1,Y1), fractal(N,X1,Y1,L,2.0943951024,X2,Y2), fractal(N,X2,Y2,L,4.1887902048,_,_))).

fractal(0,X,Y,L,A,XN,YN):- !, XN is X+L*cos(A), YN is Y+L*sin(A), XX is round(X),XXN is round(XN), YY is round(Y),YYN is round(YN), plot(XX,YY,XXN,YYN).
fractal(I,X,Y,L,A,XN,YN):- II is I-1, LL is L/3, A2 is A-1.0471975512, A3 is A+1.0471975512, fractal(II,X,Y,LL,A,X1,Y1), fractal(II,X1,Y1,LL,A2,X2,Y2), fractal(II,X2,Y2,LL,A3,X3,Y3), fractal(II,X3,Y3,LL,A,XN,YN).

?-kochsnowflake(5,300).
```
![KochSnowflake](https://github.com/raydac/jprol/blob/master/jprolgui.png)

# Multi-threading
Because Prolog is a declarative language, it is well prepared for multi-tasking so it was interesting for me to implement and check support of multi-thread support in the engine. The engine allows to use pair predicates to span new threads (`fork/1` and `async/1`), also it has special predicate `waitasync/0` to make inter-process synchronization which allows to wait until the end of all spawned threads. The `waitasync/0` predicate can be used only from the main thread (I mean the root goal). Also there are support of lockers, they provide way to create and process critical sections through `lock/1`, `unlock/1` and `trylock/1` predicates. Take a look at the example below, it shows how to draw 3000000 color dots in multi-threads. There three threads are starting and each thread draws its own color points.
```Prolog
threadRed(P) :- for(_, 0, P), rnd(500, X), rnd(400, Y), lock(gfx), pencolor(red), dot(X, Y), unlock(gfx), fail.
    threadGreen(P) :- for(_, 0, P), rnd(500,X), rnd(400, Y), lock(gfx), pencolor(green), dot(X, Y), unlock(gfx), fail.
    threadBlue(P) :- for(_, 0, P), rnd(500, X), rnd(400, Y), lock(gfx), pencolor(blue), dot(X,Y), unlock(gfx), fail.

    ?- P = 1000000, time((graphics(500,400), fork([threadRed(P), threadGreen(P), threadBlue(P)]))).
```
