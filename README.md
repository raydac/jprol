![JPROL logo](art/github_logo.png)   
[![License Apache 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Maven central](https://maven-badges.herokuapp.com/maven-central/com.igormaznitsa/jprol-core/badge.svg)](http://search.maven.org/#artifactdetails|com.igormaznitsa|jprol-core|2.1.0|jar)
[![Java 1.8+](https://img.shields.io/badge/java-1.8%2b-green.svg)](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
[![PayPal donation](https://img.shields.io/badge/donation-PayPal-cyan.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AHWJHJFBAWGL2)
[![YooMoney donation](https://img.shields.io/badge/donation-Yoo.money-blue.svg)](https://yoomoney.ru/to/41001158080699)


# Introduction
I used to buy and collect books about computer tehcnololgies since my childhood and in 1990 I bought a book titled as "Prolog programming for Artificial Intelligence" by Ivan Bratko but didn't have enough time to read it for about 20 years. In 2009 I found some free time to read the book and was amazed by the power of the computer language. To learn it better I wrote small Java based Prolog embeddable engine and called it JProl.   

In 2014 [Yuki Katsura](http://iprolog.appstor.io/) made iPad version.

Initially the engine was a mono-block with embedded GUI part, but later it reworked and split solid-module into two modules: the engine and the GUI editor.

![GUIEditor](https://github.com/raydac/jprol/blob/master/jprolguieditor.png)   


# The engine

The engine is published in [the maven central](https://search.maven.org/artifact/com.igormaznitsa/jprol-core/2.1.0/jar) and can be used in maven projects as a dependency. It supports JDK 1.8+ and also I keep it compatible with Android API.   

```xml
<dependency>
    <groupId>com.igormaznitsa</groupId>
    <artifactId>jprol-core</artifactId>
    <version>2.1.0</version>
</dependency>
```   

It uses another project [java-prolog-parser](https://github.com/raydac/java-prolog-parser) which initially was a part of the JPprol engine but then it was extracted into autonomous module because sometime it is useful to have only prolog parser for data files. 

## Java method can play a predicate

The engine communicates with Java methods marked by special annotationas and having special signature.

For instance, below I show how implemented the `<</2` predicate of the core JProl library. As you can see, the method is just marked by `@JProlPredicate` annotation. It is imp[ossible just mark any method but only methods with special signature and arguments `JProlChoicePoint` and `TermStruct` because they provided during a call.

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

## Call the Engine from Java

Sample below shows how it is easy to inject a prolog based Eight Queens puzzle resolver into Java and get all solutions.

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

### Flags

The bootstrap library of the engine has two predicates `set_prolog_flag/2` and `current_prolog_flag/2`. They can be used to tune work of the engine or get extra info. Not all of them still supported.

- Flags:
    - __address_bit__ (read only) address size of the hosting machine. Typically 32 or 64.
    - __allow_variable_name_as_functor__ (read only)
    - __os.name__ (read only)
    - __os.arch__ (read only)
    - __bounded__ (read only) if true, integer representation is bound by min_integer and max_integer
    - __debug__ switch debugging mode true/false
    - __dialect__ (read only) fixed to `jprol`
    - __encoding__ default encoding used for opening files in text mode 
    - __gc__ (read only) if true (default), the garbage collector is active
    - __max_arity__ (read only) flag describing there is no maximum arity to compound terms
    - __max_integer__ (read only) maximum integer value if integers are bounded
    - __min_integer__ (read only) minimum integer value if integers are bounded
    - __cpu_count__ (read only) number of physical CPUs or cores in the system
    - __unknown__ determines the behaviour if an undefined procedure is encountered. If fail, the predicate fails silently. Allowed `error`,`fail` or `warning` by default `error`
    - __home__ (read only) notion of the user home directory
    - __verify__
    - __version_data__ (read only)


## Multi-threads

Prolog is a declarative language and it is well for multi-threading. I have added pair predicates to span new threads (`fork/1` and `async/1`), also the engine provides special predicate `waitasync/0` to make inter-process synchronization to wait completion of all spawned threads. The `waitasync/0` predicate can be used only from the main thread (the root goal). Also there is support for locks. You can create critical sections with `lock/1`, `unlock/1` and `trylock/1` predicates. Take a look at the prolog program below, it draws 3000000 color dots through three threads.
```Prolog
threadRed(P) :- for(_, 0, P), rnd(500, X), rnd(400, Y), lock(gfx), pencolor(red), dot(X, Y), unlock(gfx), fail.
    threadGreen(P) :- for(_, 0, P), rnd(500,X), rnd(400, Y), lock(gfx), pencolor(green), dot(X, Y), unlock(gfx), fail.
    threadBlue(P) :- for(_, 0, P), rnd(500, X), rnd(400, Y), lock(gfx), pencolor(blue), dot(X,Y), unlock(gfx), fail.

    ?- P = 1000000, time((graphics(500,400), fork([threadRed(P), threadGreen(P), threadBlue(P)]))).
```

## Example application

For demo I have written a small GUI application. It is a version of Conway's Life game where [all business rules for colony described in prolog part](/examples/java/jprol-example-life/src/main/java/com/igormaznitsa/jprol/example/life/LifeLibrary.java).   

![Conway's life with JProl](/art/jprol-example-life.png)   


# The GUI editor

For my prolog learning purposes I maded small GUI script editor which allows to execute prolog scripts and even make some graphics. It is not very strong in debugging but can be used for start of small scripts and test programs.

Its pre-built version for misc OS can be downloaded from [the release page](https://github.com/raydac/java-prolog-parser/releases/latest). It contains as standcalone versions as versions with embedded JDK.

## Prolog and Graphics
JProl GUI editor module provides predicates to work with graphics and save generated images. As an example, below you can find a program to draw a Koch snowflake fractal.
```Prolog
kochsnowflake(N,L) :- settitle('Koch snowflake'), brushcolor(white), graphics(300,300), pencolor(red), time((fractal(N,30,80,L,0,X1,Y1), fractal(N,X1,Y1,L,2.0943951024,X2,Y2), fractal(N,X2,Y2,L,4.1887902048,_,_))).

fractal(0,X,Y,L,A,XN,YN):- !, XN is X+L*cos(A), YN is Y+L*sin(A), XX is round(X),XXN is round(XN), YY is round(Y),YYN is round(YN), plot(XX,YY,XXN,YYN).
fractal(I,X,Y,L,A,XN,YN):- II is I-1, LL is L/3, A2 is A-1.0471975512, A3 is A+1.0471975512, fractal(II,X,Y,LL,A,X1,Y1), fractal(II,X1,Y1,LL,A2,X2,Y2), fractal(II,X2,Y2,LL,A3,X3,Y3), fractal(II,X3,Y3,LL,A,XN,YN).

?-kochsnowflake(5,300).
```
![KochSnowflake](https://github.com/raydac/jprol/blob/master/jprolgui.png)
