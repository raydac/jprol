[![JPROL logo](art/gihub_logo.png)]   
[![License Apache 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Java 1.8+](https://img.shields.io/badge/java-1.8%2b-green.svg)](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
[![PayPal donation](https://img.shields.io/badge/donation-PayPal-red.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AHWJHJFBAWGL2)
[![Yandex.Money donation](https://img.shields.io/badge/donation-Я.деньги-yellow.svg)](http://yasobe.ru/na/iamoss)

# Introduction
In 1990 (it was the USSR), I bought a book titled as "Prolog Programming for Artificial Intelligence" (written by Ivan Bratko) but didn't have enough time to read for about 20 years. In 2009 I got enough free time to read the book and was amazed by the power of the computer language. To learn it better I wrote small Java based Prolog embeddable engine and called it JProl.   

JProl is a small Prolog engine developed with several libraries provided out of the box. It supports Edinburgh Prolog style and its GUI version provides way to learn Prolog and develop interactive programs and experiments. The engine is written in not very optimal way and I can't recommend it to use in hi-performance or very complex cases but if you need to resolve easy learning purposes or implementation small Prolog-based DSL solutions then it can help.   

In 2014 I was asked by [Yuki Katsura](http://iprolog.appstor.io/) to open sources of the engine (he made iPad version) and the project was published as OSS under Apache License 2.0

# Use in maven projects
Since 2020, the engine core has been published in the maven central and can be easily injected into maven projects through code snippet
```xml
<dependency>
    <groupId>com.igormaznitsa</groupId>
    <artifactId>jprol-core</artifactId>
    <version>2.0.0</version>
</dependency>
```

# Prebuilt versions
There are published pre-built versions for main OSes (I can test it only for Linux, so that notify me about issues under Windows and MacOS). [Executable files can be downloaded from the latest release page](https://github.com/raydac/jprol/releases/latest)

# Embedded GUI editor
The engine has very small end restricted embedded GUI editor to create, edit and execute one Prolog program.
![GUIEditor](https://github.com/raydac/jprol/blob/master/jprolguieditor.png)

# How to define a predicate in Java
It allows communicate with Java methods and use them as predicates through special annotations.
For instance, below I show how implemented the SORT/2 predicate of the core JProl library. As you can see, the method is just marked by `@Predicate` annotation. Of course it is impossible to mark method because the method had to have specific arguments: `Goal` and `TermStruct`.
```Java
@Predicate(Signature = "sort/2", Template = {"+list,?list"},Reference="True if Sorted can be unified with a list holding the elements  of List, sorted to the standard order of terms")
    @Determined
    public static final boolean predicateSORT(final Goal goal, final TermStruct predicate) {
        final Term nonsorted = Utils.getTermFromElement(predicate.getElement(0));
        final Term sorted = Utils.getTermFromElement(predicate.getElement(1));
        final Term[] bufferarray = Utils.listToArray((TermList) nonsorted);
        Arrays.sort(bufferarray, Utils.TERM_COMPARATOR);
        final TermList sortedList = Utils.arrayToList(bufferarray);
        return sorted.Equ(sortedList);
    }
```
# How to define new operators in library
There is way to define more than one operator in your library, for such purposes you should use the `@ProlOperators` annotation.
```Java
@ProlOperators(Operators = {
    @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "is"),
    @ProlOperator(Priority = 700, Type = Operator.OPTYPE_XFX, Name = "="),
    @ProlOperator(Priority = 1000, Type = Operator.OPTYPE_XFY, Name = ","),
    @ProlOperator(Priority = 1050, Type = Operator.OPTYPE_XFY, Name = "->"),
....
    @ProlOperator(Priority = 300, Type = Operator.OPTYPE_XFX, Name = "mod"),
    @ProlOperator(Priority = 200, Type = Operator.OPTYPE_FY, Name = "\\"),
    @ProlOperator(Priority = 200, Type = Operator.OPTYPE_XFX, Name = "**")
})
public class ProlCoreLibrary extends ProlAbstractLibrary {
...
```
# Call the Engine from Java
You can see example how to call make call to newly created engine and resolve a "Eight queens" puzzle. The example shows how to create a context, provide a task and get all possible solutions. Also it is good example to show how whole knowledge base can be provided directly just through only annotations.
```Java
final ProlContext context = new ProlContext("test",DefaultProlStreamManagerImpl.getInstance());
           final ProlConsult consult = new ProlConsult("solution([]). solution([X/Y|Others]):-solution(Others),member(Y,[1,2,3,4,5,6,7,8]),notattack(X/Y,Others). notattack(_,[]). notattack(X/Y,[X1/Y1 | Others]):- Y=\\=Y1, Y1-Y=\\=X1-X, Y1-Y=\\=X-X1, notattack(X/Y,Others). member(Item,[Item|Rest]). member(Item,[First|Rest]):-member(Item,Rest). template([1/Y1,2/Y2,3/Y3,4/Y4,5/Y5,6/Y6,7/Y7,8/Y8]).", context);
            consult.consult();

            final Goal goal = new Goal("template(X),solution(X).", context);
            int combinatioCounter = 0;

            while (true) {
                final Term result = goal.solve();
                if (result == null) {
                    break;
                }
                Utils.printTermState(result);
            }
```

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
