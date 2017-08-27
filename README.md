[![License Apache 2.0](https://img.shields.io/badge/license-Apache%20License%202.0-green.svg)](http://www.apache.org/licenses/LICENSE-2.0)
[![Java 6.0+](https://img.shields.io/badge/java-6.0%2b-green.svg)](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
[![PayPal donation](https://img.shields.io/badge/donation-PayPal-red.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=AHWJHJFBAWGL2)
[![Yandex.Money donation](https://img.shields.io/badge/donation-Я.деньги-yellow.svg)](http://yasobe.ru/na/iamoss)

# Introduction
I bought a book of Ivan Bratko titled as "Prolog Programming for Artificial Intelligence" in 1990 but didn't have enough time to read it so that the book was placed into a shelf and forgotten for years till 2009. In 2009 I had read the book and was so impressed by the power of computer language that to learn it better I developed small Java based Prolog engine and called it JProl.   

JProl is a small embeddable monolitic (one uberjar) Prolog engine developed in 2010 with number of libraries out of the box. It supports Edinburgh Prolog style and has a small GUI editor which is very useful for experiments. The Engine is not very optimal one and I would not recommend to use it in any production, it is good only for learning and teaching purposes.   

In 2014 I was asked by [Yuki Katsura](http://iprolog.appstor.io/) to open sources of the engine (he then converted it into iPad version) and the project was published as OSS under Apache License 2.0

# Prebuilt versions
Prebuilt versions provided for main OSes (I can test only for Linux, so that notify me about issues under Windows and MacOS). [Executable files can be downloaded from the latest release page](https://github.com/raydac/jprol/releases/latest)

# Embedded GUI editor
The Engine has very small end restricted embedded GUI editor to create, edit and execute one Prolog program.
![GUIEditor](https://github.com/raydac/jprol/blob/master/jprolguieditor.png)

# How to define a predicate in Java
The Engine was also used as proof-of-concept of usage Java methods as predicates and it allows to mark methods by special annotations and call them from Prolog.
For instance, I am going to show you how SORT/2 predicate has been implemented in the core JProl library. As you can see below, the method is just marked by @Predicate annotation. Of course it is impossible just mark any method but the method must process specific Prolog arguments : Goal and TermStruct.
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
If you want to define more than one operator in your library, you should use ProlOperators annotation for your library class.
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
Below you can see the example shows calls to the Engine to solve the well known puzzle "Eight queens". It describes task in Prolog, solve it and print all found solutions. It is good example how Knowledge base can be defined just through annotations directly in Java code.
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
JProl GUI editor provides some predicates to draw graphics and save images, as example of Graphics calls you can take a look at Prolog program to draw a Koch snowflake fractal.
```Prolog
kochsnowflake(N,L) :- settitle('Koch snowflake'), brushcolor(white), graphics(300,300), pencolor(red), time((fractal(N,30,80,L,0,X1,Y1), fractal(N,X1,Y1,L,2.0943951024,X2,Y2), fractal(N,X2,Y2,L,4.1887902048,_,_))).

fractal(0,X,Y,L,A,XN,YN):- !, XN is X+L*cos(A), YN is Y+L*sin(A), XX is round(X),XXN is round(XN), YY is round(Y),YYN is round(YN), plot(XX,YY,XXN,YYN).
fractal(I,X,Y,L,A,XN,YN):- II is I-1, LL is L/3, A2 is A-1.0471975512, A3 is A+1.0471975512, fractal(II,X,Y,LL,A,X1,Y1), fractal(II,X1,Y1,LL,A2,X2,Y2), fractal(II,X2,Y2,LL,A3,X3,Y3), fractal(II,X3,Y3,LL,A,XN,YN).

?-kochsnowflake(5,300).
```
![KochSnowflake](https://github.com/raydac/jprol/blob/master/jprolgui.png)

# Multi-threading
It was interesting for me to implement and check support of multi-threading in the engine. The engine allows to use pair predicates to span new threads `fork/1` and `async/1`, also it has special predicate `waitasync/0` for synchronization you can wait the end of all executing spawned threads (by the main thread). `waitasync/0` predicate must be used only from the main thread (I mean the root goal). Also there are lockers which allow to create critical sections with `lock/1`, `unlock/1` and `trylock/1` predicates. Take a look at the example of their usage below, I have shown an application which draws 3000000 color dots on the graphic screen (provided by embedded GUI library), each thread paints its own color dots in its own thread.
```Prolog
threadRed(P) :- for(_, 0, P), rnd(500, X), rnd(400, Y), lock(gfx), pencolor(red), dot(X, Y), unlock(gfx), fail.
    threadGreen(P) :- for(_, 0, P), rnd(500,X), rnd(400, Y), lock(gfx), pencolor(green), dot(X, Y), unlock(gfx), fail.
    threadBlue(P) :- for(_, 0, P), rnd(500, X), rnd(400, Y), lock(gfx), pencolor(blue), dot(X,Y), unlock(gfx), fail.

    ?- P = 1000000, time((graphics(500,400), fork([threadRed(P), threadGreen(P), threadBlue(P)]))).
```
