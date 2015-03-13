Introduction
=============
I bought a book of Ivan Bratko titled as "Prolog Programming for Artificial Intelligence" in 1990 but didn't have time to read that thus the book was placed into my shelf and forgotten for years till 2009. In 2009 I had read the book and was so impressed by the power of Prolog that to learn it better I decided to develop a small Prolog engine to be used with Java and the PROL was born.

PROL is a small embeddable monolitic (one uberjar) Prolog engine developed in 2010 with number of libraries on the board. It supports Edinburgh Prolog style and has a small editor working out of the box for experiments. it has been covered tests for about 38% so that not recommended to be used in production but for research only. I didn't spend much time to optimize it so that Einsten's puzzle it resolves in 2 seconds.

In 2014 I was asked by Yuki Katsura to open sources and now I have opened them as an OSS project under Apache License 2.0

License
========
The Engine is under [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0)

Graphics User Interface
========================
![Screenshot](https://github.com/raydac/jprol/blob/master/screenshotprolpad.jpg)

Prebuilt versions
==================
Prebuilt versions of the Windows executable file (based on Launch4j) and the JAR version you can find in [my Google drive folder](https://googledrive.com/host/0BxHnNp97IgMRSEN0VDFCMGYtZkk/)

How to use from Java
=====================
## Technical requirements
The Engine needs Java version 1.6+. For development I use NetBeans? 7.4-8.0 and all GUI forms developed in NetBeans? editor. The Engine actively uses stack but very very deep recursion can make troubles and even crash or hang JVM!

## Start in GUI mode
To start the GUI editor you can use below CLI commands
```
   java -jar jprol-1.1.3-SNAPSHOT.jar
```
or
```
   java -cp jprol-1.1.3-SNAPSHOT.jar com.igormaznitsa.prol.easygui.main
```
## How to define a predicate in Java
It is very easy to define new predicates in Java. For instance, I am going to show you how a SORT/2 predicate has been implemented in the core Prol library. As you can see below, you should just place @Predicate annotation for a method which implements the functionality. Of course the method must be in the special format and it must have two arguments Goal and TermStruct.
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
## How to define new operators in a library
If you want to define more than one operator in your library, you should use the ProlOperators annotation which should be placed for your library class.
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
## Call the engine from Java
Below you can see the example shows how to use the Prol engine to solve the well known puzzle "Eight queens". It describes task, solve that and print all found solutions. It is a good example that the knowledge base can be defined through annotations just in Java code.
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
## Multithreading
It was interesting for me to implement support of multithreading in the engine to take a look how it is compatible with Prolog and it looks nice and much easy than in Java. The engine allows to use pair predicates to span new threads fork/1 and async/1, also it has special predicate waitasync/0 for synchronization you can wait the end of all executing spawned threads (by the main thread). The waitasync/0 predicate must be used only from the main thread (I mean the root goal). Also there are lockers which allow to create critical sections with lock/1, unlock/1 and trylock/1 predicates. Take a look at the example of their usage below, I have shown an application which draws 3000000 color dots on the graphic screen (provided by embedded GUI library), each thread paints its own color dots in its own thread. 
```Prolog
threadRed(P) :- for(_, 0, P), rnd(500, X), rnd(400, Y), lock(gfx), pencolor(red), dot(X, Y), unlock(gfx), fail.
    threadGreen(P) :- for(_, 0, P), rnd(500,X), rnd(400, Y), lock(gfx), pencolor(green), dot(X, Y), unlock(gfx), fail.
    threadBlue(P) :- for(_, 0, P), rnd(500, X), rnd(400, Y), lock(gfx), pencolor(blue), dot(X,Y), unlock(gfx), fail.

    ?- P = 1000000, time((graphics(500,400), fork([threadRed(P), threadGreen(P), threadBlue(P)]))).
```
End word
=========
I am very impressed by power of Prolog, I think it is one of the greatest computer languages (may be it is the best computer language at all) and the language makes a programmer to write really thinking software. Please keep in your mind that PROL is an Academic project and it was not developed to be used in production but it helps make some experiments and learn Prolog. 
