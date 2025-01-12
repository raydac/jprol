forall(Generator, Test) :- \\+ (Generator, \\+ Test).

append([], Zs, Zs).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

member(X,[X|_]).
member(X,[A|Rest]):-member(X,Rest).
