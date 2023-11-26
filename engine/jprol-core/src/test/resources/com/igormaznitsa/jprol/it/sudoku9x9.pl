% from https://github.com/melissamangos/sudoku

% Main predicate, calles other predicates
sudoku(R1C1, R1C2, R1C3, R1C4, R1C5, R1C6, R1C7, R1C8, R1C9,
     R2C1, R2C2, R2C3, R2C4, R2C5, R2C6, R2C7, R2C8, R2C9,
     R3C1, R3C2, R3C3, R3C4, R3C5, R3C6, R3C7, R3C8, R3C9,
     R4C1, R4C2, R4C3, R4C4, R4C5, R4C6, R4C7, R4C8, R4C9,
     R5C1, R5C2, R5C3, R5C4, R5C5, R5C6, R5C7, R5C8, R5C9,
     R6C1, R6C2, R6C3, R6C4, R6C5, R6C6, R6C7, R6C8, R6C9,
     R7C1, R7C2, R7C3, R7C4, R7C5, R7C6, R7C7, R7C8, R7C9,
     R8C1, R8C2, R8C3, R8C4, R8C5, R8C6, R8C7, R8C8, R8C9,
     R9C1, R9C2, R9C3, R9C4, R9C5, R9C6, R9C7, R9C8, R9C9) :-
     solved(R1C1, R1C2, R1C3, R1C4, R1C5, R1C6, R1C7, R1C8, R1C9,
     R2C1, R2C2, R2C3, R2C4, R2C5, R2C6, R2C7, R2C8, R2C9,
     R3C1, R3C2, R3C3, R3C4, R3C5, R3C6, R3C7, R3C8, R3C9,
     R4C1, R4C2, R4C3, R4C4, R4C5, R4C6, R4C7, R4C8, R4C9,
     R5C1, R5C2, R5C3, R5C4, R5C5, R5C6, R5C7, R5C8, R5C9,
     R6C1, R6C2, R6C3, R6C4, R6C5, R6C6, R6C7, R6C8, R6C9,
     R7C1, R7C2, R7C3, R7C4, R7C5, R7C6, R7C7, R7C8, R7C9,
     R8C1, R8C2, R8C3, R8C4, R8C5, R8C6, R8C7, R8C8, R8C9,
     R9C1, R9C2, R9C3, R9C4, R9C5, R9C6, R9C7, R9C8, R9C9),
     printsudoku(R1C1, R1C2, R1C3, R1C4, R1C5, R1C6, R1C7, R1C8, R1C9),
     printsudoku(R2C1, R2C2, R2C3, R2C4, R2C5, R2C6, R2C7, R2C8, R2C9),
     printsudoku(R3C1, R3C2, R3C3, R3C4, R3C5, R3C6, R3C7, R3C8, R3C9),
     printsudoku(R4C1, R4C2, R4C3, R4C4, R4C5, R4C6, R4C7, R4C8, R4C9),
     printsudoku(R5C1, R5C2, R5C3, R5C4, R5C5, R5C6, R5C7, R5C8, R5C9),
     printsudoku(R6C1, R6C2, R6C3, R6C4, R6C5, R6C6, R6C7, R6C8, R6C9),
     printsudoku(R7C1, R7C2, R7C3, R7C4, R7C5, R7C6, R7C7, R7C8, R7C9),
     printsudoku(R8C1, R8C2, R8C3, R8C4, R8C5, R8C6, R8C7, R8C8, R8C9),
     printsudoku(R9C1, R9C2, R9C3, R9C4, R9C5, R9C6, R9C7, R9C8, R9C9).

% Helps to print the values
printsudoku(A, B, C, D, E, F, G, H, I) :- write(' '), write(A), write('  '), write(B), write('  '), write(C),
write('  '), write(D), write('  '), write(E), write('  '), write(F),
write('  '), write(G), write('  '), write(H), write('  '), write(I), nl.

% Solves the puzzle by making sure the values are all different
solved(R1C1, R1C2, R1C3, R1C4, R1C5, R1C6, R1C7, R1C8, R1C9,
     R2C1, R2C2, R2C3, R2C4, R2C5, R2C6, R2C7, R2C8, R2C9,
     R3C1, R3C2, R3C3, R3C4, R3C5, R3C6, R3C7, R3C8, R3C9,
     R4C1, R4C2, R4C3, R4C4, R4C5, R4C6, R4C7, R4C8, R4C9,
     R5C1, R5C2, R5C3, R5C4, R5C5, R5C6, R5C7, R5C8, R5C9,
     R6C1, R6C2, R6C3, R6C4, R6C5, R6C6, R6C7, R6C8, R6C9,
     R7C1, R7C2, R7C3, R7C4, R7C5, R7C6, R7C7, R7C8, R7C9,
     R8C1, R8C2, R8C3, R8C4, R8C5, R8C6, R8C7, R8C8, R8C9,
     R9C1, R9C2, R9C3, R9C4, R9C5, R9C6, R9C7, R9C8, R9C9 ):-
     different(R1C1, R1C2, R1C3, R1C4, R1C5, R1C6, R1C7, R1C8, R1C9), % First row
     different(R2C1, R2C2, R2C3, R2C4, R2C5, R2C6, R2C7, R2C8, R2C9), % Second row
     different(R3C1, R3C2, R3C3, R3C4, R3C5, R3C6, R3C7, R3C8, R3C9), % Third row
     different(R4C1, R4C2, R4C3, R4C4, R4C5, R4C6, R4C7, R4C8, R4C9), % Fourth row
     different(R5C1, R5C2, R5C3, R5C4, R5C5, R5C6, R5C7, R5C8, R5C9), % Fifth row
     different(R6C1, R6C2, R6C3, R6C4, R6C5, R6C6, R6C7, R6C8, R6C9), % Sixth row
     different(R7C1, R7C2, R7C3, R7C4, R7C5, R7C6, R7C7, R7C8, R7C9), % Seventh row
     different(R8C1, R8C2, R8C3, R8C4, R8C5, R8C6, R8C7, R8C8, R8C9), % Eighth row
     different(R9C1, R9C2, R9C3, R9C4, R9C5, R9C6, R9C7, R9C8, R9C9), % Nineth row
     different(R1C1, R2C1, R3C1, R4C1, R5C1, R6C1, R7C1, R8C1, R9C1), % First column
     different(R1C2, R2C2, R3C2, R4C2, R5C2, R6C2, R7C2, R8C2, R9C2), % Second column
     different(R1C3, R2C3, R3C3, R4C3, R5C3, R6C3, R7C3, R8C3, R9C3), % Third column
     different(R1C4, R2C4, R3C4, R4C4, R5C4, R6C4, R7C4, R8C4, R9C4), % Fourth column
     different(R1C5, R2C5, R3C5, R4C5, R5C5, R6C5, R7C5, R8C5, R9C5), % Fifth column
     different(R1C6, R2C6, R3C6, R4C6, R5C6, R6C6, R7C6, R8C6, R9C6), % Sixth column
     different(R1C7, R2C7, R3C7, R4C7, R5C7, R6C7, R7C7, R8C7, R9C7), % Seventh column
     different(R1C8, R2C8, R3C8, R4C8, R5C8, R6C8, R7C8, R8C8, R9C8), % Eighth column
     different(R1C9, R2C9, R3C9, R4C9, R5C9, R6C9, R7C9, R8C9, R9C9), % Nineth column
     different(R1C1, R1C2, R1C3, R2C1, R2C2, R2C3, R3C1, R3C2, R3C3), % Top Left block
     different(R1C4, R1C5, R1C6, R2C4, R2C5, R2C6, R3C4, R3C5, R3C6), % Top Middle block
     different(R1C7, R1C8, R1C9, R2C7, R2C8, R2C9, R3C7, R3C8, R3C9), % Top Right block
     different(R4C1, R4C2, R4C3, R5C1, R5C2, R5C3, R6C1, R6C2, R6C3), % Left block
     different(R4C4, R4C5, R4C6, R5C4, R5C5, R5C6, R6C4, R6C5, R6C6), % Middle block
     different(R4C7, R4C8, R4C9, R5C7, R5C8, R5C9, R6C7, R6C8, R6C9), % Right block
     different(R7C1, R7C2, R7C3, R8C1, R8C2, R8C3, R9C1, R9C2, R9C3), % Bottom Left block
     different(R7C4, R7C5, R7C6, R8C4, R8C5, R8C6, R9C4, R9C5, R9C6), % Bottom Middle block
     different(R7C7, R7C8, R7C9, R8C7, R8C8, R8C9, R9C7, R9C8, R9C9). % Bottom Right block

% Determine if each square in either a row, column, or block are
% different
different(A, B, C, D, E, F, G, H, I) :- num(A), num(B), num(C), num(D), num(E), num(F), num(G), num(H), num(I),
A\=B, A\=C, A\=D, A\=E, A\=F, A\=G, A\=H, A\=I,
B\=C, B\=D, B\=E, B\=F, B\=G, B\=H, B\=I,
C\=D, C\=E, C\=F, C\=G, C\=H, C\=I,
D\=E, D\=F, D\=G, D\=H, D\=I,
E\=F, E\=G, E\=H, E\=I,
F\=G, F\=H, F\=I,
G\=H, G\=I,
H\=I.

% Initialize numbers
num(1). num(2). num(3). num(4). num(5). num(6). num(7). num(8). num(9).
