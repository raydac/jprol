package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

@Timeout(30)
class LogicalTasksTest extends AbstractJProlTest {

  private static final String RIVER_PROGRAM =
      "opp(left, right) :- !."
          + "opp(right, left)."
          + "step(s(F, W, G, C), alone, s(F2, W, G, C)) :- opp(F, F2)."
          + "step(s(F, F, G, C), wolf, s(F2, F2, G, C)) :- opp(F, F2)."
          + "step(s(F, W, F, C), goat, s(F2, W, F2, C)) :- opp(F, F2)."
          + "step(s(F, W, G, F), cabbage, s(F2, W, G, F2)) :- opp(F, F2)."
          + "safe(s(F, W, G, C)) :-"
          + "  (W == G -> F == W ; true),"
          + "  (G == C -> F == G ; true)."
          + "search(s(right, right, right, right), _, Moves, Moves)."
          + "search(State, Seen, Acc, Moves) :-"
          + "  step(State, Move, Next),"
          + "  safe(Next),"
          + "  \\+ member(Next, Seen),"
          + "  search(Next, [Next|Seen], [Move|Acc], Moves)."
          + "replay(s(right, right, right, right), [])."
          + "replay(State, [Move|Moves]) :-"
          + "  step(State, Move, Next),"
          + "  safe(Next),"
          + "  replay(Next, Moves)."
          + "replay_all([])."
          + "replay_all([Path|Rest]) :-"
          + "  replay(s(left, left, left, left), Path),"
          + "  replay_all(Rest)."
          + "crossings(Paths) :-"
          + "  Start = s(left, left, left, left),"
          +
          "  findall(Moves, (search(Start, [Start], [], Rev), reverse(Rev, Moves), length(Moves, 7)), Found),"
          + "  sort(Found, Paths),"
          + "  replay_all(Paths).";

  private static final String TIC_TAC_TOE_PROGRAM =
      "line([1, 2, 3]). line([4, 5, 6]). line([7, 8, 9])."
          + "line([1, 4, 7]). line([2, 5, 8]). line([3, 6, 9])."
          + "line([1, 5, 9]). line([3, 5, 7])."
          + "cell(1, [H|_], H) :- !."
          + "cell(N, [_|T], X) :- N > 1, N1 is N - 1, cell(N1, T, X)."
          + "place(1, [_|T], P, [P|T]) :- !."
          + "place(N, [H|T], P, [H|R]) :- N > 1, N1 is N - 1, place(N1, T, P, R)."
          + "empties(Board, Cells) :- empties(Board, 1, Cells)."
          + "empties([], _, [])."
          + "empties([e|T], I, [I|Rest]) :- !, I1 is I + 1, empties(T, I1, Rest)."
          + "empties([_|T], I, Rest) :- I1 is I + 1, empties(T, I1, Rest)."
          + "winner(Board, P) :-"
          + "  line([A, B, C]),"
          + "  cell(A, Board, P), cell(B, Board, P), cell(C, Board, P),"
          + "  P \\== e, !."
          + "score(x, 1). score(o, -1)."
          + "best(Board, Move, Score) :-"
          + "  Low is -2, High is 2,"
          + "  max_search(Board, Low, High, Move, Score)."
          + "max_search(Board, _, _, none, Score) :- winner(Board, W), !, score(W, Score)."
          + "max_search(Board, _, _, none, 0) :- \\+ member(e, Board), !."
          + "max_search(Board, Alpha, Beta, Move, Score) :-"
          + "  empties(Board, Cells),"
          + "  Worst is -2,"
          + "  max_loop(Cells, Board, Alpha, Beta, none, Worst, Move, Score)."
          + "max_loop([], _, _, _, Move, Score, Move, Score) :- !."
          + "max_loop([C|Cs], Board, Alpha, Beta, BM, BS, Move, Score) :-"
          + "  place(C, Board, x, Next),"
          + "  min_search(Next, Alpha, Beta, _, CS),"
          + "  (CS > BS -> BM1 = C, BS1 = CS ; BM1 = BM, BS1 = BS),"
          + "  (Alpha >= BS1 -> Alpha1 = Alpha ; Alpha1 = BS1),"
          + "  (Alpha1 >= Beta -> Move = BM1, Score = BS1"
          + "  ; max_loop(Cs, Board, Alpha1, Beta, BM1, BS1, Move, Score))."
          + "min_search(Board, _, _, none, Score) :- winner(Board, W), !, score(W, Score)."
          + "min_search(Board, _, _, none, 0) :- \\+ member(e, Board), !."
          + "min_search(Board, Alpha, Beta, Move, Score) :-"
          + "  empties(Board, Cells),"
          + "  Worst is 2,"
          + "  min_loop(Cells, Board, Alpha, Beta, none, Worst, Move, Score)."
          + "min_loop([], _, _, _, Move, Score, Move, Score) :- !."
          + "min_loop([C|Cs], Board, Alpha, Beta, BM, BS, Move, Score) :-"
          + "  place(C, Board, o, Next),"
          + "  max_search(Next, Alpha, Beta, _, CS),"
          + "  (CS < BS -> BM1 = C, BS1 = CS ; BM1 = BM, BS1 = BS),"
          + "  (Beta =< BS1 -> Beta1 = Beta ; Beta1 = BS1),"
          + "  (Alpha >= Beta1 -> Move = BM1, Score = BS1"
          + "  ; min_loop(Cs, Board, Alpha, Beta1, BM1, BS1, Move, Score))."
          + "played(Board, R) :- best(Board, Move, Score), R = Move - Score.";

  private static final String KNAPSACK_PROGRAM =
      "catalog([item(a, 6, 2), item(b, 10, 4), item(c, 12, 6), item(d, 7, 3), item(e, 4, 1)])."
          + "pack([], _, [], 0)."
          + "pack([item(N, V, W)|Rest], Cap, [N|Chosen], Total) :-"
          + "  W =< Cap,"
          + "  Left is Cap - W,"
          + "  pack(Rest, Left, Chosen, RestV),"
          + "  Total is RestV + V."
          + "pack([_|Rest], Cap, Chosen, Total) :- pack(Rest, Cap, Chosen, Total)."
          + "max_pack([pack(V, C)], V, C) :- !."
          + "max_pack([pack(V, C)|Rest], BestV, BestC) :-"
          + "  max_pack(Rest, RestV, RestC),"
          + "  (V > RestV -> BestV = V, BestC = C ; BestV = RestV, BestC = RestC)."
          + "value_of([], _, 0)."
          + "value_of([N|Ns], Items, Total) :-"
          + "  item_spec(N, Items, V, _),"
          + "  value_of(Ns, Items, Rest),"
          + "  Total is Rest + V."
          + "weight_of([], _, 0)."
          + "weight_of([N|Ns], Items, Total) :-"
          + "  item_spec(N, Items, _, W),"
          + "  weight_of(Ns, Items, Rest),"
          + "  Total is Rest + W."
          + "item_spec(N, [item(N, V, W)|_], V, W) :- !."
          + "item_spec(N, [_|Rest], V, W) :- item_spec(N, Rest, V, W)."
          + "best(Cap, Chosen, Value) :-"
          + "  catalog(Items),"
          + "  findall(pack(V, C), pack(Items, Cap, C, V), Packs),"
          + "  max_pack(Packs, Value, Chosen),"
          + "  value_of(Chosen, Items, Value),"
          + "  weight_of(Chosen, Items, W),"
          + "  W =< Cap."
          + "loaded(Cap, R) :- best(Cap, Chosen, Value), R = Chosen - Value.";

  private static final String MST_PROGRAM =
      "link(a, b, 1). link(b, c, 2). link(a, c, 4)."
          + "link(c, d, 3). link(b, d, 6). link(d, e, 5). link(c, e, 7)."
          + "nodes([a, b, c, d, e])."
          + "singletons([], [])."
          + "singletons([N|Ns], [[N]|Sets]) :- singletons(Ns, Sets)."
          + "find_set([Set|Rest], Node, Found) :-"
          + "  (member(Node, Set) -> Found = Set ; find_set(Rest, Node, Found))."
          + "drop_set([Set|Rest], Set, Rest) :- !."
          + "drop_set([Other|Rest], Set, [Other|Out]) :- drop_set(Rest, Set, Out)."
          + "sum_w([], 0)."
          + "sum_w([edge(_, _, W)|Rest], Total) :- sum_w(Rest, RestW), Total is RestW + W."
          + "kruskal([], _, Acc, Cost, Tree, Cost) :- reverse(Acc, Tree)."
          + "kruskal([e(W, A, B)|Es], Sets, Acc, Cost0, Tree, Cost) :-"
          + "  find_set(Sets, A, SA),"
          + "  find_set(Sets, B, SB),"
          + "  (SA == SB ->"
          + "    kruskal(Es, Sets, Acc, Cost0, Tree, Cost)"
          + "  ; drop_set(Sets, SA, S1),"
          + "    drop_set(S1, SB, S2),"
          + "    append(SA, SB, Joined),"
          + "    Cost1 is Cost0 + W,"
          + "    kruskal(Es, [Joined|S2], [edge(A, B, W)|Acc], Cost1, Tree, Cost)"
          + "  )."
          + "mst(Tree, Cost) :-"
          + "  findall(e(W, A, B), link(A, B, W), Edges),"
          + "  msort(Edges, Sorted),"
          + "  nodes(Nodes),"
          + "  singletons(Nodes, Sets),"
          + "  kruskal(Sorted, Sets, [], 0, Tree, Cost),"
          + "  sum_w(Tree, Cost)."
          + "spanning(R) :- mst(Tree, Cost), R = Tree - Cost.";

  private static final String UNIFY_PROGRAM =
      "deref(vr(N), S, T) :- member(bind(N, V), S), !, deref(V, S, T)."
          + "deref(T, _, T)."
          + "occurs(N, vr(N), _) :- !."
          + "occurs(N, vr(M), S) :- member(bind(M, V), S), !, occurs(N, V, S)."
          + "occurs(N, fun(_, Args), S) :- member(Arg, Args), occurs(N, Arg, S), !."
          + "unify(A, B, S0, S) :- deref(A, S0, DA), deref(B, S0, DB), unify_d(DA, DB, S0, S)."
          + "unify_d(vr(N), vr(N), S, S) :- !."
          + "unify_d(vr(N), T, S, [bind(N, T)|S]) :- \\+ occurs(N, T, S), !."
          + "unify_d(T, vr(N), S0, S) :- !, unify_d(vr(N), T, S0, S)."
          + "unify_d(atm(A), atm(A), S, S) :- !."
          + "unify_d(fun(F, As), fun(F, Bs), S0, S) :- !, unify_args(As, Bs, S0, S)."
          + "unify_args([], [], S, S) :- !."
          + "unify_args([A|As], [B|Bs], S0, S) :-"
          + "  unify(A, B, S0, S1),"
          + "  unify_args(As, Bs, S1, S)."
          + "apply_term(vr(N), S, T) :- member(bind(N, V), S), !, apply_term(V, S, T)."
          + "apply_term(vr(N), _, vr(N)) :- !."
          + "apply_term(atm(A), _, atm(A)) :- !."
          + "apply_term(fun(F, Args), S, fun(F, Out)) :- apply_args(Args, S, Out)."
          + "apply_args([], _, [])."
          + "apply_args([A|As], S, [B|Bs]) :- apply_term(A, S, B), apply_args(As, S, Bs)."
          + "unified(R) :-"
          + "  unify(fun(f, [vr(x), atm(a)]), fun(f, [atm(b), vr(y)]), [], S1),"
          + "  apply_term(fun(f, [vr(x), vr(y)]), S1, Left),"
          + "  unify(fun(p, [vr(x), vr(y), vr(x)]), fun(p, [vr(y), atm(a), atm(a)]), [], S2),"
          + "  apply_term(vr(x), S2, Right),"
          + "  R = Left - Right.";

  private static final String MARRIAGE_PROGRAM =
      "pref(m1, [w1, w2, w3])."
          + "pref(m2, [w2, w1, w3])."
          + "pref(m3, [w1, w2, w3])."
          + "pref(w1, [m2, m1, m3])."
          + "pref(w2, [m1, m2, m3])."
          + "pref(w3, [m1, m2, m3])."
          + "ahead(Better, Worse, [Better|Rest]) :- member(Worse, Rest), !."
          + "ahead(Better, Worse, [_|Rest]) :- ahead(Better, Worse, Rest)."
          + "prefers(Person, Better, Worse) :- pref(Person, Order), ahead(Better, Worse, Order)."
          + "engaged([match(W, M)|_], W, M) :- !."
          + "engaged([_|Rest], W, M) :- engaged(Rest, W, M)."
          + "drop_match([match(W, _)|Rest], W, Rest) :- !."
          + "drop_match([H|Rest], W, [H|Out]) :- drop_match(Rest, W, Out)."
          + "take_hold([hold(M, Left)|Rest], M, Left, Rest) :- !."
          + "take_hold([H|Rest], M, Left, [H|Out]) :- take_hold(Rest, M, Left, Out)."
          + "gs([], _, Eng, Eng) :- !."
          + "gs([prop(_, [])|_], _, _, _) :- !, fail."
          + "gs([prop(M, [W|Ws])|Rest], Waiting, Eng, Out) :-"
          + "  (engaged(Eng, W, Current) ->"
          + "    (prefers(W, M, Current) ->"
          + "      take_hold(Waiting, Current, Left, Waiting1),"
          + "      drop_match(Eng, W, Eng1),"
          + "      gs([prop(Current, Left)|Rest], [hold(M, Ws)|Waiting1], [match(W, M)|Eng1], Out)"
          + "    ; gs([prop(M, Ws)|Rest], Waiting, Eng, Out))"
          + "  ; gs(Rest, [hold(M, Ws)|Waiting], [match(W, M)|Eng], Out))."
          + "blocking(Matches) :-"
          + "  member(match(W, M), Matches),"
          + "  member(match(W2, M2), Matches),"
          + "  M \\== M2,"
          + "  prefers(M, W2, W),"
          + "  prefers(W2, M, M2)."
          + "married(Sorted) :-"
          + "  pref(m1, P1), pref(m2, P2), pref(m3, P3),"
          + "  gs([prop(m1, P1), prop(m2, P2), prop(m3, P3)], [], [], Raw),"
          + "  sort(Raw, Sorted),"
          + "  \\+ blocking(Sorted).";

  private static final String QUEEN_ENDGAME_PROGRAM =
      "dir(-1, -1). dir(-1, 0). dir(-1, 1)."
          + "dir(0, -1). dir(0, 1)."
          + "dir(1, -1). dir(1, 0). dir(1, 1)."
          + "near(F1, R1, F2, R2) :-"
          + "  Df is abs(F1 - F2), Dr is abs(R1 - R2),"
          + "  Df =< 1, Dr =< 1, Df + Dr > 0."
          + "ray(F, R, DF, DR, B1f, B1r, B2f, B2r, Tf, Tr) :-"
          + "  F1 is F + DF, R1 is R + DR,"
          + "  ray_at(F1, R1, DF, DR, B1f, B1r, B2f, B2r, Tf, Tr)."
          + "ray_at(F, R, _, _, _, _, _, _, _, _) :-"
          + "  (F < 1 ; F > 8 ; R < 1 ; R > 8), !, fail."
          + "ray_at(F, R, _, _, B1f, B1r, B2f, B2r, _, _) :-"
          + "  ((F =:= B1f, R =:= B1r) ; (F =:= B2f, R =:= B2r)), !, fail."
          + "ray_at(F, R, _, _, _, _, _, _, F, R)."
          + "ray_at(F, R, DF, DR, B1f, B1r, B2f, B2r, Tf, Tr) :-"
          + "  F1 is F + DF, R1 is R + DR,"
          + "  ray_at(F1, R1, DF, DR, B1f, B1r, B2f, B2r, Tf, Tr)."
          + "queen_attacks(Qf, Qr, Tf, Tr, B1f, B1r, B2f, B2r) :-"
          + "  Qf >= 1,"
          + "  dir(DF, DR),"
          + "  ray(Qf, Qr, DF, DR, B1f, B1r, B2f, B2r, Tf, Tr)."
          + "checked(Kf, Kr, Qf, Qr, Bf, Br) :- near(Bf, Br, Kf, Kr), !."
          + "checked(Kf, Kr, Qf, Qr, Bf, Br) :-"
          + "  queen_attacks(Qf, Qr, Bf, Br, Kf, Kr, 0, 0)."
          + "w_king(Kf, Kr, Qf, Qr, Bf, Br, Nf, Nr) :-"
          + "  dir(DF, DR),"
          + "  Nf is Kf + DF, Nr is Kr + DR,"
          + "  Nf >= 1, Nf =< 8, Nr >= 1, Nr =< 8,"
          + "  \\+ (Nf =:= Qf, Nr =:= Qr),"
          + "  \\+ (Nf =:= Bf, Nr =:= Br),"
          + "  \\+ near(Nf, Nr, Bf, Br)."
          + "w_queen(Kf, Kr, Qf, Qr, Bf, Br, Nf, Nr) :-"
          + "  queen_attacks(Qf, Qr, Nf, Nr, Kf, Kr, Bf, Br)."
          + "capture(Nf, Nr, Qf, Qr, Kf, Kr, 0, 0) :-"
          + "  Qf >= 1, Nf =:= Qf, Nr =:= Qr, !,"
          + "  \\+ near(Qf, Qr, Kf, Kr)."
          + "capture(_, _, Qf, Qr, _, _, Qf, Qr)."
          + "b_king(Kf, Kr, Qf, Qr, Bf, Br, Nf, Nr, Qf2, Qr2) :-"
          + "  dir(DF, DR),"
          + "  Nf is Bf + DF, Nr is Br + DR,"
          + "  Nf >= 1, Nf =< 8, Nr >= 1, Nr =< 8,"
          + "  \\+ (Nf =:= Kf, Nr =:= Kr),"
          + "  \\+ near(Nf, Nr, Kf, Kr),"
          + "  capture(Nf, Nr, Qf, Qr, Kf, Kr, Qf2, Qr2),"
          + "  \\+ checked(Kf, Kr, Qf2, Qr2, Nf, Nr)."
          + "mated(Kf, Kr, Qf, Qr, Bf, Br) :-"
          + "  checked(Kf, Kr, Qf, Qr, Bf, Br),"
          + "  \\+ b_king(Kf, Kr, Qf, Qr, Bf, Br, _, _, _, _)."
          + "w_child(Kf, Kr, Qf, Qr, Bf, Br, Nf, Nr, Qf, Qr, Bf, Br) :-"
          + "  w_king(Kf, Kr, Qf, Qr, Bf, Br, Nf, Nr)."
          + "w_child(Kf, Kr, Qf, Qr, Bf, Br, Kf, Kr, Nf, Nr, Bf, Br) :-"
          + "  w_queen(Kf, Kr, Qf, Qr, Bf, Br, Nf, Nr)."
          + "white_mates(Kf, Kr, Qf, Qr, Bf, Br) :-"
          + "  w_child(Kf, Kr, Qf, Qr, Bf, Br, K2, R2, Q2, S2, B2, T2),"
          + "  mated(K2, R2, Q2, S2, B2, T2)."
          + "replies_lose(Kf, Kr, Qf, Qr, Bf, Br) :-"
          + "  b_king(Kf, Kr, Qf, Qr, Bf, Br, _, _, _, _),"
          + "  \\+ (b_king(Kf, Kr, Qf, Qr, Bf, Br, Nf, Nr, Q2, S2),"
          + "      \\+ white_mates(Kf, Kr, Q2, S2, Nf, Nr))."
          + "key_move(k(Nf, Nr)) :-"
          + "  w_king(6, 6, 6, 8, 8, 5, Nf, Nr),"
          + "  \\+ mated(Nf, Nr, 6, 8, 8, 5),"
          + "  replies_lose(Nf, Nr, 6, 8, 8, 5)."
          + "key_move(q(6, 8, Nf, Nr)) :-"
          + "  w_queen(6, 6, 6, 8, 8, 5, Nf, Nr),"
          + "  \\+ mated(6, 6, Nf, Nr, 8, 5),"
          + "  replies_lose(6, 6, Nf, Nr, 8, 5)."
          + "forced(R) :-"
          + "  \\+ checked(6, 6, 6, 8, 8, 5),"
          + "  findall(M, key_move(M), Keys0), sort(Keys0, Keys),"
          + "  findall(k(Bf, Br), b_king(6, 6, 2, 4, 8, 5, Bf, Br, _, _), Replies0),"
          + "  sort(Replies0, Replies),"
          + "  findall(q(2, 4, Tf, Tr),"
          + "    (w_queen(6, 6, 2, 4, 8, 6, Tf, Tr), mated(6, 6, Tf, Tr, 8, 6)),"
          + "    Mates0),"
          + "  sort(Mates0, Mates),"
          + "  R = end(Keys, Replies, Mates)."
          + "immediate_mate :-"
          + "  w_child(6, 6, 6, 8, 8, 5, Kf, Kr, Qf, Qr, Bf, Br),"
          + "  mated(Kf, Kr, Qf, Qr, Bf, Br)."
          + "corner_stalemate :-"
          + "  \\+ checked(2, 6, 3, 7, 1, 8),"
          + "  \\+ b_king(2, 6, 3, 7, 1, 8, _, _, _, _)."
          + "corner_mate :- mated(2, 6, 3, 7, 1, 8).";

  @Test
  void testWolfGoatCabbageCrossings() {
    assertEquals(
        "[['goat','alone','cabbage','goat','wolf','alone','goat'],"
            + "['goat','alone','wolf','goat','cabbage','alone','goat']]",
        this.proveSingle(RIVER_PROGRAM, "crossings(Paths).", "Paths")
    );
    this.checkOnce(
        RIVER_PROGRAM,
        "Start = s(left, left, left, left),"
            + " search(Start, [Start], [], Rev), reverse(Rev, Moves), length(Moves, N), N < 7.",
        false
    );
  }

  @Test
  void testAlphaBetaChoosesTheWinningTicTacToeMove() {
    assertEquals(
        "9 - 1",
        this.proveSingle(
            TIC_TAC_TOE_PROGRAM,
            "played([x, o, x, e, o, x, o, e, e], R).",
            "R"
        )
    );
    assertEquals(
        "5 - 0",
        this.proveSingle(
            TIC_TAC_TOE_PROGRAM,
            "played([x, o, x, o, e, x, o, x, o], R).",
            "R"
        )
    );
  }

  @Test
  void testKnapsackRebuildsTheOptimalLoad() {
    assertEquals(
        "['b','d','e'] - 21",
        this.proveSingle(KNAPSACK_PROGRAM, "loaded(8, R).", "R")
    );
    assertEquals(
        "[] - 0",
        this.proveSingle(KNAPSACK_PROGRAM, "loaded(0, R).", "R")
    );
  }

  @Test
  void testKruskalBuildsTheMinimumSpanningTree() {
    assertEquals(
        "[edge('a','b',1),edge('b','c',2),edge('c','d',3),edge('d','e',5)] - 11",
        this.proveSingle(MST_PROGRAM, "spanning(R).", "R")
    );
  }

  @Test
  void testExplicitUnificationHonorsTheOccursCheck() {
    assertEquals(
        "fun('f',[atm('b'),atm('a')]) - atm('a')",
        this.proveSingle(UNIFY_PROGRAM, "unified(R).", "R")
    );
    this.checkOnce(
        UNIFY_PROGRAM,
        "unify(fun(f, [vr(x)]), fun(f, [fun(f, [vr(x)])]), [], _).",
        false
    );
    this.checkOnce(
        UNIFY_PROGRAM,
        "unify(fun(p, [vr(x), vr(x)]), fun(p, [fun(f, [vr(y)]), vr(y)]), [], _).",
        false
    );
    this.checkOnce(
        UNIFY_PROGRAM,
        "unify(fun(f, [atm(a)]), fun(g, [atm(a)]), [], _).",
        false
    );
  }

  @Test
  void testGaleShapleyFindsTheManOptimalMatching() {
    assertEquals(
        "[match('w1','m1'),match('w2','m2'),match('w3','m3')]",
        this.proveSingle(MARRIAGE_PROGRAM, "married(Matches).", "Matches")
    );
  }

  @Test
  void testQueenEndgameForcesMateInTwo() {
    assertEquals(
        "end([q(6,8,2,4)],[k(8,6)],[q(2,4,8,4)])",
        this.proveSingle(QUEEN_ENDGAME_PROGRAM, "forced(R).", "R")
    );
    this.checkOnce(QUEEN_ENDGAME_PROGRAM, "immediate_mate.", false);
    this.checkOnce(QUEEN_ENDGAME_PROGRAM, "corner_stalemate.", true);
    this.checkOnce(QUEEN_ENDGAME_PROGRAM, "corner_mate.", false);
  }
}
