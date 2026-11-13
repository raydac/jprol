package com.igormaznitsa.jprol.it;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.jprol.logic.JProlContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

@Timeout(30)
class MiscAlgorithmsTest extends AbstractJProlTest {

  private static final String DIJKSTRA_PROGRAM =
      "w_edge(s, a, 4). w_edge(s, b, 2)."
          + "w_edge(b, a, 1). w_edge(b, c, 8). w_edge(b, d, 10)."
          + "w_edge(a, c, 5). w_edge(c, b, 3)."
          + "w_edge(c, d, 2). w_edge(d, e, 3). w_edge(a, e, 15)."
          + "w_edge(x, y, 1)."
          + "dijkstra(Start, Goal, Cost, Path) :-"
          + "  dijkstra_loop([node(0, Start, [Start])], [], Goal, Cost, RevPath),"
          + "  reverse(RevPath, Path)."
          + "dijkstra_loop([node(Cost, Goal, RevPath)|_], _, Goal, Cost, RevPath) :- !."
          + "dijkstra_loop([node(_, U, _)|Rest], Closed, Goal, Cost, RevPath) :-"
          + "  member(U, Closed), !,"
          + "  dijkstra_loop(Rest, Closed, Goal, Cost, RevPath)."
          + "dijkstra_loop([node(C, U, P)|Rest], Closed, Goal, Cost, RevPath) :-"
          + "  findall(node(NC, V, [V|P]),"
          + "    (w_edge(U, V, W), \\+ member(V, Closed), NC is C + W),"
          + "    Next),"
          + "  append(Rest, Next, Combined),"
          + "  msort(Combined, Sorted),"
          + "  dijkstra_loop(Sorted, [U|Closed], Goal, Cost, RevPath).";

  private static final String WATER_JUGS_BFS_PROGRAM =
      "q_empty(q(0, Q, Q))."
          + "q_push(Item, q(N, Front, [Item|Back]), q(N1, Front, Back)) :- N1 is N + 1."
          + "q_pop(Item, q(N, [Item|Front], Back), q(N1, Front, Back)) :- N > 0, N1 is N - 1."
          + "jug_step(j(A, B), fill_a, j(4, B)) :- A < 4."
          + "jug_step(j(A, B), fill_b, j(A, 3)) :- B < 3."
          + "jug_step(j(A, B), empty_a, j(0, B)) :- A > 0."
          + "jug_step(j(A, B), empty_b, j(A, 0)) :- B > 0."
          + "jug_step(j(A, B), pour_ab, j(NA, NB)) :-"
          + "  A > 0, B < 3, Space is 3 - B,"
          + "  (A =< Space -> Pour = A ; Pour = Space),"
          + "  NA is A - Pour, NB is B + Pour."
          + "jug_step(j(A, B), pour_ba, j(NA, NB)) :-"
          + "  B > 0, A < 4, Space is 4 - A,"
          + "  (B =< Space -> Pour = B ; Pour = Space),"
          + "  NA is A + Pour, NB is B - Pour."
          + "bfs_jugs(Start, Target, Moves) :-"
          + "  q_empty(Q0),"
          + "  q_push(state(Start, []), Q0, Q1),"
          + "  bfs_loop(Q1, [Start], Target, RevMoves),"
          + "  reverse(RevMoves, Moves)."
          + "bfs_loop(Q, Visited, Target, RevMoves) :-"
          + "  q_pop(state(Curr, Acc), Q, QRest),"
          + "  ( Curr == Target -> !, RevMoves = Acc"
          + "  ; findall(Act-Next, (jug_step(Curr, Act, Next), \\+ member(Next, Visited)), Pairs),"
          + "    enqueue_pairs(Pairs, Acc, QRest, QNext, Visited, VisitedNext),"
          + "    bfs_loop(QNext, VisitedNext, Target, RevMoves)"
          + "  )."
          + "enqueue_pairs([], _, Q, Q, V, V)."
          + "enqueue_pairs([Act-Next|Rest], Acc, Q0, QOut, V0, VOut) :-"
          + "  ( member(Next, V0) -> Q1 = Q0, V1 = V0"
          + "  ; q_push(state(Next, [Act|Acc]), Q0, Q1), V1 = [Next|V0]"
          + "  ),"
          + "  enqueue_pairs(Rest, Acc, Q1, QOut, V1, VOut)."
          + "replay_jugs(State, [], State)."
          + "replay_jugs(State, [Act|Rest], Final) :-"
          + "  jug_step(State, Act, Next), replay_jugs(Next, Rest, Final).";

  private static final String RED_BLACK_TREE_PROGRAM =
      "rbt_build(Keys, Tree) :- rbt_build_acc(Keys, empty, Tree)."
          + "rbt_build_acc([], Acc, Acc)."
          +
          "rbt_build_acc([K|Ks], Acc, Tree) :- rbt_insert(K, Acc, Next), rbt_build_acc(Ks, Next, Tree)."
          + "rbt_insert(Key, Tree, t(b, L, K, R)) :- rbt_ins(Key, Tree, t(_, L, K, R))."
          + "rbt_ins(X, empty, t(r, empty, X, empty)) :- !."
          + "rbt_ins(X, t(C, L, K, R), Out) :-"
          + "  ( X < K -> rbt_ins(X, L, L1), rbt_balance(C, L1, K, R, Out)"
          + "  ; X > K -> rbt_ins(X, R, R1), rbt_balance(C, L, K, R1, Out)"
          + "  ; Out = t(C, L, K, R)"
          + "  )."
          +
          "rbt_balance(b, t(r, t(r, A, X, B), Y, C), Z, D, t(r, t(b, A, X, B), Y, t(b, C, Z, D))) :- !."
          +
          "rbt_balance(b, t(r, A, X, t(r, B, Y, C)), Z, D, t(r, t(b, A, X, B), Y, t(b, C, Z, D))) :- !."
          +
          "rbt_balance(b, A, X, t(r, t(r, B, Y, C), Z, D), t(r, t(b, A, X, B), Y, t(b, C, Z, D))) :- !."
          +
          "rbt_balance(b, A, X, t(r, B, Y, t(r, C, Z, D)), t(r, t(b, A, X, B), Y, t(b, C, Z, D))) :- !."
          + "rbt_balance(C, L, K, R, t(C, L, K, R))."
          + "rbt_inorder(Tree, Keys) :- rbt_inorder_dl(Tree, Keys, [])."
          + "rbt_inorder_dl(empty, T, T)."
          + "rbt_inorder_dl(t(_, L, K, R), H, T) :-"
          + "  rbt_inorder_dl(L, H, [K|M]), rbt_inorder_dl(R, M, T)."
          + "rbt_member(X, t(_, L, K, R)) :-"
          + "  ( X =:= K -> true ; X < K -> rbt_member(X, L) ; rbt_member(X, R) )."
          + "rbt_no_red_red(empty)."
          + "rbt_no_red_red(t(C, L, _, R)) :-"
          + "  \\+ (C == r, (L = t(r, _, _, _) ; R = t(r, _, _, _))),"
          + "  rbt_no_red_red(L), rbt_no_red_red(R)."
          + "rbt_black_height(empty, 1)."
          + "rbt_black_height(t(C, L, _, R), BH) :-"
          + "  rbt_black_height(L, BHL), rbt_black_height(R, BHR),"
          + "  BHL =:= BHR,"
          + "  (C == b -> BH is BHL + 1 ; BH = BHL).";

  private static final String DPLL_SAT_PROGRAM =
      "complement(pos(V), neg(V))."
          + "complement(neg(V), pos(V))."
          + "simplify_cnf([], _, [])."
          + "simplify_cnf([Clause|Rest], Lit, Out) :-"
          + "  ( member(Lit, Clause) -> simplify_cnf(Rest, Lit, Out)"
          + "  ; complement(Lit, Comp),"
          + "    remove_lit(Clause, Comp, Reduced),"
          + "    Out = [Reduced|RestOut],"
          + "    simplify_cnf(Rest, Lit, RestOut)"
          + "  )."
          + "remove_lit([], _, [])."
          + "remove_lit([X|Xs], Lit, Out) :-"
          + "  ( X == Lit -> remove_lit(Xs, Lit, Out)"
          + "  ; Out = [X|Rest], remove_lit(Xs, Lit, Rest)"
          + "  )."
          + "dpll([], Acc, Sorted) :- !, sort(Acc, Sorted)."
          + "dpll(Clauses, _, _) :- member([], Clauses), !, fail."
          + "dpll(Clauses, Acc, Model) :-"
          + "  member([Unit], Clauses), !,"
          + "  simplify_cnf(Clauses, Unit, Next),"
          + "  dpll(Next, [Unit|Acc], Model)."
          + "dpll([[Lit|ClauseTail]|Rest], Acc, Model) :-"
          + "  All = [[Lit|ClauseTail]|Rest],"
          + "  ( Pick = Lit ; complement(Lit, Pick) ),"
          + "  simplify_cnf(All, Pick, Next),"
          + "  dpll(Next, [Pick|Acc], Model).";

  private static final String SYMBOLIC_DIFF_PROGRAM =
      "deriv(X, X, 1) :- atom(X), !."
          + "deriv(C, X, 0) :- atomic(C), C \\== X, !."
          + "deriv(U + V, X, DU + DV) :- !, deriv(U, X, DU), deriv(V, X, DV)."
          + "deriv(U - V, X, DU - DV) :- !, deriv(U, X, DU), deriv(V, X, DV)."
          + "deriv(-U, X, -DU) :- !, deriv(U, X, DU)."
          + "deriv(U * V, X, DU * V + U * DV) :- !, deriv(U, X, DU), deriv(V, X, DV)."
          + "deriv(U ** N, X, N * U ** N1 * DU) :- integer(N), !, N1 is N - 1, deriv(U, X, DU)."
          + "deriv(sin(U), X, cos(U) * DU) :- !, deriv(U, X, DU)."
          + "deriv(cos(U), X, -(sin(U) * DU)) :- !, deriv(U, X, DU)."
          + "simplify_fix(E, Out) :-"
          + "  simplify_step(E, Next),"
          + "  ( E == Next -> Out = Next ; simplify_fix(Next, Out) )."
          +
          "simplify_step(A + B, R) :- !, simplify_step(A, SA), simplify_step(B, SB), simp_add(SA, SB, R)."
          +
          "simplify_step(A - B, R) :- !, simplify_step(A, SA), simplify_step(B, SB), simp_sub(SA, SB, R)."
          +
          "simplify_step(A * B, R) :- !, simplify_step(A, SA), simplify_step(B, SB), simp_mul(SA, SB, R)."
          +
          "simplify_step(A ** B, R) :- !, simplify_step(A, SA), simplify_step(B, SB), simp_pow(SA, SB, R)."
          + "simplify_step(-A, R) :- !, simplify_step(A, SA), simp_neg(SA, R)."
          + "simplify_step(sin(A), sin(SA)) :- !, simplify_step(A, SA)."
          + "simplify_step(cos(A), cos(SA)) :- !, simplify_step(A, SA)."
          + "simplify_step(E, E)."
          + "simp_add(A, B, V) :- number(A), number(B), !, V is A + B."
          + "simp_add(0, B, B) :- !."
          + "simp_add(A, 0, A) :- !."
          + "simp_add(A, B, A + B)."
          + "simp_sub(A, B, V) :- number(A), number(B), !, V is A - B."
          + "simp_sub(A, 0, A) :- !."
          + "simp_sub(0, B, -B) :- !."
          + "simp_sub(A, B, 0) :- A == B, !."
          + "simp_sub(A, B, A - B)."
          + "simp_mul(A, B, V) :- number(A), number(B), !, V is A * B."
          + "simp_mul(0, _, 0) :- !."
          + "simp_mul(_, 0, 0) :- !."
          + "simp_mul(1, B, B) :- !."
          + "simp_mul(A, 1, A) :- !."
          + "simp_mul(C1, C2 * E, C * E) :- number(C1), number(C2), !, C is C1 * C2."
          + "simp_mul(C1 * E, C2, C * E) :- number(C1), number(C2), !, C is C1 * C2."
          + "simp_mul(A, B, A * B)."
          + "simp_pow(_, 0, 1) :- !."
          + "simp_pow(A, 1, A) :- !."
          + "simp_pow(A, B, A ** B)."
          + "simp_neg(0, 0) :- !."
          + "simp_neg(-A, A) :- !."
          + "simp_neg(N, V) :- number(N), !, V is -N."
          + "simp_neg(A, -A)."
          + "eval_sym(X, X, Val, Val) :- atom(X), !."
          + "eval_sym(N, _, _, N) :- number(N), !."
          +
          "eval_sym(A + B, X, V, R) :- !, eval_sym(A, X, V, RA), eval_sym(B, X, V, RB), R is RA + RB."
          +
          "eval_sym(A - B, X, V, R) :- !, eval_sym(A, X, V, RA), eval_sym(B, X, V, RB), R is RA - RB."
          +
          "eval_sym(A * B, X, V, R) :- !, eval_sym(A, X, V, RA), eval_sym(B, X, V, RB), R is RA * RB."
          +
          "eval_sym(A ** B, X, V, R) :- !, eval_sym(A, X, V, RA), eval_sym(B, X, V, RB), R is RA ** RB."
          + "eval_sym(-A, X, V, R) :- !, eval_sym(A, X, V, RA), R is -RA.";

  private static final String HINDLEY_MILNER_PROGRAM =
      "occurs_in(V, T) :- var(T), !, V == T."
          +
          "occurs_in(V, T) :- compound(T), functor(T, _, N), between(1, N, I), arg(I, T, A), occurs_in(V, A), !."
          + "unify_t(T1, T2) :- var(T1), var(T2), T1 == T2, !."
          + "unify_t(T1, T2) :- var(T1), !, \\+ occurs_in(T1, T2), T1 = T2."
          + "unify_t(T1, T2) :- var(T2), !, \\+ occurs_in(T2, T1), T2 = T1."
          + "unify_t(T1, T2) :- atomic(T1), atomic(T2), !, T1 == T2."
          + "unify_t(T1, T2) :-"
          + "  compound(T1), compound(T2),"
          + "  functor(T1, F, N), functor(T2, F, N),"
          + "  unify_t_args(1, N, T1, T2)."
          + "unify_t_args(I, N, _, _) :- I > N, !."
          + "unify_t_args(I, N, T1, T2) :-"
          + "  arg(I, T1, A1), arg(I, T2, A2),"
          + "  unify_t(A1, A2),"
          + "  I1 is I + 1,"
          + "  unify_t_args(I1, N, T1, T2)."
          + "lookup_t(Name, [mono(Name, T)|_], Out) :- !, unify_t(T, Out)."
          + "lookup_t(Name, [poly(Name, EnvSnap, GenT)|_], Out) :- !,"
          + "  copy_term(EnvSnap - GenT, EnvSnap - InstT),"
          + "  unify_t(InstT, Out)."
          + "lookup_t(Name, [_|Rest], Out) :- lookup_t(Name, Rest, Out)."
          + "typeof(_, num(_), T) :- !, unify_t(T, int_t)."
          + "typeof(_, bool(_), T) :- !, unify_t(T, bool_t)."
          + "typeof(Env, var(X), T) :- !, lookup_t(X, Env, T)."
          + "typeof(Env, pair(A, B), T) :- !,"
          + "  typeof(Env, A, TA), typeof(Env, B, TB), unify_t(T, pair_t(TA, TB))."
          + "typeof(Env, if_e(Cond, Then, Else), T) :- !,"
          + "  typeof(Env, Cond, bool_t), typeof(Env, Then, T), typeof(Env, Else, T)."
          + "typeof(Env, lam(X, Body), T) :- !,"
          + "  typeof([mono(X, ArgT)|Env], Body, RetT),"
          + "  unify_t(T, arr(ArgT, RetT))."
          + "typeof(Env, app(Fn, Arg), T) :- !,"
          + "  typeof(Env, Fn, arr(ArgT, T)),"
          + "  typeof(Env, Arg, ArgT)."
          + "typeof(Env, let_e(X, Val, Body), T) :-"
          + "  typeof(Env, Val, ValT),"
          + "  typeof([poly(X, Env, ValT)|Env], Body, T).";

  private static final String MEMOIZED_DP_PROGRAM =
      ":- dynamic([memo_fib/2, memo_ed/3])."
          + "fib_dp(0, 0) :- !."
          + "fib_dp(1, 1) :- !."
          + "fib_dp(N, F) :- memo_fib(N, F), !."
          + "fib_dp(N, F) :-"
          + "  N1 is N - 1, N2 is N - 2,"
          + "  fib_dp(N1, F1), fib_dp(N2, F2),"
          + "  F is F1 + F2,"
          + "  assertz(memo_fib(N, F))."
          + "edit_dist(A, B, D) :-"
          + "  retractall(memo_ed(_, _, _)),"
          + "  atom_codes(A, CA), atom_codes(B, CB),"
          + "  ed(CA, CB, D)."
          + "ed([], Bs, N) :- !, length(Bs, N)."
          + "ed(As, [], N) :- !, length(As, N)."
          + "ed(As, Bs, D) :- memo_ed(As, Bs, D), !."
          + "ed([X|As], [X|Bs], D) :- !, ed(As, Bs, D), assertz(memo_ed([X|As], [X|Bs], D))."
          + "ed([A|As], [B|Bs], D) :-"
          + "  ed(As, [B|Bs], Del),"
          + "  ed([A|As], Bs, Ins),"
          + "  ed(As, Bs, Sub),"
          + "  min3(Del, Ins, Sub, Best),"
          + "  D is Best + 1,"
          + "  assertz(memo_ed([A|As], [B|Bs], D))."
          + "min3(A, B, C, M) :- (A =< B -> M1 = A ; M1 = B), (M1 =< C -> M = M1 ; M = C).";

  private static final String ARITH_PARSER_PROGRAM =
      "eval_text(Atom, Result) :-"
          + "  atom_codes(Atom, Codes),"
          + "  tokenize(Codes, Tokens),"
          + "  parse_expr(Result, Tokens, [])."
          + "tokenize([], []) :- !."
          + "tokenize([C|Cs], Ts) :- C =< 32, !, tokenize(Cs, Ts)."
          + "tokenize([C|Cs], [num(N)|Ts]) :- C >= 48, C =< 57, !,"
          + "  read_digits(Cs, Ds, Rest), number_codes(N, [C|Ds]), tokenize(Rest, Ts)."
          + "tokenize([43|Cs], [op(+)|Ts]) :- !, tokenize(Cs, Ts)."
          + "tokenize([45|Cs], [op(-)|Ts]) :- !, tokenize(Cs, Ts)."
          + "tokenize([42|Cs], [op(*)|Ts]) :- !, tokenize(Cs, Ts)."
          + "tokenize([47,47|Cs], [op(//)|Ts]) :- !, tokenize(Cs, Ts)."
          + "tokenize([94|Cs], [op(^)|Ts]) :- !, tokenize(Cs, Ts)."
          + "tokenize([40|Cs], [lparen|Ts]) :- !, tokenize(Cs, Ts)."
          + "tokenize([41|Cs], [rparen|Ts]) :- !, tokenize(Cs, Ts)."
          + "read_digits([C|Cs], [C|Ds], Rest) :- C >= 48, C =< 57, !, read_digits(Cs, Ds, Rest)."
          + "read_digits(Rest, [], Rest)."
          + "parse_expr(V, T0, T) :- parse_term(L, T0, T1), parse_expr_tail(L, V, T1, T)."
          +
          "parse_expr_tail(Acc, V, [op(+)|T1], T) :- !, parse_term(R, T1, T2), Acc1 is Acc + R, parse_expr_tail(Acc1, V, T2, T)."
          +
          "parse_expr_tail(Acc, V, [op(-)|T1], T) :- !, parse_term(R, T1, T2), Acc1 is Acc - R, parse_expr_tail(Acc1, V, T2, T)."
          + "parse_expr_tail(Acc, Acc, T, T)."
          + "parse_term(V, T0, T) :- parse_power(L, T0, T1), parse_term_tail(L, V, T1, T)."
          +
          "parse_term_tail(Acc, V, [op(*)|T1], T) :- !, parse_power(R, T1, T2), Acc1 is Acc * R, parse_term_tail(Acc1, V, T2, T)."
          +
          "parse_term_tail(Acc, V, [op(//)|T1], T) :- !, parse_power(R, T1, T2), Acc1 is Acc // R, parse_term_tail(Acc1, V, T2, T)."
          + "parse_term_tail(Acc, Acc, T, T)."
          + "parse_power(V, T0, T) :-"
          + "  parse_unary(Base, T0, T1),"
          + "  ( T1 = [op(^)|T2] -> parse_power(Exp, T2, T), V is Base ** Exp"
          + "  ; V = Base, T = T1"
          + "  )."
          + "parse_unary(V, [op(-)|T1], T) :- !, parse_unary(U, T1, T), V is -U."
          + "parse_unary(V, [op(+)|T1], T) :- !, parse_unary(V, T1, T)."
          + "parse_unary(V, [lparen|T1], T) :- !, parse_expr(V, T1, [rparen|T])."
          + "parse_unary(N, [num(N)|T], T).";

  private static final String TOPO_SORT_PROGRAM =
      ":- dynamic(dep/2)."
          + "dep(core, parser). dep(core, utils)."
          + "dep(io, core). dep(thread, core)."
          + "dep(gui, io). dep(gui, thread)."
          + "dep(app, gui). dep(parser, utils)."
          + "all_nodes([app, gui, io, thread, core, parser, utils])."
          + "topo_sort(Order) :-"
          + "  all_nodes(Nodes),"
          + "  dfs_all(Nodes, [], [], RevOrder),"
          + "  reverse(RevOrder, Order)."
          + "dfs_all([], _, Done, Done)."
          + "dfs_all([N|Ns], Gray, Done0, DoneOut) :-"
          + "  dfs_node(N, Gray, Done0, Done1),"
          + "  dfs_all(Ns, Gray, Done1, DoneOut)."
          + "dfs_node(N, _, Done, Done) :- member(N, Done), !."
          + "dfs_node(N, Gray, _, _) :- member(N, Gray), !, fail."
          + "dfs_node(N, Gray, Done0, [N|Done1]) :-"
          + "  findall(D, dep(N, D), Deps),"
          + "  dfs_all(Deps, [N|Gray], Done0, Done1)."
          + "comes_before(A, B, List) :-"
          + "  append(_, [A|AfterA], List),"
          + "  member(B, AfterA).";

  private void calcAckermann(final int n, final int m, final int expected) {
    try (final JProlContext context = this.makeContextAndConsult(
        "ackermann(0, M, R) :- R is M + 1."
            + "ackermann(N, 0, R) :- N > 0, N1 is N - 1, ackermann(N1, 1, R)."
            + "ackermann(N, M, R) :- N > 0, M > 0, M1 is M - 1, ackermann(N, M1, R1),"
            + " N1 is N - 1, ackermann(N1, R1, R)."
    )) {
      final JProlChoicePoint goal = context.makeChoicePoint("ackermann(" + n + ',' + m + ",A).");
      final Term resultTerm = goal.prove();
      assertNotNull(resultTerm);
      final Term result = goal.findVar("A").orElseThrow().getValue();
      assertNotNull(result);
      assertEquals(expected, result.toNumber().intValue());
      assertNull(goal.prove());
    }
  }

  @Test
  void testAckerman() {
    this.calcAckermann(0, 0, 1);
    this.calcAckermann(0, 1, 2);
    this.calcAckermann(0, 2, 3);
    this.calcAckermann(0, 3, 4);
    this.calcAckermann(0, 4, 5);
    this.calcAckermann(1, 0, 2);
    this.calcAckermann(1, 1, 3);
    this.calcAckermann(1, 2, 4);
    this.calcAckermann(1, 3, 5);
    this.calcAckermann(1, 4, 6);
    this.calcAckermann(2, 0, 3);
    this.calcAckermann(2, 1, 5);
    this.calcAckermann(2, 2, 7);
    this.calcAckermann(2, 3, 9);
    this.calcAckermann(3, 0, 5);
    this.calcAckermann(3, 5, 253);
    this.calcAckermann(3, 6, 509);
  }

  @Test
  void testFibonacci() {
    try (final JProlContext context = this.makeContextAndConsult(
        "fib(1,1):-!. fib(0,0):-!. fib(N,Result):-Npp is N-2, Np is N-1,"
            + " fib(Npp,Resultpp), fib(Np,Resultp), Result is Resultpp+Resultp.")) {
      final JProlChoicePoint goal = context.makeChoicePoint("fib(22,Result).");

      assertNotNull(goal.prove());
      assertEquals(17711L, getVarAsNumber(goal, "Result").intValue());
      assertNull(goal.prove());
    }
  }

  @Test
  void testQuasiObjectMethods() {
    try (final JProlContext context = this.makeContextAndConsult(
        "object(rectangle(Length,Width),[(area(A):-A is Length * Width),"
            + "(describe :- write('Rectangle of size'), write(Length * Width))])."
            + "send(Object,Message):-get_methods(Object,Methods),process(Message,Methods)."
            + "get_methods(Object,Methods):-object(Object,Methods)."
            + "process(Message,[Message|_])."
            + "process(Message,[(Message :- Body)|_]):-call(Body)."
            + "process(Message,[_|Methods]):-process(Message,Methods).")) {

      final JProlChoicePoint goal =
          context.makeChoicePoint("Rec1=rectangle(4,3),send(Rec1,area(Area)).");

      assertNotNull(goal.prove());
      assertEquals(12L, getVarAsNumber(goal, "Area").longValue());
      assertNull(goal.prove());
    }
  }

  @Test
  void testDijkstraShortestWeightedPath() {
    assertEquals(
        "13 - ['s','b','a','c','d','e']",
        this.proveSingle(DIJKSTRA_PROGRAM, "dijkstra(s, e, C, P), R = C - P.", "R")
    );
    assertEquals(
        "3 - ['s','b','a']",
        this.proveSingle(DIJKSTRA_PROGRAM, "dijkstra(s, a, C, P), R = C - P.", "R")
    );
    assertEquals(
        "0 - ['s']",
        this.proveSingle(DIJKSTRA_PROGRAM, "dijkstra(s, s, C, P), R = C - P.", "R")
    );
    this.checkOnce(DIJKSTRA_PROGRAM, "dijkstra(s, y, _, _).", false);
  }

  @Test
  void testBreadthFirstSearchWithOpenDifferenceListQueue() {
    assertEquals(
        "['fill_b','pour_ba','fill_b','pour_ba','empty_a','pour_ba']",
        this.proveSingle(
            WATER_JUGS_BFS_PROGRAM,
            "bfs_jugs(j(0, 0), j(2, 0), Moves), replay_jugs(j(0, 0), Moves, j(2, 0)).",
            "Moves"
        )
    );
    assertEquals(
        "4",
        this.proveSingle(
            WATER_JUGS_BFS_PROGRAM,
            "bfs_jugs(j(0, 0), j(4, 2), Moves), length(Moves, Len).",
            "Len"
        )
    );
  }

  @Test
  void testRedBlackTreeInsertionAndBalanceInvariants() {
    assertEquals(
        "[1,2,3,4,6,7,9,11,13,15,17,18,20]",
        this.proveSingle(
            RED_BLACK_TREE_PROGRAM,
            "rbt_build([15,6,18,3,7,17,20,2,4,13,9,6,15,1,11], T),"
                + " rbt_no_red_red(T),"
                + " rbt_black_height(T, BH), BH >= 3,"
                + " forall(member(K, [1,7,13,20]), rbt_member(K, T)),"
                + " \\+ rbt_member(8, T),"
                + " rbt_inorder(T, Sorted).",
            "Sorted"
        )
    );
  }

  @Test
  void testDpllPropositionalSatSolver() {
    final String uniqueSatFormula =
        "[[pos(a), pos(b)],"
            + " [neg(a), neg(b)],"
            + " [pos(a), neg(b)],"
            + " [neg(a), pos(b), pos(c)],"
            + " [neg(c), pos(a)]]";
    assertEquals(
        "[neg('b'),pos('a'),pos('c')]",
        this.proveSingle(DPLL_SAT_PROGRAM, "dpll(" + uniqueSatFormula + ", [], M).", "M")
    );

    final String pigeonhole3Into2 =
        "[[pos(p11), pos(p12)], [pos(p21), pos(p22)], [pos(p31), pos(p32)],"
            + " [neg(p11), neg(p21)], [neg(p11), neg(p31)], [neg(p21), neg(p31)],"
            + " [neg(p12), neg(p22)], [neg(p12), neg(p32)], [neg(p22), neg(p32)]]";
    this.checkOnce(DPLL_SAT_PROGRAM, "dpll(" + pigeonhole3Into2 + ", [], _).", false);
  }

  @Test
  void testSymbolicDifferentiationAndAlgebraicSimplifier() {
    assertEquals(
        "9 * 'x' ** 2 - 10 * 'x' + 7",
        this.proveSingle(
            SYMBOLIC_DIFF_PROGRAM,
            "deriv(3 * x ** 3 - 5 * x ** 2 + 7 * x - 11, x, Raw), simplify_fix(Raw, Simp).",
            "Simp"
        )
    );
    assertEquals(
        "23",
        this.proveSingle(
            SYMBOLIC_DIFF_PROGRAM,
            "deriv(3 * x ** 3 - 5 * x ** 2 + 7 * x - 11, x, Raw),"
                + " simplify_fix(Raw, Simp), eval_sym(Simp, x, 2, Val).",
            "Val"
        )
    );
    assertEquals(
        "cos('x' ** 2) * 2 * 'x'",
        this.proveSingle(
            SYMBOLIC_DIFF_PROGRAM,
            "deriv(sin(x ** 2), x, Raw), simplify_fix(Raw, Simp).",
            "Simp"
        )
    );
  }

  @Test
  void testHindleyMilnerPolymorphicTypeInference() {
    assertEquals(
        "'int_t'",
        this.proveSingle(
            HINDLEY_MILNER_PROGRAM,
            "typeof([], let_e(id, lam(x, var(x)),"
                + " if_e(app(var(id), bool(true)), app(var(id), num(42)), num(0))), T).",
            "T"
        )
    );
    assertEquals(
        "pair_t('bool_t','int_t')",
        this.proveSingle(
            HINDLEY_MILNER_PROGRAM,
            "typeof([], let_e(id, lam(x, var(x)),"
                + " pair(app(var(id), bool(true)), app(var(id), num(7)))), T).",
            "T"
        )
    );
    this.checkOnce(
        HINDLEY_MILNER_PROGRAM,
        "typeof([], lam(f, pair(app(var(f), bool(true)), app(var(f), num(7)))), _).",
        false
    );
    this.checkOnce(
        HINDLEY_MILNER_PROGRAM,
        "typeof([], lam(x, app(var(x), var(x))), _).",
        false
    );
    this.checkOnce(
        HINDLEY_MILNER_PROGRAM,
        "typeof([], lam(f, lam(g, lam(x, app(var(f), app(var(g), var(x)))))), T),"
            + " T =@= arr(arr(B, C), arr(arr(A, B), arr(A, C))).",
        true
    );
  }

  @Test
  void testMemoizedDynamicProgrammingFibonacciAndEditDistance() {
    assertEquals(
        "190392490709135",
        this.proveSingle(MEMOIZED_DP_PROGRAM, "fib_dp(70, F).", "F")
    );
    assertEquals(
        "[3,5,4,0]",
        this.proveSingle(
            MEMOIZED_DP_PROGRAM,
            "edit_dist(kitten, sitting, D1),"
                + " edit_dist(intention, execution, D2),"
                + " edit_dist('', test, D3),"
                + " edit_dist(prolog, prolog, D4),"
                + " R = [D1, D2, D3, D4].",
            "R"
        )
    );
  }

  @Test
  void testRecursiveDescentArithmeticExpressionParser() {
    assertEquals(
        "502",
        this.proveSingle(
            ARITH_PARSER_PROGRAM,
            "eval_text(' 2 ^ 3 ^ 2 - (14 + 6) // 4 * (3 + -1) ', V).",
            "V"
        )
    );
    assertEquals(
        "45",
        this.proveSingle(ARITH_PARSER_PROGRAM, "eval_text('100 - 30 - 20 - 5', V).", "V")
    );
    this.checkOnce(ARITH_PARSER_PROGRAM, "eval_text('(1 + 2', _).", false);
    this.checkOnce(ARITH_PARSER_PROGRAM, "eval_text('1 + 2 3', _).", false);
  }

  @Test
  void testTopologicalSortAndCycleDetectionOnDag() {
    this.checkOnce(
        TOPO_SORT_PROGRAM,
        "topo_sort(Order), length(Order, 7),"
            + " forall(dep(Pkg, Dep), comes_before(Dep, Pkg, Order)).",
        true
    );
    this.checkOnce(
        TOPO_SORT_PROGRAM,
        "assertz(dep(utils, gui)), topo_sort(_).",
        false
    );
  }
}
