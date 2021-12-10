:- module(paint, [paint_board/0]).
:- use_module(game).

even_row(false).

:- dynamic even_row/1.

paint_board():-
    findall(X, cell(X, _, _, _, _, true), P),
    paint_board2(P).

paint_board2([_|_]):-
    is_min(Min_row),
    is_max(Max_row),
    is_min_to_the_left(Rl, Cl),
    is_max_to_the_right(Rr, Cr),
    R is Min_row - 1,
    TopR is Max_row + 1,
    amount_of_cells(R, Rl, Cl, Rr, Cr, FirstC, LastC, Amount),
    paint(R, FirstC, LastC, TopR, Amount),
    finish(Amount).

paint_board2([]):- paint(-1, -1, 1, 1, 3).

finish(_):- even_row(false).
finish(Amount):-
    even_row(true),
    A is Amount - 1,
    paint_base(A).

paint(R, C, TopC, TopR, Amount):-
    not(TopR < R),
    even_row(false),
    paint_top(Amount),
    paint_center(R, C, TopC),
    paint_base(Amount),
    change_row(),
    NR is R + 1,
    NTopC is TopC - 1,
    paint(NR, C, NTopC, TopR, Amount).

paint(R, C, TopC, TopR, Amount):-
    not(R > TopR),
    even_row(true),
    paint_center(R, C, TopC),
    change_row(),
    NR is R + 1,
    NC is C - 1,
    paint(NR, NC, TopC, TopR, Amount).


paint_top(Amount):-
    paint_top_first(Amount),
    write("\n"),
    paint_top_second(Amount),
    write("\n").

paint_top_first(0).
paint_top_first(Amount):-
    Amount > 0,
    write("  / \\ "),
    NAmount is Amount - 1,
    paint_top_first(NAmount).

paint_top_second(0).
paint_top_second(Amount):-
    Amount > 0,
    write(" /   \\"),
    NAmount is Amount - 1,
    paint_top_second(NAmount).

paint_center(R, C, T):-
    paint_center_first(R, C, T),
    write("|\n"),
    paint_center_second(R, C, T),
    write("|\n").

paint_center_first(_, C, T):- C > T.
paint_center_first(R, C, T):-
    not(C > T),
    find_cell(R, C, Color, Bug),
    write("| " + Color + " "),
    print_bug(Bug),
    write(" "),
    NC is C + 1,
    paint_center_first(R, NC, T).

paint_center_second(_, C, T):- C > T.
paint_center_second(R, C, T):-
    not(C > T),
    write("|"),
    print_number(R),
    write(","),
    print_number(C),
    NC is C + 1,
    paint_center_second(R, NC, T).

paint_base(Amount):-
    paint_base_first(Amount),
    write("\n"),
    paint_base_second(Amount),
    write("\n").

paint_base_first(0).
paint_base_first(Amount):-
    Amount > 0,
    write(" \\   /"),
    NAmount is Amount - 1,
    paint_base_first(NAmount).

paint_base_second(0).
paint_base_second(Amount):-
    Amount > 0,
    write("  \\ / "),
    NAmount is Amount - 1,
    paint_base_first(NAmount).

change_row():-
    even_row(true),
    retract(even_row(true)),
    assert(even_row(false)).

change_row():-
    even_row(false),
    retract(even_row(false)),
    assert(even_row(true)).

find_cell(R, C, Color, Bug):-
    findall([BT, Color, SP], cell(BT, R, C, Color, SP, true), P),
    choose_max_SP(P, Sp),
    is_member2([Bug, Color, Sp], P).

is_member2(X, [X|_]).
is_member2(X, [_|Z]):- is_member2(X, Z).

choose_max_SP([[_, _, Sp]], Sp):- !.
choose_max_SP([[_, _, SP]|L], Sp):-
    choose_max_SP(L, R),
    max_element(SP, R, Sp).

max_element(X, Y, Y):- X < Y.
max_element(X, Y, X):- not(X < Y).

print_bug(queen):- write("Q").
print_bug(ant):- write("A").
print_bug(beetle):- write("B").
print_bug(spider):- write("S").
print_bug(grasshopper):- write("G").
print_bug(ladybug):- write("L").
print_bug(mosquito):- write("M").
print_bug(pillbug):- write("P").

print_number(N):-
    N < 0, !,
    write("-"),
    NN is -1 * N,
    print_number_in_one(NN).
print_number(N):-
    print_number_in_two(N).

print_number_in_one(N):-
    N < 10, !,
    write(N).
print_number_in_one(N):-
    NN is mod(N, 10),
    write(NN).

print_number_in_two(N):-
    N < 10, !, 
    write(" " + N).
print_number_in_two(N):- write(N).

is_min(R):-
    findall(Row, cell(_, Row, _, _, _, true), P),
    choose_min(P, R).

choose_min([R], R):- !.
choose_min([X|Y], R):-
    choose_min(Y, P),
    min_element(X, P, R).

min_element(X, Y, Y):- X > Y.
min_element(X, Y, X):- not(X > Y).

is_max(R):-
    findall(Row, cell(_, Row, _, _, _, true), P),
    choose_max(P, R).

choose_max([X], X):- !.
choose_max([X|L], R):-
    choose_max(L, S),
    max_element(S, X, R).

is_min_to_the_left(R, C):-
    findall(Col, cell(_, _, Col, _, _, true), P),
    choose_min(P, C),
    findall(Row, cell(_, Row, C, _, _, true), Q),
    choose_min(Q, R).

is_max_to_the_right(R, C):-
    findall(Col, cell(_, _, Col, _, _, true), P),
    choose_max(P, C),
    findall(Row, cell(_, Row, C, _, _, true), Q),
    choose_max(Q, R).

amount_of_cells(R, Rl, Cl, Rr, Cr, FirstC, LastC, Amount):-
    FirstC is (R - Rl) // 2 + Cl,
    LastC is (R - Rr) // 2 + Cr,
    Amount is LastC - FirstC + 1. 
