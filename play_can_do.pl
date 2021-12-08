% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(play_can_do, [play_can_do/2]).
:- use_module(game).
:- use_module(bug_can_move).
:- use_module(bug_can_power).


play_can_do(Color, Plays) :-
    can_put(Color, Put),
    can_move(Color, Move),
    can_power(Color, Power),
    append([Put, Move, Power], Plays).

can_move(Color, []) :-
    cell(queen, _, _, Color, _, false).

can_move(Color, X) :-
    cell(queen, _, _, Color, _, true),
    bug_can_move(Color, BugCanMove),
    can_move2(BugCanMove, X).

can_move2([], []).

can_move2([_|_], [move]).


can_put(Color, []) :-
    not(cell(_, _, _, Color, _, false)).

can_put(Color, [put]) :-
    cell(_, _, _, Color, _, false).


can_power(Color, []) :-
    cell(queen, _, _, Color, _, false).

can_power(Color, X) :-
    cell(queen, _, _, Color, _, true),
    bug_can_power(Color, BugCanPower),
    can_power2(BugCanPower, X).

can_power2([], []).

can_power2([_|_], [power]).