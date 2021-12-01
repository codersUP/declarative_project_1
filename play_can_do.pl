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

can_move(Color, []) :-
    cell(queen, _, _, Color, _, true),
    bug_can_move(Color, BugCanMove),
    BugCanMove = [].

can_move(Color, [move]) :-
    cell(queen, _, _, Color, _, true),
    bug_can_move(Color, BugCanMove),
    not(BugCanMove = []).


can_put(Color, []) :-
    not(cell(_, _, _, Color, _, false)).

can_put(Color, [put]) :-
    cell(_, _, _, Color, _, false).


can_power(Color, []) :-
    cell(queen, _, _, Color, _, false).

can_power(Color, []) :-
    cell(queen, _, _, Color, _, true),
    bug_can_power(Color, BugCanPower),
    BugCanPower = [].

can_power(Color, [power]) :-
    cell(queen, _, _, Color, _, true),
    bug_can_power(Color, BugCanPower),
    not(BugCanPower = []).
