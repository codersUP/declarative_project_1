% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(play_can_do, [play_can_do/2]).
:- use_module(game).


play_can_do(Color, [put]) :-
    not(cell(queen, _, _, Color, _,true)).

play_can_do(Color, [move]) :-
    not(cell(_, _, _, Color, _, false)).

play_can_do(Color, [move, put]) :-
    cell(queen, _, _, Color, _,true),
    cell(_, _, _, Color, _, false).
