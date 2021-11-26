% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(real_valid_moves, [real_valid_moves/2]).

:- use_module(can_move).
:- use_module(game).
:- use_module(valid_moves).
:- use_module(hive).


real_valid_moves(cell(Bug, Row, Column, _, StackPosition, InGame), []) :- 
    not(can_move(cell(Bug, Row, Column, _, StackPosition, InGame))).

real_valid_moves(cell(Bug, Row, Column, _, StackPosition, InGame), RealValidMoves) :- 
    can_move(cell(Bug, Row, Column, _, StackPosition, InGame)),
    valid_moves(cell(Bug, Row, Column, _, StackPosition, InGame), ValidMoves),
    trying_moves(cell(Bug, Row, Column, _, StackPosition, InGame), ValidMoves, RealValidMoves).
    
trying_moves(cell(_, _, _, _, _, _), [], []).

trying_moves(cell(Bug, Row, Column, _, StackPosition, InGame), [[MovesHR, MovesHC]| MovesT], FilterMoves) :-
    retract(cell(Bug, Row, Column, _, StackPosition, InGame)),
    assertz(cell(Bug, MovesHR, MovesHC, _, StackPosition, InGame)),
    try_move([MovesHR, MovesHC], ValidMoves1),
    retract(cell(Bug, MovesHR, MovesHC, _, StackPosition, InGame)),
    assertz(cell(Bug, Row, Column, _, StackPosition, InGame)),

    trying_moves(cell(Bug, Row, Column, _, StackPosition, InGame), MovesT, ValidMoves2),
    append(ValidMoves1, ValidMoves2, FilterMoves).


try_move(_, []) :-
    not(hive_consitent()).

try_move(X, [X]) :-
    hive_consitent().
