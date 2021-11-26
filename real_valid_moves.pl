% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(real_valid_moves, [real_valid_moves/2]).

:- use_module(can_move).
:- use_module(game).
:- use_module(valid_moves).
:- use_module(hive).
:- use_module(make_move).


real_valid_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), []) :- 
    not(can_move(cell(Bug, Row, Column, Color, StackPosition, InGame))).

real_valid_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), RealValidMoves) :- 
    can_move(cell(Bug, Row, Column, Color, StackPosition, InGame)),
    valid_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), ValidMoves),
    trying_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), ValidMoves, RealValidMoves).
    
trying_moves(cell(_, _, _, _, _, _), [], []).

trying_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), [[MovesHR, MovesHC]| MovesT], FilterMoves) :-
    make_move(cell(Bug, Row, Column, Color, StackPosition, InGame), [MovesHR, MovesHC]),
    try_move([MovesHR, MovesHC], ValidMoves1),
    make_move(cell(Bug, MovesHR, MovesHC, Color, StackPosition, InGame), [Row, Column]).

    trying_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), MovesT, ValidMoves2),
    append(ValidMoves1, ValidMoves2, FilterMoves).


try_move(_, []) :-
    not(hive_consitent()).

try_move(X, [X]) :-
    hive_consitent().
