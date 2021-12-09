% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(real_valid_moves, [real_valid_moves/2, try_move/2]).

:- use_module(can_move).
:- use_module(game).
:- use_module(valid_moves).
:- use_module(hive).
:- use_module(make_move).


% real_valid_moves_only_one(cell(Bug, Row, Column, Color, StackPosition, InGame), []) :- 
%     not(can_move(cell(Bug, Row, Column, Color, StackPosition, InGame))).

% real_valid_moves_only_one(cell(Bug, Row, Column, Color, StackPosition, InGame), RealValidMoves) :- 
%     can_move(cell(Bug, Row, Column, Color, StackPosition, InGame)),
%     valid_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), RealValidMoves).
%     trying_moves_only_one(cell(Bug, Row, Column, Color, StackPosition, InGame), ValidMoves, RealValidMoves).


real_valid_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), []) :- 
    not(can_move(cell(Bug, Row, Column, Color, StackPosition, InGame))).

real_valid_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), RealValidMoves) :- 
    can_move(cell(Bug, Row, Column, Color, StackPosition, InGame)),
    valid_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), RealValidMoves).
    % trying_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), ValidMoves, RealValidMoves).
    
trying_moves(cell(_, _, _, _, _, _), [], []).

trying_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), [[MovesHR, MovesHC]| MovesT], FilterMoves) :-
    make_move(cell(Bug, Row, Column, Color, StackPosition, InGame), [MovesHR, MovesHC], _),
    try_move([MovesHR, MovesHC], ValidMoves1),
    make_move(cell(Bug, MovesHR, MovesHC, Color, StackPosition, InGame), [Row, Column], _),

    trying_moves(cell(Bug, Row, Column, Color, StackPosition, InGame), MovesT, ValidMoves2),
    append(ValidMoves1, ValidMoves2, FilterMoves).


try_move(X, [X]) :-
    hive_consitent(),!.

try_move(_, []).


% trying_moves_only_one(cell(_, _, _, _, _, _), [], []).

% trying_moves_only_one(cell(Bug, Row, Column, Color, StackPosition, InGame), [[MovesHR, MovesHC]| MovesT], X) :-
%     make_move(cell(Bug, Row, Column, Color, StackPosition, InGame), [MovesHR, MovesHC], _),
%     try_move([MovesHR, MovesHC], ValidMoves1),
%     make_move(cell(Bug, MovesHR, MovesHC, Color, StackPosition, InGame), [Row, Column], _),
%     trying_moves_only_one_2(ValidMoves1, cell(Bug, Row, Column, Color, StackPosition, InGame), [[MovesHR, MovesHC]| MovesT], X).

% trying_moves_only_one_2([_|_], cell(_, _, _, _, _, _), [[MovesHR, MovesHC]| _], [[MovesHR, MovesHC]]).

% trying_moves_only_one_2([], cell(Bug, Row, Column, Color, StackPosition, InGame), [[_, _]| MovesT], X) :-
%     trying_moves_only_one(cell(Bug, Row, Column, Color, StackPosition, InGame), MovesT, X).