% cell(BugType, Row, Column, Color, StackPosition, InGame)

:-module(real_valid_moves, [real_valid_moves/2]).

:- use_module(can_move).
:- use_module(game).
:- use_module(valid_moves).


real_valid_moves(cell(Bug, Row, Column, _, StackPosition, InGame), []) :- 
    not(can_move(cell(Bug, Row, Column, _, StackPosition, InGame))).

real_valid_moves(cell(Bug, Row, Column, _, StackPosition, InGame), RealValidMoves) :- 
    can_move(cell(Bug, Row, Column, _, StackPosition, InGame)),
    valid_moves(cell(Bug, Row, Column, _, StackPosition, InGame), RealValidMoves).
    
% trying_moves(cell(Bug, Row, Column, _, StackPosition, InGame)).
    
