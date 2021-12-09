% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(can_move, [can_move/1]).
:- use_module(game).
:- use_module(real_valid_moves).

can_move(cell(Bug, Row, Column, Color, StackPosition, true)) :-
    not((cell(_, Row, Column, _, SP, true), SP > StackPosition)),
    cell(queen, _, _, Color, _, true),

    retract(cell(Bug, Row, Column, Color, StackPosition, true)),
    try_move(cell(Bug, Row, Column, Color, StackPosition, true), X),
    assertz(cell(Bug, Row, Column, Color, StackPosition, true)),

    X = [_|_].