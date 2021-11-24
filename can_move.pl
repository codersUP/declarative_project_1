% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(can_move, [can_move/1]).
:- use_module(game).

can_move(cell(_, Row, Column, _, StackPosition, true)) :-
    not((cell(_, Row, Column, _, SP, true), SP > StackPosition)).