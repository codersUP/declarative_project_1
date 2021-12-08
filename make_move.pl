% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(make_move, [make_move/3]).
:- use_module(game).
:- use_module(utils).

make_move(cell(Bug, Row, Column, Color, StackPosition, InGame), [MoveR, MoveC], SP_greater) :-
    findall(SP, cell(_, MoveR, MoveC, _, SP, true), SPs),
    sort(SPs, SPs_sorted),
    reverse(SPs_sorted, SPs_sorted_reverse),
    get_first_or_0(SPs_sorted_reverse, SP_greater),
    retract(cell(Bug, Row, Column, Color, StackPosition, InGame)),
    assertz(cell(Bug, MoveR, MoveC, Color, SP_greater, true)).