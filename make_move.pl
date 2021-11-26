% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(make_move, [make_move/2]).
:- use_module(game).

make_move(cell(Bug, Row, Column, Color, StackPosition, true), [MoveR, MoveC]) :-
    findall(SP, cell(_, Row, Column, _, SP, true), SPs),
    sort(SPs, SPs_sorted),
    reverse(SPs_sorted, SPs_sorted_reverse),
    get_first_or_0(SPs_sorted_reverse, SP_greater),
    retract(cell(Bug, Row, Column, Color, StackPosition, true)),
    assertz(cell(Bug, MoveR, MoveC, Color, SP_greater, true)).
    

get_first_or_0([], 0).
get_first_or_0([LH, LT], LH).
