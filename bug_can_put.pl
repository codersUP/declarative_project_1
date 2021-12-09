:- module(bug_can_put, [bug_can_put/3, select_cell_to_put/3]).
:- use_module(game).

bug_can_put(Color, Turn, [queen]) :-
    Turn > 2,
    not(cell(queen, _, _, Color, _, true)).

bug_can_put(Color, _, BugCanPut) :-
    findall(B, cell(B, _, _, Color, _, false), BugCanPut1),
    sort(BugCanPut1, BugCanPut).


select_cell_to_put(Color, Bug, Cell) :-
    findall(SP, cell(Bug, 0, 0, Color, SP, false), SPs),
    sort(SPs, SPs_sorted),
    get_first_or_0(SPs_sorted, SP_lower),
    Cell = [Bug, 0, 0, Color, SP_lower].

get_first_or_0([], 0).
get_first_or_0([LH|_], LH).