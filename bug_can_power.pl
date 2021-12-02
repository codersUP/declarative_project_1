:- module(bug_can_power, [bug_can_power/2]).
:- use_module(game).
:- use_module(real_valid_moves).
:- use_module(power).
:- use_module(valid_moves).


bug_can_power(Color, BugCanPower) :-
    B = pillbug,
    findall([B, R, C, Color, Sp], cell(B, R, C, Color, Sp, true), Cells),
    bug_can_power2(Cells, BugCanPower1),

    B2 = mosquito,
    findall([B2, R2, C2, Color, Sp2], cell(B2, R2, C2, Color, Sp2, true), Cells2),
    search_for_neighbors_pillbug(Cells2, Cells2Pillbugs),
    bug_can_power2(Cells2Pillbugs, BugCanPower2),

    append(BugCanPower1, BugCanPower2, BugCanPower).

bug_can_power2([], []).

bug_can_power2([[B, R, C, Color, Sp]|CellsT], BugCanPower) :-
    bug_can_power3([B, R, C, Color, Sp], BugCanPower1),
    bug_can_power2(CellsT, BugCanPower2),
    append(BugCanPower1, BugCanPower2, BugCanPower).
    
    
bug_can_power3([B, R, C, Color, Sp], []) :-
    pillbug_power_can_apply(cell(B, R, C, Color, Sp, true), PowerCanApply),
    PowerCanApply = [].

bug_can_power3([B, R, C, Color, Sp], [[B, R, C, Color, Sp]]) :-
    pillbug_power_can_apply(cell(B, R, C, Color, Sp, true), PowerCanApply),
    not(PowerCanApply = []).


search_for_neighbors_pillbug([], []).

search_for_neighbors_pillbug([[B, R, C, Color, Sp]|CellsT], Filtered) :-
    search_for_neighbors_pillbug2([B, R, C, Color, Sp], Filtered1),
    search_for_neighbors_pillbug(CellsT, Filtered2),
    append(Filtered1, Filtered2, Filtered).

search_for_neighbors_pillbug2([_, R, C, _, _], []):-
    neighbors(cell(_, R, C, _, _, _), Neighbors),
    bug_type_of_neighbors(Neighbors, BugTypeNeighbors),
    not(member(pillbug, BugTypeNeighbors)).

search_for_neighbors_pillbug2([B, R, C, Color, Sp], [[B, R, C, Color, Sp]]):-
    neighbors(cell(_, R, C, _, _, _), Neighbors),
    bug_type_of_neighbors(Neighbors, BugTypeNeighbors),
    member(pillbug, BugTypeNeighbors).