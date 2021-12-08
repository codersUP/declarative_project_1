:- module(bug_can_power, [bug_can_power/2, bug_can_power_only_one/2]).
:- use_module(game).
:- use_module(real_valid_moves).
:- use_module(power).
:- use_module(valid_moves).


bug_can_power_only_one(Color, BugCanPower1) :-
    B = pillbug,
    findall([B, R, C, Color, Sp], cell(B, R, C, Color, Sp, true), Cells),
    bug_can_power2_only_one(Cells, BugCanPower1),
    BugCanPower1 = [_|_].

bug_can_power_only_one(Color, BugCanPower2) :-
    B2 = mosquito,
    findall([B2, R2, C2, Color, Sp2], cell(B2, R2, C2, Color, Sp2, true), Cells2),
    search_for_neighbors_pillbug(Cells2, Cells2Pillbugs),
    bug_can_power2_only_one(Cells2Pillbugs, BugCanPower2).


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
    
    
bug_can_power3([B, R, C, Color, Sp], X) :-
    pillbug_power_can_apply_only_one(cell(B, R, C, Color, Sp, true), PowerCanApply),
    bug_can_power3_2(PowerCanApply, [B, R, C, Color, Sp], X).

bug_can_power3_2([], _, []).

bug_can_power3_2([_|_], X, [X]).


bug_can_power2_only_one([], []).

bug_can_power2_only_one([[B, R, C, Color, Sp]|_], [[B, R, C, Color, Sp]]) :-
    pillbug_power_can_apply_only_one(cell(B, R, C, Color, Sp, true), PowerCanApply),
    PowerCanApply = [_|_].

bug_can_power2_only_one([[_, _, _, _, _]|CellsT], X)  :-
    bug_can_power2_only_one(CellsT, X).


search_for_neighbors_pillbug([], []).

search_for_neighbors_pillbug([[B, R, C, Color, Sp]|CellsT], Filtered) :-
    search_for_neighbors_pillbug2([B, R, C, Color, Sp], Filtered1),
    search_for_neighbors_pillbug(CellsT, Filtered2),
    append(Filtered1, Filtered2, Filtered).

search_for_neighbors_pillbug2([B, R, C, Color, Sp], X):-
    neighbors(cell(_, R, C, _, _, _), Neighbors),
    bug_type_of_neighbors(Neighbors, BugTypeNeighbors),
    search_for_neighbors_pillbug2_2(BugTypeNeighbors, [B, R, C, Color, Sp], X).

search_for_neighbors_pillbug2_2(BugTypeNeighbors, _, []) :-
    not(member(pillbug, BugTypeNeighbors)).

search_for_neighbors_pillbug2_2(BugTypeNeighbors, X, [X]) :-
    member(pillbug, BugTypeNeighbors).