% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(power, [pillbug_power_can_apply/2, pillbug_power_can_apply_only_one/2]).
:- use_module(game).
:- use_module(valid_moves).
:- use_module(possible_colocation).
:- use_module(stack).
:- use_module(make_move).
:- use_module(real_valid_moves).


pillbug_power_can_apply_only_one(cell(_, Row, Column, _, _, true), PowerCanApply) :-
    neighbors(cell(_, Row, Column, _, _, _), Neighbors),
    filter_list_by_stack_only_one(Neighbors, NeighborsWithOnlyOne),
    filter_list_not_last_move(NeighborsWithOnlyOne, FilteredNotLastMove),
    not_neighbors(cell(_, Row, Column, _, _, _), NotNeighbors),
    filter_bugs_dont_break_hive_only_one(FilteredNotLastMove, NotNeighbors, PowerCanApply).


pillbug_power_can_apply(cell(_, Row, Column, _, _, true), PowerCanApply) :-
    neighbors(cell(_, Row, Column, _, _, _), Neighbors),
    filter_list_by_stack_only_one(Neighbors, NeighborsWithOnlyOne),
    filter_list_not_last_move(NeighborsWithOnlyOne, FilteredNotLastMove),
    not_neighbors(cell(_, Row, Column, _, _, _), NotNeighbors),
    filter_bugs_dont_break_hive(FilteredNotLastMove, NotNeighbors, PowerCanApply).

    
filter_list_not_last_move([], []).

filter_list_not_last_move([[RH, CH]|Tail], Filtered) :-
    was_last_move(RH, CH, Filtered1),
    filter_list_not_last_move(Tail, Filtered2),
    append(Filtered1, Filtered2, Filtered).

was_last_move(Row, Column, []) :-
    last_move(cell(_, R, C, _, _, _)),
    Row = R,
    Column = C.

was_last_move(Row, Column, [[Row, Column]]) :-
    last_move(cell(_, R, C, _, _, _)),
    not((Row = R, Column = C)).


filter_bugs_dont_break_hive([], _, []).

filter_bugs_dont_break_hive([[BugRH,BugCH]|BugsTail], PossbleMoves, FilteredBugs) :-
    filter_bugs_dont_break_hive_move([BugRH, BugCH], PossbleMoves, Filtered1),
    filter_bugs_dont_break_hive(BugsTail, PossbleMoves, Filtered2),
    append(Filtered1, Filtered2, FilteredBugs).


filter_bugs_dont_break_hive_move(_, [], []).

filter_bugs_dont_break_hive_move([BugR, BugC], [[PMRH, PMCH]|PMT], FilteredMoves) :-
    cell(Bug, BugR, BugC, Color, StackPosition, true),
    make_move(cell(Bug, BugR, BugC, Color, StackPosition, true), [PMRH, PMCH]),
    try_move([BugR, BugC], Filtered1),
    make_move(cell(Bug, PMRH, PMCH, Color, StackPosition, true), [BugR, BugC]),

    filter_bugs_dont_break_hive_move([BugR, BugC], PMT, Filtered2),
    append(Filtered1, Filtered2, Filtered3),
    sort(Filtered3, FilteredMoves).

filter_bugs_dont_break_hive_only_one([], _, []).

filter_bugs_dont_break_hive_only_one([[BugRH,BugCH]|_], PossbleMoves, Filtered1) :-
    filter_bugs_dont_break_hive_move_only_one([BugRH, BugCH], PossbleMoves, Filtered1),
    Filtered1 = [_|_].

filter_bugs_dont_break_hive_only_one([[_,_]|BugsTail], PossbleMoves, Filtered2) :-
    filter_bugs_dont_break_hive_only_one(BugsTail, PossbleMoves, Filtered2).

filter_bugs_dont_break_hive_move_only_one(_, [], []).

filter_bugs_dont_break_hive_move_only_one([BugR, BugC], [[PMRH, PMCH]|_], Filtered1) :-
    cell(Bug, BugR, BugC, Color, StackPosition, true),
    make_move(cell(Bug, BugR, BugC, Color, StackPosition, true), [PMRH, PMCH]),
    try_move([BugR, BugC], Filtered1),
    make_move(cell(Bug, PMRH, PMCH, Color, StackPosition, true), [BugR, BugC]),
    Filtered1 = [_|_].

filter_bugs_dont_break_hive_move_only_one([BugR, BugC], [[_, _]|PMT], Filtered2) :-
    filter_bugs_dont_break_hive_move_only_one([BugR, BugC], PMT, Filtered2).