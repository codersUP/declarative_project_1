% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(power, [pillbug_power_can_apply/2, pillbug_power_can_apply_only_one/2]).
:- use_module(game).
:- use_module(valid_moves).
:- use_module(possible_colocation).
:- use_module(stack).
:- use_module(make_move).
:- use_module(real_valid_moves).
:- use_module(can_move).


pillbug_power_can_apply_only_one(cell(_, Row, Column, _, _, true), PowerCanApply) :-
    neighbors(cell(_, Row, Column, _, _, _), Neighbors),
    filter_list_by_stack_only_one(Neighbors, NeighborsWithOnlyOne),
    filter_list_not_last_move(NeighborsWithOnlyOne, FilteredNotLastMove),
    filter_bugs_dont_break_hive_only_one(FilteredNotLastMove, PowerCanApply).


pillbug_power_can_apply(cell(_, Row, Column, _, _, true), PowerCanApply) :-
    neighbors(cell(_, Row, Column, _, _, _), Neighbors),
    filter_list_by_stack_only_one(Neighbors, NeighborsWithOnlyOne),
    filter_list_not_last_move(NeighborsWithOnlyOne, FilteredNotLastMove),
    filter_bugs_dont_break_hive(FilteredNotLastMove, PowerCanApply).

    
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


filter_bugs_dont_break_hive([], []).

filter_bugs_dont_break_hive([[BugRH,BugCH]|BugsTail], FilteredBugs) :-
    filter_bugs_dont_break_hive2([BugRH, BugCH], Filtered1),
    filter_bugs_dont_break_hive(BugsTail, Filtered2),
    append(Filtered1, Filtered2, FilteredBugs).

filter_bugs_dont_break_hive2([BugRH, BugCH], []) :-
    cell(Bug, BugRH, BugCH, Color, StackPosition, true),
    not(can_move(cell(Bug, BugRH, BugCH, Color, StackPosition, true))).

filter_bugs_dont_break_hive2([BugRH, BugCH], [[BugRH, BugCH]]) :-
    cell(Bug, BugRH, BugCH, Color, StackPosition, true),
    can_move(cell(Bug, BugRH, BugCH, Color, StackPosition, true)).


filter_bugs_dont_break_hive_only_one([], []).

filter_bugs_dont_break_hive_only_one([[BugRH,BugCH]|_], [BugRH, BugCH]) :-
    filter_bugs_dont_break_hive2([BugRH, BugCH], Filtered1),
    Filtered1 = [_|_].

filter_bugs_dont_break_hive_only_one([[_,_]|BugsTail], Filtered2) :-
    filter_bugs_dont_break_hive_only_one(BugsTail, Filtered2).