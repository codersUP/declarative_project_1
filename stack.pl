% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(stack, [on_top/1, stack_with_only_one/2, filter_list_by_stack_only_one/2, bug_on_top/3]).
:- use_module(game).
:- use_module(utils).

on_top(cell(_, Row, Column, _, StackPosition, true)) :-
    findall(SP, cell(_, Row, Column, _, SP, true), SPs),
    sort(SPs, SPs_sorted),
    reverse(SPs_sorted, SPs_sorted_reverse),
    get_first_or_0(SPs_sorted_reverse, SP_greater),
    SP_greater = StackPosition.

bug_on_top(Row, Column, Bug) :-
    findall(SP, cell(_, Row, Column, _, SP, true), SPs),
    sort(SPs, SPs_sorted),
    reverse(SPs_sorted, SPs_sorted_reverse),
    get_first_or_0(SPs_sorted_reverse, SP_greater),
    cell(Bug, Row, Column, _, SP_greater, true).


stack_with_only_one(Row, Column) :-
    findall(SP, cell(_, Row, Column, _, SP, true), SPs),
    SPs = [_].


filter_list_by_stack_only_one([], []).

filter_list_by_stack_only_one([[RH, RC]| Tail], Filtered) :-
    filter_by_stack_only_one(RH, RC, Filtered1),
    filter_list_by_stack_only_one(Tail, Filtered2),
    append(Filtered1, Filtered2, Filtered).

filter_by_stack_only_one(Row, Column, [[Row, Column]]) :-
    stack_with_only_one(Row, Column),!.

filter_by_stack_only_one(Row, Column, []).
