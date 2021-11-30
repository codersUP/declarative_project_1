:- module(bug_can_put, [bug_can_put/3]).
:- use_module(game).

bug_can_put(Color, Turn, [queen]) :-
    Turn > 6,
    not(cell(queen, _, _, Color, _, true)).

bug_can_put(Color, _, BugCanPut) :-
    findall(B, cell(B, _, _, Color, _, false), BugCanPut1),
    sort(BugCanPut1, BugCanPut).