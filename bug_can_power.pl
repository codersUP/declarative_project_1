:- module(bug_can_power, [bug_can_power/2]).
:- use_module(game).
:- use_module(real_valid_moves).
:- use_module(power).

bug_can_power(Color, []) :-
    not(cell(pillbug, _, _, Color, _, true)).

bug_can_power(Color, BugCanPower) :-
    B = pillbug,
    findall([B, R, C, Color, Sp], cell(B, R, C, Color, Sp, true), Cells),
    bug_can_power2(Cells, BugCanPower).

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
