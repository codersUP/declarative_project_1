:- module(bug_can_move, [bug_can_move/2, bug_can_move_only_one/2]).
:- use_module(game).
:- use_module(real_valid_moves).

bug_can_move_only_one(Color, []) :-
    not(cell(queen, _, _, Color, _, true)).

bug_can_move_only_one(Color, BugCanMove) :-
    findall([B, R, C, Color, Sp], cell(B, R, C, Color, Sp, true), Cells),
    bug_can_move2_only_one(Cells, BugCanMove).

bug_can_move(Color, []) :-
    not(cell(queen, _, _, Color, _, true)).

bug_can_move(Color, BugCanMove) :-
    findall([B, R, C, Color, Sp], cell(B, R, C, Color, Sp, true), Cells),
    bug_can_move2(Cells, BugCanMove).

bug_can_move2([], []).

bug_can_move2([[B, R, C, Color, Sp]|CellsT], BugCanMove) :-
    bug_can_move3([B, R, C, Color, Sp], BugCanMove1),
    bug_can_move2(CellsT, BugCanMove2),
    append(BugCanMove1, BugCanMove2, BugCanMove).
    
    
bug_can_move3([B, R, C, Color, Sp], X) :-
    real_valid_moves_only_one(cell(B, R, C, Color, Sp, true), RealValidMoves),
    bug_can_move3_2(RealValidMoves, [B, R, C, Color, Sp], X).

bug_can_move3_2([], _, []).

bug_can_move3_2([_|_], X, [X]).

bug_can_move2_only_one([], []).

bug_can_move2_only_one([[B, R, C, Color, Sp]|CellsT], X) :-
    real_valid_moves_only_one(cell(B, R, C, Color, Sp, true), RealValidMoves),
    bug_can_move2_only_one_2(RealValidMoves, [[B, R, C, Color, Sp]|CellsT], X).

bug_can_move2_only_one_2([_|_], [[B, R, C, Color, Sp]|_], [[B, R, C, Color, Sp]]).

bug_can_move2_only_one_2([], [[_, _, _, _, _]|CellsT], X) :-
    bug_can_move2_only_one(CellsT, X).
