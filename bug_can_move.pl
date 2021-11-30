:- module(bug_can_move, [bug_can_move/2]).
:- use_module(game).
:- use_module(real_valid_moves).

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
    
    
bug_can_move3([B, R, C, Color, Sp], []) :-
    real_valid_moves(cell(B, R, C, Color, Sp, true), RealValidMoves),
    RealValidMoves = [].

bug_can_move3([B, R, C, Color, Sp], [[B, R, C, Color, Sp]]) :-
    real_valid_moves(cell(B, R, C, Color, Sp, true), RealValidMoves),
    not(RealValidMoves = []).
