% (r-1, s)
% (r-1, s+1)
% (r, s-1)
% (r, s+1)
% (r+1, s-1)
% (r+1, s)

%      (-1, 0) (-1, 1)
% (0, -1)  (0, 0)  (0, 1)
%      (1, -1) (1, 0)

% cell(BugType, Row, Column, Color, StackPosition, InGame)
:- module(valid_moves, [valid_moves/2, neighbors/2, bug_type_of_neighbors/2]).
:- use_module(game).
:- use_module(stack).


valid_moves(cell(queen, Row, Column, _, _, _), ValidMoves) :-
    valid_moves_queen(cell(_, Row, Column, _, _, _), ValidMoves).
valid_moves(cell(beetle, Row, Column, _, _, _), ValidMoves) :-
    valid_moves_beetle(cell(_, Row, Column, _, _, _), ValidMoves).
valid_moves(cell(grasshopper, Row, Column, _, _, _), ValidMoves) :-
    valid_moves_grasshopper(cell(_, Row, Column, _, _, _), ValidMoves).
valid_moves(cell(spider, Row, Column, _, _, _), ValidMoves) :-
    valid_moves_spider(cell(_, Row, Column, _, _, _), ValidMoves).
valid_moves(cell(ant, Row, Column, _, _, _), ValidMoves) :-
    valid_moves_ant(cell(_, Row, Column, _, _, _), ValidMoves).
valid_moves(cell(ladybug, Row, Column, _, _, _), ValidMoves) :-
    valid_moves_ladybug(cell(_, Row, Column, _, _, _), ValidMoves).
valid_moves(cell(pillbug, Row, Column, _, _, _), ValidMoves) :-
    valid_moves_queen(cell(_, Row, Column, _, _, _), ValidMoves).
valid_moves(cell(mosquito, Row, Column, _, StackPosition, _), ValidMoves) :-
    valid_moves_mosquito(cell(_, Row, Column, _, StackPosition, _), ValidMoves).


valid_moves_queen(cell(_, Row, Column, _, _, _), ValidMoves) :-
    R1 is Row - 1, C1 is Column,     (cell(_, R1, C1, _, _, true) -> append([], VM1);           append([[[R1, C1]]], VM1)),
    R2 is Row - 1, C2 is Column + 1, (cell(_, R2, C2, _, _, true) -> append([VM1], VM2);        append([[[R2, C2]], VM1], VM2)),
    R3 is Row,     C3 is Column - 1, (cell(_, R3, C3, _, _, true) -> append([VM2], VM3);        append([[[R3, C3]], VM2], VM3)),
    R4 is Row,     C4 is Column + 1, (cell(_, R4, C4, _, _, true) -> append([VM3], VM4);        append([[[R4, C4]], VM3], VM4)),
    R5 is Row + 1, C5 is Column - 1, (cell(_, R5, C5, _, _, true) -> append([VM4], VM5);        append([[[R5, C5]], VM4], VM5)),
    R6 is Row + 1, C6 is Column,     (cell(_, R6, C6, _, _, true) -> append([VM5], ValidMoves); append([[[R6, C6]], VM5], ValidMoves)).


valid_moves_beetle(cell(_, Row, Column, _, _, _), ValidMoves) :-
    R1 is Row - 1, C1 is Column,   
    R2 is Row - 1, C2 is Column + 1,
    R3 is Row,     C3 is Column - 1,
    R4 is Row,     C4 is Column + 1,
    R5 is Row + 1, C5 is Column - 1,
    R6 is Row + 1, C6 is Column,
    append([[[R1, C1], [R2, C2], [R3, C3], [R4, C4], [R5, C5], [R6, C6]]], ValidMoves).


valid_moves_grasshopper(cell(_, Row, Column, _, _, _), ValidMoves) :-
    % left
    grasshopper_line(cell(_, Row, Column, _, _, _), 0, -1, [], VM1),
    % right
    grasshopper_line(cell(_, Row, Column, _, _, _), 0, 1, VM1, VM2),
    % up left
    grasshopper_line(cell(_, Row, Column, _, _, _), -1, 0, VM2, VM3),
    % up right
    grasshopper_line(cell(_, Row, Column, _, _, _), -1, 1, VM3, VM4),
    % down left
    grasshopper_line(cell(_, Row, Column, _, _, _), 1, -1, VM4, VM5),
    % down right
    grasshopper_line(cell(_, Row, Column, _, _, _), 1, 0, VM5, ValidMoves).

grasshopper_line(cell(_, Row, Column, _, _, _), R, C, Moves, ValidMoves) :-
    R1 is Row + R, C1 is Column + C, (cell(_, R1, C1, _, _, true) -> 
        grasshopper_line(cell(_, R1, C1, _, _, _), R, C, Moves, ValidMoves);
        append([[[R1, C1]], Moves], ValidMoves)).
    

valid_moves_spider(cell(_, Row, Column, _, _, _), ValidMoves) :-
    spider_3_moves(cell(_, Row, Column, _, _, _), 3, [], ValidMoves).

spider_3_moves(cell(_, Row, Column, _, _, _), Jump, Moves, ValidMoves) :-
    (Jump = 0 -> append([[[Row, Column]], Moves], ValidMoves);(
    J1 is Jump - 1,
    
    R1 is Row - 1, C1 is Column,     
    neighbors(cell(_, R1, C1, _, _, _), L1),
    ((not(cell(_, R1, C1, _, _, true)), not(L1 = [])) -> spider_3_moves(cell(_, R1, C1, _, _, _), J1, Moves, VM1); append([Moves], VM1)),

    R2 is Row - 1, C2 is Column + 1, 
    neighbors(cell(_, R2, C2, _, _, _), L2),
    ((not(cell(_, R2, C2, _, _, true)), not(L2 = [])) -> spider_3_moves(cell(_, R2, C2, _, _, _), J1, VM1, VM2); append([[], VM1], VM2)),

    R3 is Row,     C3 is Column - 1, 
    neighbors(cell(_, R3, C3, _, _, _), L3),
    ((not(cell(_, R3, C3, _, _, true)), not(L3 = [])) -> spider_3_moves(cell(_, R3, C3, _, _, _), J1, VM2, VM3); append([[], VM2], VM3)),

    R4 is Row,     C4 is Column + 1, 
    neighbors(cell(_, R4, C4, _, _, _), L4),
    ((not(cell(_, R4, C4, _, _, tru)), not(L4 = [])) -> spider_3_moves(cell(_, R4, C4, _, _, _), J1, VM3, VM4); append([[], VM3], VM4)),

    R5 is Row + 1, C5 is Column - 1, 
    neighbors(cell(_, R5, C5, _, _, _), L5),
    ((not(cell(_, R5, C5, _, _, true)), not(L5 = [])) -> spider_3_moves(cell(_, R5, C5, _, _, _), J1, VM4, VM5); append([[], VM4], VM5)),

    R6 is Row + 1, C6 is Column,
    neighbors(cell(_, R6, C6, _, _, _), L6),
    ((not(cell(_, R6, C6, _, _, true)), not(L6 = [])) -> spider_3_moves(cell(_, R6, C6, _, _, _), J1, VM5, VM6); append([[], VM5], VM6)),

    sort(VM6, ValidMoves)
    )).


neighbors(cell(_, Row, Column, _, _, _), Neighbors) :-
    R1 is Row - 1, C1 is Column,     (not(cell(_, R1, C1, _, _, true)) -> append([], VM1);           append([[[R1, C1]]], VM1)),
    R2 is Row - 1, C2 is Column + 1, (not(cell(_, R2, C2, _, _, true)) -> append([VM1], VM2);        append([[[R2, C2]], VM1], VM2)),
    R3 is Row,     C3 is Column - 1, (not(cell(_, R3, C3, _, _, true)) -> append([VM2], VM3);        append([[[R3, C3]], VM2], VM3)),
    R4 is Row,     C4 is Column + 1, (not(cell(_, R4, C4, _, _, true)) -> append([VM3], VM4);        append([[[R4, C4]], VM3], VM4)),
    R5 is Row + 1, C5 is Column - 1, (not(cell(_, R5, C5, _, _, true)) -> append([VM4], VM5);        append([[[R5, C5]], VM4], VM5)),
    R6 is Row + 1, C6 is Column,     (not(cell(_, R6, C6, _, _, true)) -> append([VM5], Neighbors); append([[[R6, C6]], VM5], Neighbors)).


valid_moves_ant(cell(_, Row, Column, _, _, _), ValidMoves) :-
    ant_moves(cell(_, Row, Column, _, _, _), [], ValidMoves).

ant_moves(cell(_, Row, Column, _, _, _), Moves, ValidMoves) :-
    R1 is Row - 1, C1 is Column,     
    neighbors(cell(_, R1, C1, _, _, _), L1),
    ((not(member([R1, C1], Moves)), not(cell(_, R1, C1, _, _, true)), not(L1 = [])) -> (append([[[R1, C1]], Moves], VM11), ant_moves(cell(_, R1, C1, _, _, _), VM11, VM1)); append([Moves], VM1)),

    R2 is Row - 1, C2 is Column + 1, 
    neighbors(cell(_, R2, C2, _, _, _), L2),
    ((not(member([R2, C2], VM1)), not(cell(_, R2, C2, _, _, true)), not(L2 = [])) -> (append([[[R2, C2]], VM1], VM21), ant_moves(cell(_, R2, C2, _, _, _), VM21, VM2)); append([[], VM1], VM2)),

    R3 is Row,     C3 is Column - 1, 
    neighbors(cell(_, R3, C3, _, _, _), L3),
    ((not(member([R3, C3], VM2)), not(cell(_, R3, C3, _, _, true)), not(L3 = [])) -> (append([[[R3, C3]], VM2], VM31), ant_moves(cell(_, R3, C3, _, _, _), VM31, VM3)); append([[], VM2], VM3)),

    R4 is Row,     C4 is Column + 1, 
    neighbors(cell(_, R4, C4, _, _, _), L4),
    ((not(member([R4, C4], VM3)), not(cell(_, R4, C4, _, _, true)), not(L4 = [])) -> (append([[[R4, C4]], VM3], VM41), ant_moves(cell(_, R4, C4, _, _, _), VM41, VM4)); append([[], VM3], VM4)),

    R5 is Row + 1, C5 is Column - 1, 
    neighbors(cell(_, R5, C5, _, _, _), L5),
    ((not(member([R5, C5], VM4)), not(cell(_, R5, C5, _, _, true)), not(L5 = [])) -> (append([[[R5, C5]], VM4], VM51), ant_moves(cell(_, R5, C5, _, _, _), VM51, VM5)); append([[], VM4], VM5)),

    R6 is Row + 1, C6 is Column,
    neighbors(cell(_, R6, C6, _, _, _), L6),
    ((not(member([R6, C6], VM5)), not(cell(_, R6, C6, _, _, true)), not(L6 = [])) -> (append([[[R6, C6]], VM5], VM61), ant_moves(cell(_, R1, C1, _, _, _), VM61, ValidMoves)); append([[], VM5], ValidMoves)).


valid_moves_ladybug(cell(_, Row, Column, _, _, _), ValidMoves) :-
    ladybug_3_moves(cell(_, Row, Column, _, _, _), 3, [], ValidMoves).

ladybug_3_moves(cell(_, Row, Column, _, _, _), 0, Moves, ValidMoves) :-
    append([[[Row, Column]], Moves], ValidMoves).

ladybug_3_moves(cell(_, Row, Column, _, _, _), 1, Moves, ValidMoves) :-
    J1 is 0,

    R1 is Row - 1, C1 is Column,     
    (not(cell(_, R1, C1, _, _, true)) -> ladybug_3_moves(cell(_, R1, C1, _, _, _), J1, Moves, VM1); append([Moves], VM1)),

    R2 is Row - 1, C2 is Column + 1, 
    (not(cell(_, R2, C2, _, _, true)) -> ladybug_3_moves(cell(_, R2, C2, _, _, _), J1, VM1, VM2); append([[], VM1], VM2)),

    R3 is Row,     C3 is Column - 1, 
    (not(cell(_, R3, C3, _, _, true)) -> ladybug_3_moves(cell(_, R3, C3, _, _, _), J1, VM2, VM3); append([[], VM2], VM3)),

    R4 is Row,     C4 is Column + 1,
    (not(cell(_, R4, C4, _, _, true)) -> spider_3_moves(cell(_, R4, C4, _, _, _), J1, VM3, VM4); append([[], VM3], VM4)),

    R5 is Row + 1, C5 is Column - 1,
    (not(cell(_, R5, C5, _, _, true)) -> spider_3_moves(cell(_, R5, C5, _, _, _), J1, VM4, VM5); append([[], VM4], VM5)),

    R6 is Row + 1, C6 is Column,
    (not(cell(_, R6, C6, _, _, true)) -> spider_3_moves(cell(_, R6, C6, _, _, _), J1, VM5, VM6); append([[], VM5], VM6)),

    sort(VM6, ValidMoves).

ladybug_3_moves(cell(_, Row, Column, _, _, _), Jump, Moves, ValidMoves) :-
    J1 is Jump - 1,

    R1 is Row - 1, C1 is Column,
    (cell(_, R1, C1, _, _, true) -> ladybug_3_moves(cell(_, R1, C1, _, _, _), J1, Moves, VM1); append([Moves], VM1)),

    R2 is Row - 1, C2 is Column + 1,
    (cell(_, R2, C2, _, _, true) -> ladybug_3_moves(cell(_, R2, C2, _, _, _), J1, VM1, VM2); append([[], VM1], VM2)),

    R3 is Row,     C3 is Column - 1,
    (cell(_, R3, C3, _, _, true) -> ladybug_3_moves(cell(_, R3, C3, _, _, _), J1, VM2, VM3); append([[], VM2], VM3)),

    R4 is Row,     C4 is Column + 1, 
    (cell(_, R4, C4, _, _, true) -> ladybug_3_moves(cell(_, R4, C4, _, _, _), J1, VM3, VM4); append([[], VM3], VM4)),

    R5 is Row + 1, C5 is Column - 1,
    (cell(_, R5, C5, _, _, true) -> ladybug_3_moves(cell(_, R5, C5, _, _, _), J1, VM4, VM5); append([[], VM4], VM5)),

    R6 is Row + 1, C6 is Column,
    (cell(_, R6, C6, _, _, true) -> ladybug_3_moves(cell(_, R6, C6, _, _, _), J1, VM5, VM6); append([[], VM5], VM6)),

    sort(VM6, ValidMoves).


valid_moves_mosquito(cell(_, Row, Column, _, StackPosition, _), ValidMoves) :-
    StackPosition > 0,
    valid_moves_beetle(cell(_, Row, Column, _, _, _), ValidMoves).

valid_moves_mosquito(cell(_, Row, Column, _, StackPosition, _), ValidMoves) :-
    StackPosition = 0,
    neighbors(cell(_, Row, Column, _, _, _), Neighbors),
    bug_type_of_neighbors(Neighbors, BugTypeNeighbors),
    valid_moves_mosquito2(cell(_, Row, Column, _, _, _), BugTypeNeighbors, ValidMoves).


valid_moves_mosquito2(cell(_, _, _, _, _, _), [], []).

valid_moves_mosquito2(cell(_, Row, Column, _, _, _), [mosquito|BugTypeT], Moves) :-
    valid_moves_mosquito2(cell(_, Row, Column, _, _, _), BugTypeT, Moves).

valid_moves_mosquito2(cell(_, Row, Column, _, _, _), [BugTypeH|BugTypeT], Moves) :-
    valid_moves(cell(BugTypeH, Row, Column, _, _, _), Moves1),
    valid_moves_mosquito2(cell(_, Row, Column, _, _, _), BugTypeT, Moves2),
    append(Moves1, Moves2, Moves3),
    sort(Moves3, Moves).

bug_type_of_neighbors([], []).

bug_type_of_neighbors([[NHR, NHC]|NeighborsT], BugType) :-
    bug_on_top(NHR, NHC, Bug1),
    bug_type_of_neighbors(NeighborsT, Bug2),
    append([Bug1], Bug2, BugType1),
    sort(BugType1, BugType).