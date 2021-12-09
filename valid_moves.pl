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
:- module(valid_moves, [valid_moves/3, neighbors/2, bug_type_of_neighbors/2]).
:- use_module(game).
:- use_module(stack).


valid_moves(queen, cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    valid_moves_queen(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).

valid_moves(beetle, cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    valid_moves_beetle(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).

valid_moves(grasshopper, cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    valid_moves_grasshopper(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).

valid_moves(spider, cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    valid_moves_spider(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).

valid_moves(ant, cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    valid_moves_ant(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).

valid_moves(ladybug, cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    valid_moves_ladybug(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).

valid_moves(pillbug, cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    valid_moves_queen(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).

valid_moves(mosquito, cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    valid_moves_mosquito(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).


valid_moves_queen(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    retract(cell(Bug, Row, Column, Color, Sp, InGame)),
    R1 is Row - 1, C1 is Column,     moves_queen([Row, Column], [R1, C1], VM1),
    R2 is Row - 1, C2 is Column + 1, moves_queen([Row, Column], [R2, C2], VM2),
    R3 is Row,     C3 is Column - 1, moves_queen([Row, Column], [R3, C3], VM3),
    R4 is Row,     C4 is Column + 1, moves_queen([Row, Column], [R4, C4], VM4),
    R5 is Row + 1, C5 is Column - 1, moves_queen([Row, Column], [R5, C5], VM5),
    R6 is Row + 1, C6 is Column,     moves_queen([Row, Column], [R6, C6], VM6),
    assertz(cell(Bug, Row, Column, Color, Sp, InGame)),
    append([VM1, VM2, VM3, VM4, VM5, VM6], ValidMoves).

moves_queen([R, C], [RD, CD], []) :-
    cell(_, RD, CD, _, _, true);
    not(can_enter([R, C], [RD, CD]));
    neighbors(cell(_, RD, CD, _, _, _), X),
    X = [].

moves_queen([R, C], [RD, CD], [[RD, CD]]) :-
    not(cell(_, RD, CD, _, _, true)),
    can_enter([R, C], [RD, CD]),
    neighbors(cell(_, RD, CD, _, _, _), X),
    X = [_|_].


valid_moves_beetle(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    retract(cell(Bug, Row, Column, Color, Sp, InGame)),
    R1 is Row - 1, C1 is Column,    moves_beetle([R1, C1], VM1),
    R2 is Row - 1, C2 is Column + 1,moves_beetle([R2, C2], VM2),
    R3 is Row,     C3 is Column - 1,moves_beetle([R3, C3], VM3),
    R4 is Row,     C4 is Column + 1,moves_beetle([R4, C4], VM4),
    R5 is Row + 1, C5 is Column - 1,moves_beetle([R5, C5], VM5),
    R6 is Row + 1, C6 is Column,    moves_beetle([R6, C6], VM6),
    assertz(cell(Bug, Row, Column, Color, Sp, InGame)),
    append([VM1, VM2, VM3, VM4, VM5, VM6], ValidMoves).

moves_beetle([R, C], []) :-
    neighbors(cell(_, R, C, _, _, _), X),
    X = [].

moves_beetle([R, C], [[R, C]]) :-
    neighbors(cell(_, R, C, _, _, _), X),
    X = [_|_].


valid_moves_grasshopper(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    retract(cell(Bug, Row, Column, Color, Sp, InGame)),
    % left
    grasshopper_line(cell(_, Row, Column, _, _, _), 0, -1, VM1),
    % right
    grasshopper_line(cell(_, Row, Column, _, _, _), 0,  1, VM2),
    % up left
    grasshopper_line(cell(_, Row, Column, _, _, _), -1, 0, VM3),
    % up right
    grasshopper_line(cell(_, Row, Column, _, _, _), -1, 1, VM4),
    % down left
    grasshopper_line(cell(_, Row, Column, _, _, _), 1, -1, VM5),
    % down right
    grasshopper_line(cell(_, Row, Column, _, _, _), 1,  0, VM6),

    assertz(cell(Bug, Row, Column, Color, Sp, InGame)),

    append([VM1, VM2, VM3, VM4, VM5, VM6], ValidMoves).

grasshopper_line(cell(_, Row, Column, _, _, _), R, C, []) :-
    R1 is Row + R, C1 is Column + C,
    not(cell(_, R1, C1, _, _, true)),
    neighbors(cell(_, R1, C1, _, _, _), X),
    X = [].

grasshopper_line(cell(_, Row, Column, _, _, _), R, C, [[R1, C1]]) :-
    R1 is Row + R, C1 is Column + C,
    not(cell(_, R1, C1, _, _, true)),
    neighbors(cell(_, R1, C1, _, _, _), X),
    X = [_|_].
    
grasshopper_line(cell(_, Row, Column, _, _, _), R, C, ValidMoves) :-
    R1 is Row + R, C1 is Column + C,
    cell(_, R1, C1, _, _, true),
    grasshopper_line(cell(_, R1, C1, _, _, _), R, C, ValidMoves).


valid_moves_spider(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    retract(cell(Bug, Row, Column, Color, Sp, InGame)),
    spider_3_moves(cell(Bug, Row, Column, Color, Sp, InGame), 3, ValidMoves),
    assertz(cell(Bug, Row, Column, Color, Sp, InGame)).

spider_3_moves(cell(_, Row, Column, _, _, _), 0, [[Row, Column]]).

spider_3_moves(cell(_, Row, Column, _, _, _), Jump, ValidMoves) :-
    Jump > 0,
    J1 is Jump - 1,
    
    R1 is Row - 1, C1 is Column,
    spider_3_moves2([Row, Column], [R1, C1], J1, VM1),

    R2 is Row - 1, C2 is Column + 1,
    spider_3_moves2([Row, Column], [R2, C2], J1, VM2),

    R3 is Row,     C3 is Column - 1,
    spider_3_moves2([Row, Column], [R3, C3], J1, VM3),

    R4 is Row,     C4 is Column + 1,
    spider_3_moves2([Row, Column], [R4, C4], J1, VM4),

    R5 is Row + 1, C5 is Column - 1,
    spider_3_moves2([Row, Column], [R5, C5], J1, VM5),

    R6 is Row + 1, C6 is Column,
    spider_3_moves2([Row, Column], [R6, C6], J1, VM6),

    append([VM1, VM2, VM3, VM4, VM5, VM6], VM7),

    sort(VM7, ValidMoves).


spider_3_moves2([R, C], [RD, CD], _, []) :-
    neighbors(cell(_, RD, CD, _, _, _), Neighbors),
    Neighbors = [];
    cell(_, RD, CD, _, _, true);
    not(can_enter([R, C], [RD, CD])).

spider_3_moves2([R, C], [RD, CD], Jump, ValidMoves) :-
    neighbors(cell(_, RD, CD, _, _, _), Neighbors),
    not(Neighbors = []),
    not(cell(_, RD, CD, _, _, true)),
    can_enter([R, C], [RD, CD]),
    spider_3_moves(cell(_, RD, CD, _, _, _), Jump, ValidMoves).


neighbors(cell(_, Row, Column, _, _, _), Neighbors) :-
    R1 is Row - 1, C1 is Column,     (not(cell(_, R1, C1, _, _, true)) -> append([], VM1);           append([[[R1, C1]]], VM1)),
    R2 is Row - 1, C2 is Column + 1, (not(cell(_, R2, C2, _, _, true)) -> append([VM1], VM2);        append([[[R2, C2]], VM1], VM2)),
    R3 is Row,     C3 is Column - 1, (not(cell(_, R3, C3, _, _, true)) -> append([VM2], VM3);        append([[[R3, C3]], VM2], VM3)),
    R4 is Row,     C4 is Column + 1, (not(cell(_, R4, C4, _, _, true)) -> append([VM3], VM4);        append([[[R4, C4]], VM3], VM4)),
    R5 is Row + 1, C5 is Column - 1, (not(cell(_, R5, C5, _, _, true)) -> append([VM4], VM5);        append([[[R5, C5]], VM4], VM5)),
    R6 is Row + 1, C6 is Column,     (not(cell(_, R6, C6, _, _, true)) -> append([VM5], Neighbors); append([[[R6, C6]], VM5], Neighbors)).


valid_moves_ant(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    retract(cell(Bug, Row, Column, Color, Sp, InGame)),
    ant_moves(cell(Bug, Row, Column, Color, Sp, InGame), [], ValidMoves),
    assertz(cell(Bug, Row, Column, Color, Sp, InGame)).

ant_moves(cell(_, Row, Column, _, _, _), Moves, ValidMoves) :-
    R1 is Row - 1, C1 is Column,
    ant_moves2([R1, C1], Moves, VM1),

    R2 is Row - 1, C2 is Column + 1,
    ant_moves2([R2, C2], VM1, VM2),

    R3 is Row,     C3 is Column - 1,
    ant_moves2([R3, C3], VM2, VM3),

    R4 is Row,     C4 is Column + 1,
    ant_moves2([R4, C4], VM3, VM4),

    R5 is Row + 1, C5 is Column - 1,
    ant_moves2([R5, C5], VM4, VM5),

    R6 is Row + 1, C6 is Column,
    ant_moves2([R6, C6], VM5, ValidMoves).


ant_moves2([R, C], Moves, Moves) :-
    not(
        (neighbors(cell(_, R, C, _, _, _), Neighbors),
        not(member([R, C], Moves)),
        not(cell(_, R, C, _, _, true)),
        not(Neighbors = []))
        ).

ant_moves2([R, C], Moves, Result) :-
    neighbors(cell(_, R, C, _, _, _), Neighbors),
    not(member([R, C], Moves)),
    not(cell(_, R, C, _, _, true)),
    not(Neighbors = []),
    append([[R, C]], Moves, Moves1),
    ant_moves(cell(_, R, C, _, _, _), Moves1, Result).



valid_moves_ladybug(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    retract(cell(Bug, Row, Column, Color, Sp, InGame)),
    ladybug_3_moves(cell(_, Row, Column, _, _, _), 3, ValidMoves),
    assertz(cell(Bug, Row, Column, Color, Sp, InGame)).

ladybug_3_moves(cell(_, Row, Column, _, _, _), 0, [[Row, Column]]).

ladybug_3_moves(cell(_, Row, Column, _, _, _), 1, ValidMoves) :-
    J1 is 0,

    R1 is Row - 1, C1 is Column,
    ladybug_3_moves_land(R1, C1, J1, VM1),

    R2 is Row - 1, C2 is Column + 1,
    ladybug_3_moves_land(R2, C2, J1, VM2),

    R3 is Row,     C3 is Column - 1,
    ladybug_3_moves_land(R3, C3, J1, VM3),

    R4 is Row,     C4 is Column + 1,
    ladybug_3_moves_land(R4, C4, J1, VM4),

    R5 is Row + 1, C5 is Column - 1,
    ladybug_3_moves_land(R5, C5, J1, VM5),

    R6 is Row + 1, C6 is Column,
    ladybug_3_moves_land(R6, C6, J1, VM6),

    append([VM1, VM2, VM3, VM4, VM5, VM6], VM7),
    sort(VM7, ValidMoves).

ladybug_3_moves(cell(_, Row, Column, _, _, _), Jump, ValidMoves) :-
    Jump > 1,
    J1 is Jump - 1,

    R1 is Row - 1, C1 is Column,
    ladybug_3_moves_air(R1, C1, J1, VM1),

    R2 is Row - 1, C2 is Column + 1,
    ladybug_3_moves_air(R2, C2, J1, VM2),

    R3 is Row,     C3 is Column - 1,
    ladybug_3_moves_air(R3, C3, J1, VM3),

    R4 is Row,     C4 is Column + 1,
    ladybug_3_moves_air(R4, C4, J1, VM4),

    R5 is Row + 1, C5 is Column - 1,
    ladybug_3_moves_air(R5, C5, J1, VM5),

    R6 is Row + 1, C6 is Column,
    ladybug_3_moves_air(R6, C6, J1, VM6),

    append([VM1, VM2, VM3, VM4, VM5, VM6], VM7),
    sort(VM7, ValidMoves).

ladybug_3_moves_land(R, C, _, []) :-
    cell(_, R, C, _, _, true).

ladybug_3_moves_land(R, C, Jump, ValidMoves):-
    not(cell(_, R, C, _, _, true)),
    ladybug_3_moves(cell(_, R, C, _, _, _), Jump, ValidMoves).

ladybug_3_moves_air(R, C, _, []) :-
    not(cell(_, R, C, _, _, true)).

ladybug_3_moves_air(R, C, Jump, ValidMoves):-
    cell(_, R, C, _, _, true),
    ladybug_3_moves(cell(_, R, C, _, _, _), Jump, ValidMoves).


valid_moves_mosquito(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    Sp > 0,
    valid_moves_beetle(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves).

valid_moves_mosquito(cell(Bug, Row, Column, Color, Sp, InGame), ValidMoves) :-
    Sp = 0,
    neighbors(cell(_, Row, Column, _, _, _), Neighbors),
    bug_type_of_neighbors(Neighbors, BugTypeNeighbors),
    valid_moves_mosquito2(cell(Bug, Row, Column, Color, Sp, InGame), BugTypeNeighbors, ValidMoves).


valid_moves_mosquito2(cell(_, _, _, _, _, _), [], []).

valid_moves_mosquito2(cell(Bug, Row, Column, Color, Sp, InGame), [mosquito|BugTypeT], Moves) :-
    valid_moves_mosquito2(cell(Bug, Row, Column, Color, Sp, InGame), BugTypeT, Moves).

valid_moves_mosquito2(cell(Bug, Row, Column, Color, Sp, InGame), [BugTypeH|BugTypeT], Moves) :-
    valid_moves(BugTypeH, cell(Bug, Row, Column, Color, Sp, InGame), Moves1),
    valid_moves_mosquito2(cell(Bug, Row, Column, Color, Sp, InGame), BugTypeT, Moves2),
    append(Moves1, Moves2, Moves3),
    sort(Moves3, Moves).

bug_type_of_neighbors([], []).

bug_type_of_neighbors([[NHR, NHC]|NeighborsT], BugType) :-
    bug_on_top(NHR, NHC, Bug1),
    bug_type_of_neighbors(NeighborsT, Bug2),
    append([Bug1], Bug2, BugType1),
    sort(BugType1, BugType).


can_enter([R1, C1], [R2, C2]) :-
    R2 is R1,
    C2 is C1 + 1,

    RE1 is R1 - 1,
    CE1 is C1 + 1,

    RE2 is R1 + 1,
    CE2 is C1,

    not((cell(_, RE1, CE1, _, _, true) , cell(_, RE2, CE2, _, _, true))).

can_enter([R1, C1], [R2, C2]) :-
    R2 is R1 - 1,
    C2 is C1 + 1,

    RE1 is R1 - 1,
    CE1 is C1,

    RE2 is R1,
    CE2 is C1 + 1,

    not((cell(_, RE1, CE1, _, _, true) , cell(_, RE2, CE2, _, _, true))).

can_enter([R1, C1], [R2, C2]) :-
    R2 is R1 - 1,
    C2 is C1,

    RE1 is R1,
    CE1 is C1 - 1,

    RE2 is R1 - 1,
    CE2 is C1 + 1,

    not((cell(_, RE1, CE1, _, _, true) , cell(_, RE2, CE2, _, _, true))).

can_enter([R1, C1], [R2, C2]) :-
    R2 is R1,
    C2 is C1 - 1,

    RE1 is R1 + 1,
    CE1 is C1 - 1,

    RE2 is R1 - 1,
    CE2 is C1,

    not((cell(_, RE1, CE1, _, _, true) , cell(_, RE2, CE2, _, _, true))).

can_enter([R1, C1], [R2, C2]) :-
    R2 is R1 + 1,
    C2 is C1 - 1,

    RE1 is R1 + 1,
    CE1 is C1,

    RE2 is R1,
    CE2 is C1 - 1,

    not((cell(_, RE1, CE1, _, _, true) , cell(_, RE2, CE2, _, _, true))).

can_enter([R1, C1], [R2, C2]) :-
    R2 is R1 + 1,
    C2 is C1,

    RE1 is R1,
    CE1 is C1 + 1,

    RE2 is R1 + 1,
    CE2 is C1 - 1,

    not((cell(_, RE1, CE1, _, _, true) , cell(_, RE2, CE2, _, _, true))).