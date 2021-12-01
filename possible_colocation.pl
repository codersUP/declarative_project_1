:- module(possible_colocation, [possible_colocation/2, not_neighbors/2]).
:- use_module(game).


possible_colocation(_, [[0, 0]]) :-
    findall([X,Y], cell(_, X, Y, _, _, true), Cells),
    Cells = [].

possible_colocation(_, [[-1, 0], [-1, 1], [0, 1], [1, 0], [1, -1], [0, -1]]) :-
    findall([X,Y], cell(_, X, Y, _, _, true), Cells),
    Cells = [[_, _]].

possible_colocation(Color, Colocations) :-
    free_spots(FreeSpots),
    possible_colocation2(FreeSpots, Color, Colocations).


possible_colocation2([], _, []).

possible_colocation2([[SpotHR, SpotHC]| SpotT], Color, Possible) :-
    possible_colocation3(cell(_, SpotHR, SpotHC, Color, _, _), PC1),
    possible_colocation2(SpotT, Color, PC2),
    append(PC1, PC2, Possible).


possible_colocation3(cell(_, R, C, Color, _, _), []) :-
    not(hive_colocation_color(cell(_, R, C, Color, _, _))).

possible_colocation3(cell(_, R, C, Color, _, _), [[R, C]]) :-
    hive_colocation_color(cell(_, R, C, Color, _, _)).


free_spots(Spots) :-
    findall([X,Y], cell(_, X, Y, _, _, true), Cells),
    free_spots2(Cells, Spots).

free_spots2([], []).

free_spots2([[CellsHR, CellsHC]| CellsT], NotNeighborsCells) :- 
    not_neighbors(cell(_, CellsHR, CellsHC, _, _, _), FS1),
    free_spots2(CellsT, FS2),
    append(FS1, FS2, FST),
    sort(FST, NotNeighborsCells).


not_neighbors(cell(_, Row, Column, _, _, _), NotNeighbors) :-
    R1 is Row - 1, C1 is Column,     not_neighbors2(cell(_, R1, C1, _, _, true), NN1),
    R2 is Row - 1, C2 is Column + 1, not_neighbors2(cell(_, R2, C2, _, _, true), NN2),
    R3 is Row,     C3 is Column - 1, not_neighbors2(cell(_, R3, C3, _, _, true), NN3),
    R4 is Row,     C4 is Column + 1, not_neighbors2(cell(_, R4, C4, _, _, true), NN4),
    R5 is Row + 1, C5 is Column - 1, not_neighbors2(cell(_, R5, C5, _, _, true), NN5),
    R6 is Row + 1, C6 is Column,     not_neighbors2(cell(_, R6, C6, _, _, true), NN6),
    append([NN1, NN2, NN3, NN4, NN5, NN6], NotNeighbors).

not_neighbors2(cell(_, R, C, _, _, true), []):-
    cell(_, R, C, _, _, true).

not_neighbors2(cell(_, R, C, _, _, true), [[R, C]]):-
    not(cell(_, R, C, _, _, true)).


hive_colocation_color(cell(_, Row, Column, Color, _, _)) :-
    R1 is Row - 1, C1 is Column,
    not((cell(_, R1, C1, Color1, _, true), not(Color1 = Color))),
    R2 is Row - 1, C2 is Column + 1, 
    not((cell(_, R2, C2, Color2, _, true), not(Color2 = Color))),
    R3 is Row,     C3 is Column - 1,
    not((cell(_, R3, C3, Color3, _, true), not(Color3 = Color))),
    R4 is Row,     C4 is Column + 1,
    not((cell(_, R4, C4, Color4, _, true), not(Color4 = Color))),
    R5 is Row + 1, C5 is Column - 1,
    not((cell(_, R5, C5, Color5, _, true), not(Color5 = Color))),
    R6 is Row + 1, C6 is Column,
    not((cell(_, R6, C6, Color6, _, true), not(Color6 = Color))).