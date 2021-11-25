% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(hive_consitent, [hive_consitent/0]).
:- use_module(game).

delete(Element,[Element|Tail],Tail).

delete(Element,[Head|Tail],[Head|Tail1]) :-
    delete(Element,Tail,Tail1).

hive_consitent() :- not(cell(_, _, _, _, _, true)).

hive_consitent() :-
    cell(_, R, C, _, _, true),
    findall([X,Y], cell(_, X, Y, _, _, true), Cells),
    delete([R, C], Cells, Cells1),
    hive_dfs(cell(_, R, C, _, _, true), Cells1, UntouchedCells),
    UntouchedCells = [].
    

hive_dfs(cell(_, Row, Column, _, _, true), Cells, UntouchedCells) :-
    R1 is Row - 1, C1 is Column,
    (member([R1, C1], Cells) -> (delete([R1, C1], Cells, Cells12), hive_dfs(cell(_, R1, C1, _, _, true), Cells12, Cells1)); append([Cells], Cells1)),
    R2 is Row - 1, C2 is Column + 1, 
    (member([R2, C2], Cells1) -> (delete([R2, C2], Cells1, Cells22), hive_dfs(cell(_, R2, C2, _, _, true), Cells22, Cells2)); append([Cells1], Cells2)),
    R3 is Row,     C3 is Column - 1,
    (member([R3, C3], Cells2) -> (delete([R3, C3], Cells2, Cells32), hive_dfs(cell(_, R3, C3, _, _, true), Cells32, Cells3)); append([Cells2], Cells3)),
    R4 is Row,     C4 is Column + 1,
    (member([R4, C4], Cells3) -> (delete([R4, C4], Cells3, Cells42), hive_dfs(cell(_, R4, C4, _, _, true), Cells42, Cells4)); append([Cells3], Cells4)),
    R5 is Row + 1, C5 is Column - 1,
    (member([R5, C5], Cells4) -> (delete([R5, C5], Cells4, Cells52), hive_dfs(cell(_, R5, C5, _, _, true), Cells52, Cells5)); append([Cells4], Cells5)),
    R6 is Row + 1, C6 is Column,
    (member([R6, C6], Cells5) -> (delete([R6, C6], Cells5, Cells62), hive_dfs(cell(_, R6, C6, _, _, true), Cells62, UntouchedCells)); append([Cells5], UntouchedCells)).


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