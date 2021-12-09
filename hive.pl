% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(hive_consitent, [hive_consitent/0]).
:- use_module(game).

delete(Element,[Element|Tail],Tail).

delete(Element,[Head|Tail],[Head|Tail1]) :-
    delete(Element,Tail,Tail1).

hive_consitent() :- not(cell(_, _, _, _, _, true)).

hive_consitent() :-
    findall([X,Y], cell(_, X, Y, _, _, true), [[R, C]| Cells]),
    hive_dfs(cell(_, R, C, _, _, true), Cells, UntouchedCells),
    UntouchedCells = [].
    

hive_dfs(cell(_, Row, Column, _, _, true), Cells, UntouchedCells) :-
    R1 is Row - 1, C1 is Column,
    hive_dfs_visit([R1, C1], Cells, Cells1),
    R2 is Row - 1, C2 is Column + 1,
    hive_dfs_visit([R2, C2], Cells1, Cells2),
    R3 is Row,     C3 is Column - 1,
    hive_dfs_visit([R3, C3], Cells2, Cells3),
    R4 is Row,     C4 is Column + 1,
    hive_dfs_visit([R4, C4], Cells3, Cells4),
    R5 is Row + 1, C5 is Column - 1,
    hive_dfs_visit([R5, C5], Cells4, Cells5),
    R6 is Row + 1, C6 is Column,
    hive_dfs_visit([R6, C6], Cells5, UntouchedCells).


hive_dfs_visit([R, C], Cells, UntouchedCells) :-
    member([R, C], Cells),
    delete([R, C], Cells, D),!,

    hive_dfs(cell(_, R, C, _, _, true), D, UntouchedCells).

hive_dfs_visit([R, C], Cells, Cells) :-
    not(member([R, C], Cells)).