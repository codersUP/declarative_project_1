% cell(BugType, Row, Column, Color, StackPosition, InGame)

cell(1, 1, 1, 1, 1, 1).
cell(1, 2, 1, 1, 1, 1).
cell(1, 4, 1, 2, 1, 1).

delete(Element,[Element|Tail],Tail).

delete(Element,[Head|Tail],[Head|Tail1]) :-
    delete(Element,Tail,Tail1).

hive_consitent() :- not(cell(_, _, _, _, _, _)).

hive_consitent() :-
    cell(_, R, C, _, _, _),
    findall([X,Y], cell(_, X, Y, _, _, _), Cells),
    delete([R, C], Cells, Cells1),
    hive_dfs(cell(_, R, C, _, _, _), Cells1, UntouchedCells),
    UntouchedCells = [].
    

hive_dfs(cell(_, Row, Column, _, _, _), Cells, UntouchedCells) :-
    R1 is Row - 1, C1 is Column,
    (member([R1, C1], Cells) -> (delete([R1, C1], Cells, Cells12), hive_dfs(cell(_, R1, C1, _, _, _), Cells12, Cells1)); append([Cells], Cells1)),
    R2 is Row - 1, C2 is Column + 1, 
    (member([R2, C2], Cells1) -> (delete([R2, C2], Cells1, Cells22), hive_dfs(cell(_, R2, C2, _, _, _), Cells22, Cells2)); append([Cells1], Cells2)),
    R3 is Row,     C3 is Column - 1,
    (member([R3, C3], Cells2) -> (delete([R3, C3], Cells2, Cells32), hive_dfs(cell(_, R3, C3, _, _, _), Cells32, Cells3)); append([Cells2], Cells3)),
    R4 is Row,     C4 is Column + 1,
    (member([R4, C4], Cells3) -> (delete([R4, C4], Cells3, Cells42), hive_dfs(cell(_, R4, C4, _, _, _), Cells42, Cells4)); append([Cells3], Cells4)),
    R5 is Row + 1, C5 is Column - 1,
    (member([R5, C5], Cells4) -> (delete([R5, C5], Cells4, Cells52), hive_dfs(cell(_, R5, C5, _, _, _), Cells52, Cells5)); append([Cells4], Cells5)),
    R6 is Row + 1, C6 is Column,
    (member([R6, C6], Cells5) -> (delete([R6, C6], Cells5, Cells62), hive_dfs(cell(_, R6, C6, _, _, _), Cells62, UntouchedCells)); append([Cells5], UntouchedCells)).


hive_colocation_color(cell(_, Row, Column, Color, _, _)) :-
    R1 is Row - 1, C1 is Column,
    not((cell(_, R1, C1, Color1, _, _), Color1 =\= Color)),
    R2 is Row - 1, C2 is Column + 1, 
    not((cell(_, R2, C2, Color2, _, _), Color2 =\= Color)),
    R3 is Row,     C3 is Column - 1,
    not((cell(_, R3, C3, Color3, _, _), Color3 =\= Color)),
    R4 is Row,     C4 is Column + 1,
    not((cell(_, R4, C4, Color4, _, _), Color4 =\= Color)),
    R5 is Row + 1, C5 is Column - 1,
    not((cell(_, R5, C5, Color5, _, _), Color5 =\= Color)),
    R6 is Row + 1, C6 is Column,
    not((cell(_, R6, C6, Color6, _, _), Color6 =\= Color)).
