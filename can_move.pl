% cell(BugType, Row, Column, Color, StackPosition, InGame)
cell(1, 1, 1, 1, 1, 1).
cell(1, 1, 1, 1, 2, 1).


can_move(cell(_, Row, Column, _, StackPosition, _)) :-
    not((cell(_, Row, Column, _, SP, _), SP > StackPosition)).