:- module(cell_can_play, [cell_can_play/3]).
:- use_module(game).

cell_can_play(Color, Turn, [queen]) :-
    Turn > 6,
    not(cell(queen, _, _, Color, _, true)).

cell_can_play(Color, _, CellCanPlay) :-
    findall(B, cell(B, _, _, Color, _, false), CellCanPlay1),
    sort(CellCanPlay1, CellCanPlay).