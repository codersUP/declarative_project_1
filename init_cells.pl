:- module(init_cells, [init_game_cells/2]).
:- use_module(game).

init_cells(_, _, 0).

init_cells(Bug, Color, Amount) :-
    Sp is -1 * Amount,

    assert(cell(Bug, 0, 0, Color, Sp, false)),

    A1 is Amount - 1,

    init_cells(Bug, Color, A1).
    

init_game_cells([], []).

init_game_cells([BugsH|BugsT], [AmountsH|AmountsT]) :-
    init_cells(BugsH, white, AmountsH),
    init_cells(BugsH, black, AmountsH),

    init_game_cells(BugsT, AmountsT).