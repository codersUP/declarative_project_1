:- module(init_cells, [init_game_cells/2, remove_all_cells/0, remove_all_states/0]).
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

remove_all_cells() :-
    findall([Bug, Row, Column, Color, Sp, InGame], cell(Bug, Row, Column, Color, Sp, InGame), Cells),
    remove_cell(Cells).

remove_cell([]).

remove_cell([[Bug, Row, Column, Color, Sp, InGame]| Cells]) :-
    retract(cell(Bug, Row, Column, Color, Sp, InGame)),
    remove_cell(Cells).

remove_all_states() :-
    findall(Color, color_now(Color), Colors),
    remove_color(Colors),

    findall(Turn, turn(Turn), Turns),
    remove_turn(Turns),

    findall([C, R, Column, Col, Sp, InGame], last_move(cell(C, R, Column, Col, Sp, InGame)), Cells),
    remove_last_move(Cells),

    findall([N, Player], player(N, Player), Players),
    remove_player(Players).

remove_color([]).

remove_color([Color|Colors]) :-
    retract(color_now(Color)),
    remove_color(Colors).

remove_turn([]).

remove_turn([Turn|Turns]) :-
    retract(turn(Turn)),
    remove_turn(Turns).

remove_last_move([]).

remove_last_move([[C, R, Column, Col, Sp, InGame]|Cells]) :-
    retract(last_move(cell(C, R, Column, Col, Sp, InGame))),
    remove_last_move(Cells).

remove_player([]).

remove_player([[N, Player]|Players]) :-
    retract(player(N, Player)),
    remove_player(Players).