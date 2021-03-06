:- module(win, [game_finished/1, game_result/1]).
:- use_module(possible_colocation).
:- use_module(game).

game_finished(tie) :-
    game_finished(white),
    game_finished(black).

game_finished(white) :-
    cell(queen, R, C, black, SP, true),
    not_neighbors(cell(queen, R, C, black, SP, true), RVM_black),
    RVM_black = [].

game_finished(black) :-
    cell(queen, R, C, white, SP, true),
    not_neighbors(cell(queen, R, C, white, SP, true), RVM_white),
    RVM_white = [].

game_result(Result) :-
    game_finished(Result).

game_result(continue) :-
    not(game_finished(_)).