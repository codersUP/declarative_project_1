:- module(game, [cell/6, play_now/1]).
:- use_module(change_turn).
:- use_module(bug_can_put).
:- use_module(bug_can_move).
:- use_module(play_can_do).
:- use_module(possible_colocation).
:- use_module(real_valid_moves).


cell(queen, 0, 0, white, 0, true).
cell(ant,  _,  _, white, -1, false).
cell(ant,  _,  _, white, -2, false).
cell(ant,  _,  _, white, -3, false).
cell(ant,  _,  _, white, -4, false).
cell(ant,  _,  _, white, -5, false).
cell(ant,  _,  _, white, -6, false).
cell(ant,  _,  _, white, -7, false).
cell(ant,  _,  _, white, -8, false).
cell(ant,  _,  _, white, -9, false).

cell(queen, _, _, black, -1, false).
cell(ant,  _,  _, black, -1, false).
cell(ant,  _,  _, black, -2, false).
cell(ant,  _,  _, black, -3, false).
cell(ant,  _,  _, black, -4, false).
cell(ant,  _,  _, black, -5, false).
cell(ant,  _,  _, black, -6, false).
cell(ant,  _,  _, black, -7, false).
cell(ant,  _,  _, black, -8, false).
cell(ant,  _,  _, black, -9, false).

:- dynamic cell/6.

play_now(white).

:- dynamic play_now/1.

turn(1).

:- dynamic turn/1.

infinity_loop() :-
    play_now(Color),
    turn(Turn),

    play_can_do(Color, PlayCanDo),
    write(PlayCanDo),
    write("\n"),
    read(PlayToDo),
    nth1(PlayToDo, PlayCanDo, Play),
    write(Play),
    write("\n"),

    play(Play, Color, Turn),

    infinity_loop().


play(put, Color, Turn) :-
    bug_can_put(Color, Turn, BugCanPut),
    write(BugCanPut),
    write("\n"),
    read(BugToPut),
    nth1(BugToPut, BugCanPut, Bug),
    write(Bug),
    write("\n"),

    possible_colocation(Color, PossiblesColocations),
    write(PossiblesColocations),
    write("\n"),
    read(ColocationSelected),
    nth1(ColocationSelected, PossiblesColocations, Colocation),
    write(Colocation),
    write("\n").
    % poner al bicho y tal

play(move, Color, _) :-
    bug_can_move(Color, BugCanMove),
    write(BugCanMove),
    write("\n"),
    read(BugToMove),
    nth1(BugToMove, BugCanMove, [B, R, C, Color, Sp]),
    write([B, R, C, Color, Sp]),
    write("\n"),

    real_valid_moves(cell(B, R, C, Color, Sp, true), RealValidMoves),
    write(RealValidMoves),
    write("\n"),
    read(MoveToDo),
    nth1(MoveToDo, RealValidMoves, Move),
    write(Move),
    write("\n").
    %mover al bicho y tal