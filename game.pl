:- module(game, [cell/6, color_now/1, turn/1]).
:- use_module(change_turn).
:- use_module(bug_can_put).
:- use_module(bug_can_move).
:- use_module(play_can_do).
:- use_module(possible_colocation).
:- use_module(real_valid_moves).
:- use_module(make_move).
:- use_module(win).


cell(queen, 1,  1, white,  0, true).
cell(ant,   1,  2, white,  0, true).
cell(ant,   0,  3, white,  0, true).
cell(ant,  -1,  3, white,  0, true).
cell(ant,  -1,  2, white,  0, true).
cell(ant,   1,  0, white,  0, true).
% cell(ant,   0,  0, white,  0, true).
% cell(ant,   0,  0, white,  0, true).
% cell(ant,   0,  0, white,  0, true).
% cell(ant,   0,  0, white,  0, true).

cell(queen, 0,  2, black,  0, true).
% cell(ant,   0,  0, black, -1, false).
% cell(ant,   0,  0, black, -2, false).
% cell(ant,   0,  0, black, -3, false).
% cell(ant,   0,  0, black, -4, false).
% cell(ant,   0,  0, black, -5, false).
% cell(ant,   0,  0, black, -6, false).
% cell(ant,   0,  0, black, -7, false).
% cell(ant,   0,  0, black, -8, false).
% cell(ant,   0,  0, black, -9, false).

:- dynamic cell/6.

color_now(white).

:- dynamic color_now/1.

turn(1).

:- dynamic turn/1.


enter_game() :-
    game_finished(Result),
    Result = tie,
    write("TIE").

enter_game() :-
    game_finished(Result),
    write("Winner " + Result).

enter_game() :-
    color_now(Color),
    turn(Turn),
    write("Color: " + Color + " Turn: " + Turn + "\n"),

    play_can_do(Color, PlayCanDo),
    write("Select play to do " + PlayCanDo + "\n"),
    read(PlayToDo),
    nth1(PlayToDo, PlayCanDo, Play),
    write("Play selected " + Play + "\n"),

    play(Play, Color, Turn),

    change_turn(_),
    advance_turn(),

    write("----------------------------------------------------------\n"),
    enter_game().


play(put, Color, Turn) :-
    bug_can_put(Color, Turn, BugCanPut),
    write("Select bug tu put: " + BugCanPut + "\n"),
    read(BugToPut),
    nth1(BugToPut, BugCanPut, Bug),
    write("Bug selected: " + Bug + "\n"),

    possible_colocation(Color, PossiblesColocations),
    write("Select colocation: " + PossiblesColocations + "\n"),
    read(ColocationSelected),
    nth1(ColocationSelected, PossiblesColocations, Colocation),
    write("Colocation selected: " + Colocation + "\n"),

    select_cell_to_put(Color, Bug, [B, R, C, Color, Sp]),
    make_move(cell(B, R, C, Color, Sp, false), Colocation).



play(move, Color, _) :-
    bug_can_move(Color, BugCanMove),
    write("Select bug to move: " + BugCanMove + "\n"),
    read(BugToMove),
    nth1(BugToMove, BugCanMove, [B, R, C, Color, Sp]),
    write("Bug selected " + [B, R, C, Color, Sp] + "\n"),

    real_valid_moves(cell(B, R, C, Color, Sp, true), RealValidMoves),
    write("Select move to do " + RealValidMoves + "\n"),
    read(MoveToDo),
    nth1(MoveToDo, RealValidMoves, Move),
    write("Move selected: " + Move + "\n"),

    make_move(cell(B, R, C, Color, Sp, true), Move).