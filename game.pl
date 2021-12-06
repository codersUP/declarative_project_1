:- module(game, [cell/6, color_now/1, turn/1, last_move/1]).
:- use_module(change_turn).
:- use_module(bug_can_put).
:- use_module(bug_can_move).
:- use_module(bug_can_power).
:- use_module(play_can_do).
:- use_module(possible_colocation).
:- use_module(real_valid_moves).
:- use_module(power).
:- use_module(make_move).
:- use_module(win).


cell(queen, 1,  0, white,  0, true).
% cell(ant,   1,  2, white,  0, true).
% cell(ant,   0,  3, white,  0, true).
% cell(ant,  -1,  3, white,  0, false).
% cell(ladybug,  0,  0, white,  0, true).
% cell(spider,   1,  0, white,  0, true).
% cell(grasshopper,   2,  1, white,  0, true).
% cell(mosquito,   3,  0, white,  0, true).
% cell(pillbug,   0,  1, white,  0, true).
% cell(ant,   2,  0, white,  0, true).

cell(queen, 2,  -1, black,  0, true).
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

last_move(cell(queen, 2, -1, black, 0, true)).

:- dynamic last_move/1.


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

    select_play(Color),

    change_turn(_),
    advance_turn(),

    write("----------------------------------------------------------\n"),
    enter_game().


select_play(Color) :-
    play_can_do(Color, PlayCanDo),
    write("Select play to do " + PlayCanDo + "\n"),
    read(PlayToDo),
    nth1(PlayToDo, PlayCanDo, Play),
    write("Play selected " + Play + "\n"),
    turn(Turn),
    
    play(Play, Color, Turn).

play(put, Color, Turn) :-
    bug_can_put(Color, Turn, BugCanPut),
    write("Select bug tu put: " + BugCanPut + "\n"),
    read(BugToPut),
    nth1(BugToPut, BugCanPut, Bug),
    write("Bug selected: " + Bug + "\n"),

    possible_colocation(Color, PossiblesColocations),
    write("Select colocation: " + PossiblesColocations + "\n"),
    read(ColocationSelected),
    nth1(ColocationSelected, PossiblesColocations, [ColocationR, ColocationC]),
    write("Colocation selected: " + [ColocationR, ColocationC] + "\n"),

    select_cell_to_put(Color, Bug, [B, R, C, Color, Sp]),
    make_move(cell(B, R, C, Color, Sp, false), [ColocationR, ColocationC]),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(B, ColocationR, ColocationC, Color, Sp, true))).

play(move, Color, _) :-
    bug_can_move(Color, BugCanMove),
    write("Select bug to move: " + BugCanMove + "\n"),
    read(BugToMove),
    nth1(BugToMove, BugCanMove, [B, R, C, Color, Sp]),
    write("Bug selected " + [B, R, C, Color, Sp] + "\n"),

    real_valid_moves(cell(B, R, C, Color, Sp, true), RealValidMoves),
    write("Select move to do " + RealValidMoves + "\n"),
    read(MoveToDo),
    nth1(MoveToDo, RealValidMoves, [MoveR, MoveC]),
    write("Move selected: " + [MoveR, MoveC] + "\n"),

    make_move(cell(B, R, C, Color, Sp, true), [MoveR, MoveC]),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(B, MoveR, MoveC, Color, Sp, true))).

play(power, Color, _) :-
    bug_can_power(Color, BugCanPower),
    write("Select bug to use power: " + BugCanPower + "\n"),
    read(BugToPower),
    nth1(BugToPower, BugCanPower, [B, R, C, Color, Sp]),
    write("Bug selected: " + [B, R, C, Color, Sp] + "\n"),

    pillbug_power_can_apply(cell(B, R, C, Color, Sp, true), BugsPowerCanApply),
    write("Select bug to apply power: " + BugsPowerCanApply + "\n"),
    read(BugToApplyPower),
    nth1(BugToApplyPower, BugsPowerCanApply, [BugR, BugC]),
    write("Bug selected: " + [BugR, BugC] + "\n"),

    not_neighbors(cell(B, R, C, Color, Sp, true), PossiblesColocations),
    write("Select colocation: " + PossiblesColocations + "\n"),
    read(ColocationSelected),
    nth1(ColocationSelected, PossiblesColocations, [ColocationR, ColocationC]),
    write("Colocation selected: " + [ColocationR, ColocationC] + "\n"),

    cell(BugP, BugR, BugC, BugColor, BugSp, true),

    make_move(cell(BugP, BugR, BugC, BugColor, BugSp, true), [ColocationR, ColocationC]),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(BugP, ColocationR, ColocationC, BugColor, BugSp, true))).
