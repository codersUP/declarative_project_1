:- module(game, [cell/6, color_now/1, turn/1, last_move/1, player/2]).
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
:- use_module(init_cells).
:- use_module(expantions).
:- use_module(ai).
:- use_module(players).

:- dynamic cell/6.
:- dynamic color_now/1.
:- dynamic turn/1.
:- dynamic last_move/1.
:- dynamic player/2.

init_game() :-
    remove_all_cells(),
    remove_all_states(),

    Bugs = [queen, ant, grasshopper, beetle, spider],
    Amounts = [1, 3, 3, 2, 2],

    init_game_cells(Bugs, Amounts),
    select_expantions(),

    assert(color_now(white)),
    assert(turn(1)),
    assert(last_move(cell(quee, 0, 0, white, -1, false))),
    assert(player(1, human)),
    assert(player(2, human)),
    
    select_players(),
    enter_game().


enter_game() :-
    game_finished(Result),
    enter_game_2(Result).
    

enter_game() :-
    color_now(Color),
    turn(Turn),
    player_turn(Turn, Player),
    player(Player, PlayerType),

    write("Player turn: " + Player + " Player type: " + PlayerType + " Color: " + Color + " Turn: " + Turn + "\n"),
    begin_play(PlayerType, Color, Turn),

    change_turn(_),
    advance_turn(),

    write("----------------------------------------------------------\n"),!,
    enter_game().

enter_game_2(tie) :-
    write("TIE").

enter_game(Result) :-
    write("Winner " + Result).


begin_play(human, Color, Turn) :-
    select_play(Color, Turn).

begin_play(ai, Color, Turn) :-
    best_play_ai(Color, Turn, 4, _, Play),
    write(Play + "\n"),
    make_play_ai(Color, Play).


select_play(Color, _) :-
    play_can_do(Color, PlayCanDo),
    PlayCanDo = [],
    write("No play to do\n").

select_play(Color, Turn) :-
    play_can_do(Color, PlayCanDo),
    write("Select play to do " + PlayCanDo + "\n"),
    read(PlayToDo),
    nth1(PlayToDo, PlayCanDo, Play),
    write("Play selected " + Play + "\n"),
    
    play(Play, Color, Turn).

play(put, Color, Turn) :-
    bug_can_put(Color, Turn, BugCanPut),
    write("Select bug to put: " + BugCanPut + "\n"),
    read(BugToPut),
    nth1(BugToPut, BugCanPut, Bug),
    write("Bug selected: " + Bug + "\n"),

    possible_colocation(Color, PossiblesColocations),
    write("Select colocation: " + PossiblesColocations + "\n"),
    read(ColocationSelected),
    nth1(ColocationSelected, PossiblesColocations, [ColocationR, ColocationC]),
    write("Colocation selected: " + [ColocationR, ColocationC] + "\n"),

    select_cell_to_put(Color, Bug, [B, R, C, Color, Sp]),
    make_move(cell(B, R, C, Color, Sp, false), [ColocationR, ColocationC], SP_greater),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(B, ColocationR, ColocationC, Color, SP_greater, true))).

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

    make_move(cell(B, R, C, Color, Sp, true), [MoveR, MoveC], SP_greater),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(B, MoveR, MoveC, Color, SP_greater, true))).

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

    make_move(cell(BugP, BugR, BugC, BugColor, BugSp, true), [ColocationR, ColocationC], SP_greater),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(BugP, ColocationR, ColocationC, BugColor, SP_greater, true))).
