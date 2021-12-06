:- use_module(game).
:- use_module(bug_can_put).
:- use_module(make_move).
:- use_module(win).
:- use_module(color).
:- use_module(bug_can_move).
:- use_module(play_can_do).
:- use_module(possible_colocation).
:- use_module(real_valid_moves).
:- use_module(bug_can_power).
:- use_module(power).
:- use_module(utils).
:- use_module(stack).


make_play_ia(Color, [put, BugToPut, [LocationR, LocationC]]) :-
    select_cell_to_put(Color, BugToPut, [BugToPut, R, C, Color, Sp]),
    make_move(cell(BugToPut, R, C, Color, Sp, false), [LocationR, LocationC]),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(BugToPut, LocationR, LocationC, Color, Sp, true))).    

make_play_ia(Color, [move, [Bug, R, C, Color, Sp], [LocationR, LocationC]]) :-
    make_move(cell(Bug, R, C, Color, Sp, true), [LocationR, LocationC]),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(Bug, LocationR, LocationC, Color, Sp, true))).


make_play_ia(_, [power, [_, _, _, _, _], [BugToApplyPowerR, BugToApplyPowerC], [LocationR, LocationC]]) :-
    cell(BugP, BugToApplyPowerR, BugToApplyPowerC, BugColor, BugSp, true),

    make_move(cell(BugP, BugToApplyPowerR, BugToApplyPowerC, BugColor, BugSp, true), [LocationR, LocationC]),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(BugP, LocationR, LocationC, BugColor, BugSp, true))).


retract_play_ia(Color, [put, _, _]) :-
    last_move(cell(Bug, LocationR, LocationC, Color, Sp, true)),

    findall(SpInHand, cell(Bug, _, _, Color, SpInHand, false), Cells),
    sort(Cells, CellsSorted),
    get_first_or_0(CellsSorted, SPLower),

    SPL1 is SPLower - 1,

    retract(cell(Bug, LocationR, LocationC, Color, Sp, true)),
    assertz(cell(Bug, 0, 0, Color, SPL1, false)).

retract_play_ia(Color, [move, [Bug, R, C, Color, _], _]) :-
    last_move(cell(Bug, LocationR, LocationC, Color, Sp, true)),

    make_move(cell(Bug, LocationR, LocationC, Color, Sp, true), [R, C]).

retract_play_ia(Color, [power, [_, _, _, _, _], [BugToApplyPowerR, BugToApplyPowerC], _]) :-
    last_move(cell(Bug, LocationR, LocationC, Color, Sp, true)),

    make_move(cell(Bug, LocationR, LocationC, Color, Sp, true), [BugToApplyPowerR, BugToApplyPowerC]).



heuristic(Color, _, Play, _, 100) :-
    last_move(cell(LB, LR, LC, LColor, LSp, LInGame)),
    make_play_ia(Color, Play),
    game_result(Result),
    retract_play_ia(Color, Play),
    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(cell(LB, LR, LC, LColor, LSp, LInGame)),
    Result = Color.

heuristic(Color, _, Play, _, -100) :-
    last_move(cell(LB, LR, LC, LColor, LSp, LInGame)),
    make_play_ia(Color, Play),
    game_result(Result),
    retract_play_ia(Color, Play),
    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(cell(LB, LR, LC, LColor, LSp, LInGame)),
    not(Result = continue),
    not(Result = Color).

heuristic(Color, _, Play, 0, Points) :-
    last_move(cell(LB, LR, LC, LColor, LSp, LInGame)),
    make_play_ia(Color, Play),
    
    opposite_color(Color, RivalColor),
    how_many_no_moves(RivalColor, Points),
    
    retract_play_ia(Color, Play),
    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(cell(LB, LR, LC, LColor, LSp, LInGame)).
    

heuristic(Color, Turn, Play, Depth, Points) :-
    Depth > 0,
    
    last_move(cell(LB, LR, LC, LColor, LSp, LInGame)),
    make_play_ia(Color, Play),
    not(game_finished(_)),
    
    opposite_color(Color, RivalColor),
    how_many_no_moves(RivalColor, NoMoves),
    D1 is Depth - 1,
    T1 is Turn + 1,
    best_play_ia(RivalColor, T1, D1, RivalPoints),

    retract_play_ia(Color, Play),
    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(cell(LB, LR, LC, LColor, LSp, LInGame)),

    Points is NoMoves - RivalPoints.


best_play_ia(Color, Turn, Depth, Points) :-
    generates_plays_ia(Color, Turn, Plays),
    select_best_play(Color, Turn, Depth, Plays, Points).


select_best_play(_, _, [], -999999).

select_best_play(Color, Turn, Depth, [PlayH| PlayT], BestPoint) :-
    heuristic(Color, Turn, PlayH, Depth, Point1),
    select_best_play(Color, Depth, PlayT, Point2),
    append(Point1, Point2, Point3),
    reverse(Point3, [BestPoint|_]).


how_many_no_moves(Color, NoMoves) :-
    findall([B, R, C, Color, Sp], cell(B, R, C, Color, Sp, true), Cells),
    bug_can_move(Color, BugCanMove),
    length(Cells, CellsL),
    length(BugCanMove, BugCanMoveL),
    NoMoves is CellsL - BugCanMoveL.


generates_plays_ia(Color, Turn, Plays) :-
    play_can_do(Color, PlayCanDo),
    develop_plays_ia(Color, Turn, PlayCanDo, Plays).


develop_plays_ia(_, _, [], []).

develop_plays_ia(Color, Turn, [PlaysH|PlaysT], Plays) :-
    develop_play_ia(Color, Turn, PlaysH, Plays1),
    develop_plays_ia(Color,Turn, PlaysT, Plays2),
    append(Plays1, Plays2, Plays).

develop_play_ia(Color, Turn, put, Plays) :-
    bug_can_put(Color, Turn, BugCanPut),
    develop_can_put_ia(Color, BugCanPut, Plays).

develop_play_ia(Color, _, move, Plays) :-
    bug_can_move(Color, BugCanMove),
    develop_can_move_ia(Color, BugCanMove, Plays).

develop_play_ia(Color, _, power, Plays) :-
    bug_can_power(Color, BugCanPower),
    develop_can_power_ia(Color, BugCanPower, Plays).


develop_can_put_ia(_, [], []).

develop_can_put_ia(Color, [BugCanPutH|BugCanPutT], Plays) :-
    possible_colocation(Color, PossiblesColocations),
    develop_can_put_location_ia(BugCanPutH, PossiblesColocations, Plays1),
    develop_can_put_ia(Color, BugCanPutT, Plays2),
    append(Plays1, Plays2, Plays).

develop_can_put_location_ia(_, [], []).

develop_can_put_location_ia(Bug, [PCH|PCT], Plays) :-
    develop_can_put_location_ia(Bug, PCT, Plays1),
    append([[put, Bug, PCH]], Plays1, Plays).


develop_can_move_ia(_, [], []).

develop_can_move_ia(Color, [[B, R, C, Color, Sp]|BugCanMoveT], Plays) :-
    real_valid_moves(cell(B, R, C, Color, Sp, true), RealValidMoves),
    develop_can_move_location_ia([B, R, C, Color, Sp], RealValidMoves, Plays1),
    develop_can_move_ia(Color, BugCanMoveT, Plays2),
    append(Plays1, Plays2, Plays).

develop_can_move_location_ia(_, [], []).

develop_can_move_location_ia(Bug, [RVMH|RVMT], Plays) :-
    develop_can_move_location_ia(Bug, RVMT, Plays1),
    append([[move, Bug, RVMH]], Plays1, Plays).


develop_can_power_ia(_, [], []).

develop_can_power_ia(Color, [[B, R, C, Color, Sp]|BugCanPowerT], Plays) :-
    pillbug_power_can_apply(cell(B, R, C, Color, Sp, true), BugsPowerCanApply),
    develop_can_power_bug_ia([B, R, C, Color, Sp], BugsPowerCanApply, Plays1),
    develop_can_power_ia(Color, BugCanPowerT, Plays2),
    append(Plays1, Plays2, Plays).

develop_can_power_bug_ia(_, [], []).

develop_can_power_bug_ia([B, R, C, Color, Sp], [BugsCanPowerH|BugsCanPowerT], Plays) :-
    not_neighbors(cell(B, R, C, Color, Sp, true), PossiblesColocations),
    develop_can_power_location_ia([B, R, C, Color, Sp], BugsCanPowerH, PossiblesColocations, Plays1),
    develop_can_power_bug_ia([B, R, C, Color, Sp], BugsCanPowerT, Plays2),
    append(Plays1, Plays2, Plays).

develop_can_power_location_ia(_, _, [], []).

develop_can_power_location_ia(Bug, BugToPower, [PCH|PCT], Plays) :-
    develop_can_power_location_ia(Bug, BugToPower, PCT, Plays1),
    append([[power, Bug, BugToPower, PCH]], Plays1, Plays).