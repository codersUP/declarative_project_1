:- module(ai, [best_play_ai/5, make_play_ai/2]).
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
:- use_module(valid_moves).


make_play_ai(Color, [put, BugToPut, [LocationR, LocationC]]) :-
    select_cell_to_put(Color, BugToPut, [BugToPut, R, C, Color, Sp]),
    make_move(cell(BugToPut, R, C, Color, Sp, false), [LocationR, LocationC], SP_greater),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(BugToPut, LocationR, LocationC, Color, SP_greater, true))).    

make_play_ai(Color, [move, [Bug, R, C, Color, Sp], [LocationR, LocationC]]) :-
    make_move(cell(Bug, R, C, Color, Sp, true), [LocationR, LocationC], SP_greater),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(Bug, LocationR, LocationC, Color, SP_greater, true))).


make_play_ai(_, [power, [_, _, _, _, _], [BugToApplyPowerR, BugToApplyPowerC], [LocationR, LocationC]]) :-
    cell(BugP, BugToApplyPowerR, BugToApplyPowerC, BugColor, BugSp, true),

    make_move(cell(BugP, BugToApplyPowerR, BugToApplyPowerC, BugColor, BugSp, true), [LocationR, LocationC], SP_greater),

    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(BugP, LocationR, LocationC, BugColor, SP_greater, true))).

make_play_ai(_, []).


retract_play_ai(Color, [put, _, _]) :-
    last_move(cell(Bug, LocationR, LocationC, Color, Sp, true)),

    findall(SpInHand, cell(Bug, _, _, Color, SpInHand, false), Cells),
    sort(Cells, CellsSorted),
    get_first_or_0(CellsSorted, SPLower),

    SPL1 is SPLower - 1,

    retract(cell(Bug, LocationR, LocationC, Color, Sp, true)),
    assertz(cell(Bug, 0, 0, Color, SPL1, false)).

retract_play_ai(Color, [move, [Bug, R, C, Color, _], _]) :-
    last_move(cell(Bug, LocationR, LocationC, Color, Sp, true)),

    make_move(cell(Bug, LocationR, LocationC, Color, Sp, true), [R, C], _).

retract_play_ai(_, [power, [_, _, _, _, _], [BugToApplyPowerR, BugToApplyPowerC], _]) :-
    last_move(cell(Bug, LocationR, LocationC, Color, Sp, true)),

    make_move(cell(Bug, LocationR, LocationC, Color, Sp, true), [BugToApplyPowerR, BugToApplyPowerC], _).



heuristic(Color, X) :-
    game_result(Result),
    heuristic2(Result, Color, X).

heuristic(Color, Points) :-
    opposite_color(Color, RivalColor),
    how_many_no_moves(RivalColor, Point1),
    
    neighbors_queen(Color, NeighborsMyQueen),
    neighbors_queen(RivalColor, NeighborsRivalQueen),
    Point2 is NeighborsMyQueen - NeighborsRivalQueen,
    
    amount_movement(Color, MyMovements),
    amount_movement(RivalColor, RivalMovements),
    Point3 is MyMovements - RivalMovements,

    findall([R, C, Sp], cell(_, R, C, Color, Sp, false), InHand),
    findall([B, R, C, Sp], cell(B, R, C, Color, Sp, true), InGame),
    length(InGame, AmountInGame),
    length(InHand, AmountInHand),
    Point4 is AmountInGame + -2 * AmountInHand,

    Points is Point1 + Point2 + Point3 + Point4.

neighbors_queen(Color, 0) :-
    not(cell(queen, _, _, Color, _, true)).

neighbors_queen(Color, X) :-
    cell(queen, R, C, Color, Sp, true),
    neighbors(cell(queen, R, C, Color, Sp, true), N),
    length(N, X).

amount_movement(Color, 0) :-
    not(cell(_, _, _, Color, _, true)).

amount_movement(Color, X) :-
    bug_can_move(Color, Y),
    length(Y, X).
    

heuristic2(X, X, 100000).
heuristic2(Result, Color, -100000) :-
    not(Result = continue),
    not(Result = Color).


best_play_ai(Color, Turn, Depth, Points, Play) :-
    generates_plays_ai(Color, Turn, Plays),!,
    % length(Plays, X),
    % write(Color + Turn + Depth + X + "\n"),
    select_best_play(Color, Turn, Depth, Plays, Points, Play).


select_best_play(_, _, _, [], -100000, []).

select_best_play(Color, Turn, 0, [PlayH| PlayT], Point, Play) :-
    last_move(cell(LB, LR, LC, LColor, LSp, LInGame)),
    make_play_ai(Color, PlayH),
    
    heuristic(Color, Point1),
    
    retract_play_ai(Color, PlayH),
    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(LB, LR, LC, LColor, LSp, LInGame))),!,

    select_best_play(Color, Turn, 0, PlayT, Point2, Play2),!,
    select_play_more_points(Point1, PlayH, Point2, Play2, Point, Play).

select_best_play(Color, Turn, Depth, [PlayH| PlayT], Point, Play) :-
    Depth > 0,
    last_move(cell(LB, LR, LC, LColor, LSp, LInGame)),
    make_play_ai(Color, PlayH),

    heuristic(Color, Point1),

    retract_play_ai(Color, PlayH),
    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(LB, LR, LC, LColor, LSp, LInGame))),!,

    select_best_play(Color, Turn, Depth, PlayT, Point2, Play2),!,
    select_play_more_points(Color, Turn, Depth, Point1, PlayH, Point2, Play2, PointR, PlayR),
    select_play_more_points(PointR, PlayR, Point2, Play2, Point, Play).


select_play_more_points(Point1, _,Point2, Play2, Point2, Play2) :-
    not(Point1 > Point2).

select_play_more_points(Point1, Play1, Point2, _, Point1, Play1) :-
    Point1 > Point2.

select_play_more_points(_, _, _, Point1, _, Point2, Play2, Point2, Play2) :-
    Point2R is Point2 + 100,
    not(Point1 > Point2R).

select_play_more_points(Color, Turn, Depth, Point1, Play1, Point2, _, X, Play1) :-
    Point2R is Point2 + 100,
    Point1 > Point2R,

    last_move(cell(LB, LR, LC, LColor, LSp, LInGame)),
    make_play_ai(Color, Play1),
    
    D1 is Depth - 1,
    T1 is Turn + 1,
    opposite_color(Color, RivalColor),!,
    best_play_ai(RivalColor, T1, D1, RivalPoints, _),

    retract_play_ai(Color, Play1),
    retract(last_move(cell(_, _, _, _, _, _))),
    assertz(last_move(cell(LB, LR, LC, LColor, LSp, LInGame))),

    X is Point1 - RivalPoints.


how_many_no_moves(Color, NoMoves) :-
    findall([B, R, C, Color, Sp], cell(B, R, C, Color, Sp, true), Cells),
    bug_can_move(Color, BugCanMove),
    length(Cells, CellsL),
    length(BugCanMove, BugCanMoveL),
    NoMoves is CellsL - BugCanMoveL.


generates_plays_ai(Color, Turn, Plays) :-
    play_can_do(Color, PlayCanDo),
    develop_plays_ai(Color, Turn, PlayCanDo, Plays).


develop_plays_ai(_, _, [], []).

develop_plays_ai(Color, Turn, [PlaysH|PlaysT], Plays) :-
    develop_play_ai(Color, Turn, PlaysH, Plays1),
    develop_plays_ai(Color,Turn, PlaysT, Plays2),
    append(Plays1, Plays2, Plays).

develop_play_ai(Color, Turn, put, Plays) :-
    bug_can_put(Color, Turn, BugCanPut),
    develop_can_put_ai(Color, BugCanPut, Plays).

develop_play_ai(Color, _, move, Plays) :-
    bug_can_move(Color, BugCanMove),
    develop_can_move_ai(Color, BugCanMove, Plays).

develop_play_ai(Color, _, power, Plays) :-
    bug_can_power(Color, BugCanPower),
    develop_can_power_ai(Color, BugCanPower, Plays).


develop_can_put_ai(_, [], []).

develop_can_put_ai(Color, [BugCanPutH|BugCanPutT], Plays) :-
    possible_colocation(Color, PossiblesColocations),
    develop_can_put_location_ai(BugCanPutH, PossiblesColocations, Plays1),
    develop_can_put_ai(Color, BugCanPutT, Plays2),
    append(Plays1, Plays2, Plays).

develop_can_put_location_ai(_, [], []).

develop_can_put_location_ai(Bug, [PCH|PCT], Plays) :-
    develop_can_put_location_ai(Bug, PCT, Plays1),
    append([[put, Bug, PCH]], Plays1, Plays).


develop_can_move_ai(_, [], []).

develop_can_move_ai(Color, [[B, R, C, Color, Sp]|BugCanMoveT], Plays) :-
    real_valid_moves(cell(B, R, C, Color, Sp, true), RealValidMoves),
    develop_can_move_location_ai([B, R, C, Color, Sp], RealValidMoves, Plays1),
    develop_can_move_ai(Color, BugCanMoveT, Plays2),
    append(Plays1, Plays2, Plays).

develop_can_move_location_ai(_, [], []).

develop_can_move_location_ai(Bug, [RVMH|RVMT], Plays) :-
    develop_can_move_location_ai(Bug, RVMT, Plays1),
    append([[move, Bug, RVMH]], Plays1, Plays).


develop_can_power_ai(_, [], []).

develop_can_power_ai(Color, [[B, R, C, Color, Sp]|BugCanPowerT], Plays) :-
    pillbug_power_can_apply(cell(B, R, C, Color, Sp, true), BugsPowerCanApply),
    develop_can_power_bug_ai([B, R, C, Color, Sp], BugsPowerCanApply, Plays1),
    develop_can_power_ai(Color, BugCanPowerT, Plays2),
    append(Plays1, Plays2, Plays).

develop_can_power_bug_ai(_, [], []).

develop_can_power_bug_ai([B, R, C, Color, Sp], [BugsCanPowerH|BugsCanPowerT], Plays) :-
    not_neighbors(cell(B, R, C, Color, Sp, true), PossiblesColocations),
    develop_can_power_location_ai([B, R, C, Color, Sp], BugsCanPowerH, PossiblesColocations, Plays1),
    develop_can_power_bug_ai([B, R, C, Color, Sp], BugsCanPowerT, Plays2),
    append(Plays1, Plays2, Plays).

develop_can_power_location_ai(_, _, [], []).

develop_can_power_location_ai(Bug, BugToPower, [PCH|PCT], Plays) :-
    develop_can_power_location_ai(Bug, BugToPower, PCT, Plays1),
    append([[power, Bug, BugToPower, PCH]], Plays1, Plays).