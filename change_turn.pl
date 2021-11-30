:- module(change_turn, [change_turn/1, advance_turn/0]).
:- use_module(game).

change_turn(white) :-
    color_now(black),
    retract(color_now(black)),
    assertz(color_now(white)).

change_turn(black) :-
    color_now(white),
    retract(color_now(white)),
    assertz(color_now(black)).

advance_turn() :-
    turn(X),
    retract(turn(X)),
    X1 is X + 1,
    assertz(turn(X1)).