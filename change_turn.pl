:- module(change_turn, [change_turn/1, advance_turn/0]).
:- use_module(game).

change_turn(white) :-
    play_now(black),
    retract(play_now(black)),
    assertz(play_now(white)).

change_turn(black) :-
    play_now(white),
    retract(play_now(white)),
    assertz(play_now(black)).

advance_turn() :-
    turn(X),
    retract(turn(X)),
    X1 is X + 1,
    assertz(turn(X1)).