:- module(players, [select_players/0, player_turn/2]).
:- use_module(game).


select_players() :-
    select_player(1),
    select_player(2).


select_player(N) :-
    Selections = [ai, human],

    write("Select player" + N + "\n" + Selections + "\n"),

    read(Selection),
    nth1(Selection, Selections, Selected),

    retract(player(N, _)),
    assertz(player(N, Selected)).

player_turn(Turn, 2) :-
    0 is Turn mod 2.

player_turn(Turn, 1) :-
    1 is Turn mod 2.