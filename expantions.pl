:- module(expantions, [select_expantions/0]).
:- use_module(game).
:- use_module(init_cells).

expantion(ladybug, 1).
expantion(mosquito, 1).
expantion(pillbug, 1).

select_expantions() :-
    findall([B, A], expantion(B, A), Expantions),
    select_expantions2(Expantions).


select_expantions2([]).

select_expantions2([[B, A]|Tail]) :-
    x_expantion(B, A),!,
    select_expantions2(Tail).

x_expantion(Bug, Amount) :-
    Selections = [yes, no],

    write("Use" + Bug + "expantion?\n" + Selections + "\n"),

    read(Selection),
    nth1(Selection, Selections, Selected),!,

    activate_expantion(Selected, Bug, Amount).


activate_expantion(no, _, _).

activate_expantion(yes, Bug, Amount) :-
    init_game_cells([Bug], [Amount]).