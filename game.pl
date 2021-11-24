:- module(game, [cell/6]).

cell(_, 0, 0, _, 0, true).
cell(_, 0, 1, _, 0, true).
cell(ant, 0, 2, _, 0, true).
cell(ant, 0, 3, _, 0, true).
cell(ant, -1, 2, _, 0, true).
cell(ant, -1, 3, _, 0, true).

:- dynamic cell/6.