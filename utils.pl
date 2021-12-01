% cell(BugType, Row, Column, Color, StackPosition, InGame)

:- module(utils, [get_first_or_0/2]).

get_first_or_0([], 0).
get_first_or_0([LH|_], LH).