% reversi kata

-module(reversi).
-export([find_lmoves/1, find_rmoves/1]).

find_lmoves(Board) ->
    {_,[{Pos,_}]} = re:run(Board,"\.W+B",[]),
    Pos.

find_rmoves(Board) ->
    {_,[{_,Pos}]} = re:run(Board,"BW+\.",[]),
    Pos - 1.
    
