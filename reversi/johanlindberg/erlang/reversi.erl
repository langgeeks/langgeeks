% reversi kata

-module(reversi).
-export([find_moves/1]).

find_moves(Board) ->
    re:run(Board,"\.W+B",[]).
