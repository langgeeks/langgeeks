% reversi kata

-module(reversi).
-export([opponent/1, find_lmoves/2, find_rmoves/2, load_game_state/1]).
-include_lib("eunit/include/eunit.hrl").

opponent(Player) ->
    if
	Player == "B" -> "W";
	true          -> "B"
    end.

opponent_test() ->
    "B" = reversi:opponent("W"),
    "W" = reversi:opponent("B"),

    "B" = reversi:opponent("."),
    "B" = reversi:opponent("\n"),
    "B" = reversi:opponent("A").

find_lmoves(Board,Player) ->
    {_,[{Pos,_}]} = re:run(Board,
			   string:join(["\\.",opponent(Player),"+",Player],""),
			   []),
    Pos.

find_lmoves_test() ->
    0 = reversi:find_lmoves(".BW", "W"),
    0 = reversi:find_lmoves(".BBW", "W"),
    0 = reversi:find_lmoves(".BBBBBBW", "W"),
    1 = reversi:find_lmoves("..BBBW", "W").    

find_rmoves(Board,Player) ->
    {_,[{_,Pos}]} = re:run(Board,
			   string:join([Player,opponent(Player),"+\\."],""),
			   []),
    Pos - 1.
    
find_rmoves_test() ->
    2 = reversi:find_rmoves("WB.", "W"),
    3 = reversi:find_rmoves("WBB.", "W"),
    7 = reversi:find_rmoves("WBBBBBB.", "W"),
    4 = reversi:find_rmoves("WBBB..", "W").    

load_game_state(Filename) ->
    {ok,Input} = file:open(Filename,[read]),
    Rows = read_rows(1,Input,[]),
    Cols = make_cols(1,Rows),

    {board, {rows, Rows}, {cols, Cols}}.

read_rows(N,Input,Rows) ->
    [].

make_cols(N,Rows) ->
    [].

load_game_state_test() ->
    { board,
      {rows, Rows},
      {cols, Cols} } = reversi:load_game_state("test1.txt").
      
