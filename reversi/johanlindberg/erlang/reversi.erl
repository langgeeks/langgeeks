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
    Lines      = contents_of_file(Input,[]),

    Rows       = lists:sublist(Lines,1,8),
    Cols       = make_cols(Rows,[]),

    Player     = lists:nth(9,Lines), 

    {board, {rows, Rows}, {cols, Cols}, {player, Player}}.

contents_of_file(Input, Lines) ->
    case io:get_line(Input,"") of
	eof -> Lines;
	Line -> contents_of_file(Input,[Line|Lines])
    end.

make_cols(N,Rows) ->
    [].

load_game_state_test() ->
    { board,
      {rows, Rows},
      {cols, Cols},
      {player, Player} } = reversi:load_game_state("test1.txt"),
    8 = length(Rows).
      
