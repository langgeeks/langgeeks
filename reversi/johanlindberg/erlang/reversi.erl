% reversi kata

-module(reversi).
-export([opponent/1, find_lmoves/2, find_rmoves/2]).

opponent(Player) ->
    if
	Player == "B" -> "W";
	true          -> "B"
    end.

find_lmoves(Board,Player) ->
    {_,[{Pos,_}]} = re:run(Board,
			   string:join(["\\.",opponent(Player),Player],""),
			   []),
    Pos.

find_rmoves(Board,Player) ->
    {_,[{_,Pos}]} = re:run(Board,
			   string:join([Player,opponent(Player),"\\."],""),
			   []),
    Pos - 1.
    
