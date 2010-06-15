% reversi kata

-module(reversi).
-export([opponent/1, find_lmoves/2, find_rmoves/2, load_game_state/1, make_cols/1]).
-include_lib("eunit/include/eunit.hrl").

opponent(Player) ->
    if
	Player == "B" -> "W";
	true          -> "B"
    end.

extract_lpositions([[{P,_}]|Pos], Acc) ->
    Acc1 = lists:append(Acc, [P]),
    if Pos == [] -> Acc1;
       true      -> extract_lpositions(Pos, Acc1)
    end.

find_lmoves(Board, Player) ->
    {_,Positions} = re:run(Board,
			   string:join(["\\.",opponent(Player),"+",Player],""),
			   [global]),
    extract_lpositions(Positions, []).

extract_rpositions([[{P,L}]|Pos], Acc) ->
    Acc1 = lists:append(Acc, [P + L - 1]),
    if Pos == [] -> Acc1;
       true      -> extract_rpositions(Pos, Acc1)
    end.

find_rmoves(Board,Player) ->
    {_,Positions} = re:run(Board,
			   string:join([Player,opponent(Player),"+\\."],""),
			   [global]),
    extract_rpositions(Positions, []).
    
load_game_state(Filename) ->
    {ok,Input} = file:open(Filename,[read]),
    Lines      = contents_of_file(Input,[]),

    Rows       = lists:sublist(Lines,1,8),
    Cols       = make_cols(Rows),

    Player     = lists:nth(9,Lines), 

    {board, {rows, Rows}, {cols, Cols}, {player, Player}}.

contents_of_file(Input, Lines) ->
    case io:get_line(Input,"") of
	eof  -> Lines;
	Line -> contents_of_file(Input,
				 lists:append(Lines,
					      [string:strip(Line,right,$\n)]))
    end.

make_cols(Rows) ->
    make_cols(Rows, ["","","","","","","",""]).

make_cols([Row|Rows],Cols) ->
    F = fun(R,C) -> lists:append(C,[R])  end,
    C = lists:zipwith(F, Row, Cols),
    if Rows == [] -> C;
       true       -> make_cols(Rows,C)
    end.

