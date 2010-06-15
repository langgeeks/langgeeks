% reversi kata

-module(reversi).
-export([find_moves/1,
	 
	 extract_moves/3,
	 extract_moves_rows/3,
	 extract_moves_cols/3,
	 opponent/1,
	 find_lmoves/2,
	 find_rmoves/2,
	 load_game_state/1,
	 make_cols/1]).

-include_lib("eunit/include/eunit.hrl").

find_moves(Filename) ->
    {board,
     {rows, Rows},
     {cols, Cols},
     {player, Player}} = reversi:load_game_state(Filename),
    extract_moves(Rows,Cols,Player).

extract_moves(Rows,Cols,Player) ->
    lists:append(extract_moves_rows(Rows,Player,[]),
		 extract_moves_cols(Cols,Player,[])).

extract_moves_rows([Row|Rows],Player,Acc) ->
    F = fun(C) -> {C,length(Acc)} end,
    Acc1 = lists:append(Acc,
			lists:append(lists:map(F, find_lmoves(Row,Player)),
				     lists:map(F, find_rmoves(Row,Player)))),
    if
	Rows == [] -> Acc1;
	true       -> extract_moves_rows(Rows,Player,Acc1)
    end.    

extract_moves_cols([Col|Cols],Player,Acc) ->
    F = fun(R) -> {length(Acc),R} end,
    Acc1 = lists:append(Acc,
			lists:append(lists:map(F, find_lmoves(Col,Player)),
				     lists:map(F, find_rmoves(Col,Player)))),
    if
	Cols == [] -> Acc1;
	true       -> extract_moves_cols(Cols,Player,Acc1)
    end.    

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
    case re:run(Board,
		string:join(["\\.",opponent(Player),"+",Player],""),
		[global]) of
	nomatch        -> [];
	{_, Positions} -> extract_lpositions(Positions, [])
    end.

extract_rpositions([[{P,L}]|Pos], Acc) ->
    Acc1 = lists:append(Acc, [P + L - 1]),
    if Pos == [] -> Acc1;
       true      -> extract_rpositions(Pos, Acc1)
    end.

find_rmoves(Board,Player) ->
    case re:run(Board,
		string:join([Player,opponent(Player),"+\\."],""),
		[global]) of
	nomatch       -> [];
	{_,Positions} -> extract_rpositions(Positions, [])
    end.
    
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

