% reversi kata

-module(reversi).
-export([find_moves/1]).

-include_lib("eunit/include/eunit.hrl").

find_moves(Filename) ->
    {board,
     {rows, Rows},
     {cols, Cols},
     {player, Player}} = load_game_state(Filename),
    extract_moves(Rows,Cols,Player).

extract_moves(Rows,Cols,Player) ->
    lists:append(extract_moves_rows(Rows,Player,[],0),
		 extract_moves_cols(Cols,Player,[],0)).

extract_moves_rows([Row|Rows],Player,Acc,I) ->
    F = fun(C) -> {C,I} end,
    Acc1 = lists:append(Acc,
			lists:append(lists:map(F, find_lmoves(Row,Player)),
				     lists:map(F, find_rmoves(Row,Player)))),
    if
	Rows == [] -> Acc1;
	true       -> extract_moves_rows(Rows,Player,Acc1,I+1)
    end.    

extract_moves_cols([Col|Cols],Player,Acc,I) ->
    F = fun(R) -> {I,R} end,
    Acc1 = lists:append(Acc,
			lists:append(lists:map(F, find_lmoves(Col,Player)),
				     lists:map(F, find_rmoves(Col,Player)))),
    if
	Cols == [] -> Acc1;
	true       -> extract_moves_cols(Cols,Player,Acc1,I+1)
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

% tests

opponent_test() ->
    "B" = opponent("W"),
    "W" = opponent("B"),

    "B" = opponent("."),
    "B" = opponent("\n"),
    "B" = opponent("A").

find_lmoves_test() ->
    [0] = find_lmoves(".BW", "W"),
    [0] = find_lmoves(".BBW", "W"),
    [0] = find_lmoves(".BBBBBBW", "W"),
    [1] = find_lmoves("..BBBW", "W"),
    
    [0,3] = find_lmoves(".BW.BW", "W"),    
    [0,3,6,9] = find_lmoves(".BW.BW.BW.BBBBBW","W").    

find_rmoves_test() ->
    [2] = find_rmoves("WB.", "W"),
    [3] = find_rmoves("WBB.", "W"),
    [7] = find_rmoves("WBBBBBB.", "W"),
    [4] = find_rmoves("WBBB..", "W"),

    [2,5,8,16] = find_rmoves("BW.BW.BW.BWWWWWW.","B").

load_game_state_test() ->
    { board,
      {rows, Rows},
      {cols, Cols},
      {diagonals, Diagonals },
      {player, Player} } = load_game_state("test.txt"),
    "B" = Player,
    8 = length(Rows),
    8 = length(Cols),
    
    Rows = ["A1111111","2A222222","33A33333","444A4444","5555A555","66666A66","777777A7","8888888A"],
    Cols = ["A2345678","1A345678","12A45678","123A5678","1234A678","12345A78","123456A8","1234567A"].
    
find_moves_test() ->
    [{3,0},{3,1},{3,2},{3,3}] = extract_moves_rows([".BW.", ".BW.", ".BW.", ".BW."],"B",[],0),
    []                        = extract_moves_cols(["....", "BBBB", "WWWW", "...."],"B",[],0),
    [{3,0},{3,1},{3,2},{3,3}] = extract_moves([".BW.", ".BW.", ".BW.", ".BW."],
					      ["....", "BBBB", "WWWW", "...."], "B"),

    [{5,3}, {2,4}, {3,5}, {4,2}] = find_moves("test1.txt").
