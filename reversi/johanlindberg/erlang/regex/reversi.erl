% reversi kata

-module(reversi).
-export([find_moves/1]).

-include_lib("eunit/include/eunit.hrl").

find_moves(Filename) ->
    {board,
     {rows, Rows},
     {cols, Cols},
     {diagonals, Diagonals},
     {player, Player}} = load_game_state(Filename),
    extract_moves(Rows,Cols,Diagonals,Player).

extract_moves(Board,Player) ->
    true.

extract_moves(Rows,Cols,Diagonals,Player) ->
    lists:append(lists:append(extract_moves_rows(Rows,Player,[],0),
			      extract_moves_cols(Cols,Player,[],0)),
		 extract_moves_diagonals(Diagonals,Player,[],0)).

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

extract_moves_diagonals([Diagonal|Diagonals],Player,Acc,I) ->
    F = fun(R) -> {R,I} end,
    Acc1 = lists:append(Acc,
			lists:append(lists:map(F, find_lmoves(Diagonal,Player)),
				     lists:map(F, find_rmoves(Diagonal,Player)))),
    if
	Diagonals == [] -> Acc1;
	true            -> extract_moves_diagonals(Diagonals,Player,Acc1,I+1)
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

    [[A1,A2,A3,A4,A5,A6,A7,A8],
     [B1,B2,B3,B4,B5,B6,B7,B8],
     [C1,C2,C3,C4,C5,C6,C7,C8],
     [D1,D2,D3,D4,D5,D6,D7,D8],
     [E1,E2,E3,E4,E5,E6,E7,E8],
     [F1,F2,F3,F4,F5,F6,F7,F8],
     [G1,G2,G3,G4,G5,G6,G7,G8],
     [H1,H2,H3,H4,H5,H6,H7,H8],

     Player]                  = contents_of_file(Input,[]),

    Rows       = [[A1,A2,A3,A4,A5,A6,A7,A8],
		  [B1,B2,B3,B4,B5,B6,B7,B8],
		  [C1,C2,C3,C4,C5,C6,C7,C8],
		  [D1,D2,D3,D4,D5,D6,D7,D8],
		  [E1,E2,E3,E4,E5,E6,E7,E8],
		  [F1,F2,F3,F4,F5,F6,F7,F8],
		  [G1,G2,G3,G4,G5,G6,G7,G8],
		  [H1,H2,H3,H4,H5,H6,H7,H8]],

    Cols       = [[A1,B1,C1,D1,E1,F1,G1,H1],
		  [A2,B2,C2,D2,E2,F2,G2,H2],
		  [A3,B3,C3,D3,E3,F3,G3,H3],
		  [A4,B4,C4,D4,E4,F4,G4,H4],
		  [A5,B5,C5,D5,E5,F5,G5,H5],
		  [A6,B6,C6,D6,E6,F6,G6,H6],
		  [A7,B7,C7,D7,E7,F7,G7,H7],
		  [A8,B8,C8,D8,E8,F8,G8,H8]],

    Diagonals  = [[F1,G2,H3],
		  [E1,F2,G3,H4],
		  [D1,E2,F3,G4,H5],
		  [C1,D2,E3,F4,G5,H6],
		  [B1,C2,D3,E4,F5,G6,H7],
		  [A1,B2,C3,D4,E5,F6,G7,H8],
		  [A2,B3,C4,D5,E6,F7,G8],
		  [A3,B4,C5,D6,E7,F8],
		  [A4,B5,C6,D7,E8],
		  [A5,B6,C7,D8],
		  [A6,B7,C8],

		  [F8,G7,H6],
		  [E8,F7,G6,H5],
		  [D8,E7,F6,G5,H4],
		  [C8,D7,E6,F5,G4,H3],
		  [B8,C7,D6,E5,F4,G3,H2],
		  [A8,B7,C6,D5,E4,F3,G2,H1],
		  [A7,B6,C5,D4,E3,F2,G1],
		  [A6,B5,C4,D3,E2,F1],
		  [A5,B4,C3,D2,E1],
		  [A4,B3,C2,D1],
		  [A3,B2,C1]],

    {board, {rows, Rows}, {cols, Cols}, {diagonals, Diagonals}, {player, Player}}.

contents_of_file(Input, Lines) ->
    case io:get_line(Input,"") of
	eof  -> Lines;
	Line -> contents_of_file(Input,
				 lists:append(Lines,
					      [string:strip(Line,right,$\n)]))
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
    
    Rows      = ["A1111111",
		 "2A222222",
		 "33A33333",
		 "444A4444",
		 "5555A555",
		 "66666A66",
		 "777777A7",
		 "8888888A"],
    Cols      = ["A2345678",
		 "1A345678",
		 "12A45678",
		 "123A5678",
		 "1234A678",
		 "12345A78",
	         "123456A8",
	         "1234567A"],
    Diagonals = ["678",
		 "5678",
		 "45678",
		 "345678",
		 "2345678",
		 "AAAAAAAA",
		 "1234567",
		 "123456",
		 "12345",
		 "1234",
		 "123",

		 "6A8",
		 "5678",
		 "45A78",
		 "345678",
		 "234A678",
		 "12345678",
		 "123A567",
		 "123456",
		 "12A45",
		 "1234",
		 "1A3"]. 
    
find_moves_test() ->
    [{3,0},{3,1},{3,2},{3,3}] = extract_moves_rows([".BW.", ".BW.", ".BW.", ".BW."],"B",[],0),
    []                        = extract_moves_cols(["....", "BBBB", "WWWW", "...."],"B",[],0),
    [{3,0},{3,1},{3,2},{3,3}] = extract_moves([".BW.", ".BW.", ".BW.", ".BW."],
					      ["....", "BBBB", "WWWW", "...."],
					      ["....", "....", "....", "...."], "B"), % this test ignores diagonals

    [{5,3}, {2,4}, {3,5}, {4,2}] = find_moves("test1.txt"),
    [{2,1}] = extract_moves([{1,0},[[{1,1},"..BW"]]],"W").
%    [{5,6}, {3,4}, {3,6}]        = find_moves("test2.txt").
    
