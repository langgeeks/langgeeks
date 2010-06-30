% reversi kata

-module(reversi).
-export([find_moves/1]).

-include_lib("eunit/include/eunit.hrl").

find_moves(Filename) ->
    {state,
     {board, Board},
     {player, Player}} = load_game_state(Filename),
    extract_moves(Board,Player,[]).

extract_moves([[Direction,Lines]|Board],Player,Acc) ->
    Acc1 = lists:append(Acc, extract_moves_lines(Lines,Player,Direction,[])),
    if
	Board == [] -> Acc1;
	true        -> extract_moves(Board,Player,Acc1)
    end.

extract_moves_lines([[{C,R},Line]|Lines],Player,Direction,Acc) ->
    {DC,DR} = Direction,

    F = fun(P) -> {(P*DC)+C,(P*DR)+R} end,
    Acc1 = lists:append(Acc,
			lists:append(lists:map(F, find_lmoves(Line,Player)),
				     lists:map(F, find_rmoves(Line,Player)))),
    if
	Lines == [] -> Acc1;
	true        -> extract_moves_lines(Lines,Player,Direction,Acc1)
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

    Rows       = [{1,0},[[{1,1},[A1,A2,A3,A4,A5,A6,A7,A8]],
			 [{1,2},[B1,B2,B3,B4,B5,B6,B7,B8]],
			 [{1,3},[C1,C2,C3,C4,C5,C6,C7,C8]],
			 [{1,4},[D1,D2,D3,D4,D5,D6,D7,D8]],
			 [{1,5},[E1,E2,E3,E4,E5,E6,E7,E8]],
			 [{1,6},[F1,F2,F3,F4,F5,F6,F7,F8]],
			 [{1,7},[G1,G2,G3,G4,G5,G6,G7,G8]],
			 [{1,8},[H1,H2,H3,H4,H5,H6,H7,H8]]]],

    Cols       = [{0,1},[[{1,1},[A1,B1,C1,D1,E1,F1,G1,H1]],
			 [{2,1},[A2,B2,C2,D2,E2,F2,G2,H2]],
			 [{3,1},[A3,B3,C3,D3,E3,F3,G3,H3]],
			 [{4,1},[A4,B4,C4,D4,E4,F4,G4,H4]],
			 [{5,1},[A5,B5,C5,D5,E5,F5,G5,H5]],
			 [{6,1},[A6,B6,C6,D6,E6,F6,G6,H6]],
			 [{7,1},[A7,B7,C7,D7,E7,F7,G7,H7]],
			 [{8,1},[A8,B8,C8,D8,E8,F8,G8,H8]]]],

    RDiagonals = [{1,1},[[{1,6},[F1,G2,H3]],
			 [{1,5},[E1,F2,G3,H4]],
			 [{1,4},[D1,E2,F3,G4,H5]],
			 [{1,3},[C1,D2,E3,F4,G5,H6]],
			 [{1,2},[B1,C2,D3,E4,F5,G6,H7]],
			 [{1,1},[A1,B2,C3,D4,E5,F6,G7,H8]],
			 [{2,1},[A2,B3,C4,D5,E6,F7,G8]],
			 [{3,1},[A3,B4,C5,D6,E7,F8]],
			 [{4,1},[A4,B5,C6,D7,E8]],
			 [{5,1},[A5,B6,C7,D8]],
			 [{6,1},[A6,B7,C8]]]],
			 
    LDiagonals = [{-1,1},[[{8,6},[F8,G7,H6]],
			  [{8,5},[E8,F7,G6,H5]],
			  [{8,4},[D8,E7,F6,G5,H4]],
			  [{8,3},[C8,D7,E6,F5,G4,H3]],
			  [{8,2},[B8,C7,D6,E5,F4,G3,H2]],
			  [{8,1},[A8,B7,C6,D5,E4,F3,G2,H1]],
			  [{7,1},[A7,B6,C5,D4,E3,F2,G1]],
			  [{6,1},[A6,B5,C4,D3,E2,F1]],
			  [{5,1},[A5,B4,C3,D2,E1]],
			  [{4,1},[A4,B3,C2,D1]],
			  [{3,1},[A3,B2,C1]]]],

    {state, {board, [Rows, Cols, RDiagonals, LDiagonals]}, {player, Player}}.

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
    { state,
      {board, Board },
      {player, Player} } = load_game_state("test.txt"),
    "B" = Player,

    [Rows, Cols, RDiagonals, LDiagonals] = Board,
%    8 = length(Rows),
%    8 = length(Cols),
    
    Rows      = [{1,0},[[{1,1},"A1111111"],
			[{1,2},"2A222222"],
			[{1,3},"33A33333"],
			[{1,4},"444A4444"],
			[{1,5},"5555A555"],
			[{1,6},"66666A66"],
			[{1,7},"777777A7"],
			[{1,8},"8888888A"]]],
    Cols      = [{0,1},[[{1,1},"A2345678"],
			[{2,1},"1A345678"],
			[{3,1},"12A45678"],
			[{4,1},"123A5678"],
			[{5,1},"1234A678"],
			[{6,1},"12345A78"],
			[{7,1},"123456A8"],
			[{8,1},"1234567A"]]],
    RDiagonals= [{1,1},[[{1,6},"678"],
			[{1,5},"5678"],
			[{1,4},"45678"],
			[{1,3},"345678"],
			[{1,2},"2345678"],
			[{1,1},"AAAAAAAA"],
			[{2,1},"1234567"],
			[{3,1},"123456"],
			[{4,1},"12345"],
			[{5,1},"1234"],
			[{6,1},"123"]]],
    LDiagonals=[{-1,1},[[{8,6},"6A8"],
			[{8,5},"5678"],
			[{8,4},"45A78"],
			[{8,3},"345678"],
			[{8,2},"234A678"],
			[{8,1},"12345678"],
			[{7,1},"123A567"],
			[{6,1},"123456"],
			[{5,1},"12A45"],
			[{4,1},"1234"],
			[{3,1},"1A3"]]].
    
find_moves_test() ->
%    [{3,0},{3,1},{3,2},{3,3}] = extract_moves_rows([".BW.", ".BW.", ".BW.", ".BW."],"B",[],0),
%    []                        = extract_moves_cols(["....", "BBBB", "WWWW", "...."],"B",[],0),
%    [{3,0},{3,1},{3,2},{3,3}] = extract_moves([".BW.", ".BW.", ".BW.", ".BW."],
%					      ["....", "BBBB", "WWWW", "...."],
%					      ["....", "....", "....", "...."], "B"), % this test ignores diagonals

    [{2,1},{2,2},{1,1},{1,4},{3,2},{2,2}] =
	extract_moves([[{1,0},[[{1,1},"..BW"],[{1,2},"..BW"]]], % rows
		       [{0,1},[[{1,1},".BW.BW"]]], % cols
		       [{-1,1},[[{4,1},"..BW"]]], % diagonals (right to left)
		       [{1,1},[[{1,1},"..BW"]]]],"W",[]), % diagonals (left to right)
    [{6,4}, {3,5}, {4,6}, {5,3}] = find_moves("test1.txt"),
    [{6,5}, {4,3}, {6,3}]        = find_moves("test2.txt").
    
