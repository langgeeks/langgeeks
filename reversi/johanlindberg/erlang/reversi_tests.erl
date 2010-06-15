% reversi kata

-module(reversi_tests).
-include_lib("eunit/include/eunit.hrl").

opponent_test() ->
    "B" = reversi:opponent("W"),
    "W" = reversi:opponent("B"),

    "B" = reversi:opponent("."),
    "B" = reversi:opponent("\n"),
    "B" = reversi:opponent("A").

find_lmoves_test() ->
    [0] = reversi:find_lmoves(".BW", "W"),
    [0] = reversi:find_lmoves(".BBW", "W"),
    [0] = reversi:find_lmoves(".BBBBBBW", "W"),
    [1] = reversi:find_lmoves("..BBBW", "W"),
    
    [0,3] = reversi:find_lmoves(".BW.BW", "W"),    
    [0,3,6,9] = reversi:find_lmoves(".BW.BW.BW.BBBBBW","W").    

find_rmoves_test() ->
    [2] = reversi:find_rmoves("WB.", "W"),
    [3] = reversi:find_rmoves("WBB.", "W"),
    [7] = reversi:find_rmoves("WBBBBBB.", "W"),
    [4] = reversi:find_rmoves("WBBB..", "W"),

    [2,5,8,16] = reversi:find_rmoves("BW.BW.BW.BWWWWWW.","B").

load_game_state_test() ->
    { board,
      {rows, Rows},
      {cols, Cols},
      {player, Player} } = reversi:load_game_state("test.txt"),
    "B" = Player,
    8 = length(Rows),
    8 = length(Cols),
    
    Rows = ["A1111111","2A222222","33A33333","444A4444","5555A555","66666A66","777777A7","8888888A"],
    Cols = ["A2345678","1A345678","12A45678","123A5678","1234A678","12345A78","123456A8","1234567A"].
      
find_moves_test() ->
    [{3,0},{3,1},{3,2},{3,3}] = reversi:extract_moves_rows([".BW.", ".BW.", ".BW.", ".BW."],"B",[]),
    []                        = reversi:extract_moves_cols(["....", "BBBB", "WWWW", "...."],"B",[]),
    [{3,0},{3,1},{3,2},{3,3}] = reversi:extract_moves([".BW.", ".BW.", ".BW.", ".BW."],
						      ["....", "BBBB", "WWWW", "...."], "B"),

    [{2,4}, {3,5}, {4,2}, {5,3}] = reversi:find_moves("test1.txt").
