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
    
    [0,3] = reversi:find_lmoves(".BW.BW", "W").
    
find_rmoves_test() ->
    [2] = reversi:find_rmoves("WB.", "W"),
    [3] = reversi:find_rmoves("WBB.", "W"),
    [7] = reversi:find_rmoves("WBBBBBB.", "W"),
    [4] = reversi:find_rmoves("WBBB..", "W").    

load_game_state_test() ->
    { board,
      {rows, Rows},
      {cols, Cols},
      {player, Player} } = reversi:load_game_state("test1.txt"),
    "B" = Player,
    8 = length(Rows),
    8 = length(Cols).
      
