-- Reversi kata for langgeeks

process :: String -> (Char, [String])
process filename = do contents <- readFile filename
                      return (getPlayer contents, getBoard contents)

getPlayer :: String -> Char
getPlayer contents = head (head (drop 8 (lines contents)))

getBoard :: String -> [String]
getBoard contents = take 8 (lines contents)

findMoves :: Char -> [String] -> [(Int,Int)]
findMoves player [] = []
findMoves player board = findMovesR player (rows board) 0 ++ findMovesC player (cols board) 0

findMovesR :: Char -> [String] -> Int -> [(Int,Int)]
findMovesR player [] n = []
findMovesR player (row:board) n = zip moves (repeat n) ++ findMovesR player board (n+1)
                                 where moves = findMovesInRow player row 0 ++
                                               reverseIndex (findMovesInRow player (reverse row) 0)

findMovesC :: Char -> [String] -> Int -> [(Int,Int)] 
findMovesC player [] n = []
findMovesC player (row:board) n = zip (repeat n) moves ++ findMovesC player board (n+1)
                                 where moves = findMovesInRow player row 0 ++
                                               reverseIndex (findMovesInRow player (reverse row) 0)

rows :: [String] -> [String]
rows board = board

cols :: [String] -> [String]
cols board = [[head (snd (splitAt n row)) | row <- board] | n <- [0..((length (head board))- 1)]]

reverseIndex :: [Int] -> [Int]
reverseIndex [] = []
reverseIndex moves = map (\x -> 7 - x) moves

findMovesInRow :: Char -> String -> Int -> [Int]
findMovesInRow player [] pos = []
findMovesInRow player (x:xs) pos | x == '.'
                                   && findChain player xs 0 = [pos] ++ findMovesInRow player xs (pos+1)
                                 | otherwise                = findMovesInRow player xs (pos+1)

findChain :: Char -> String -> Int -> Bool
findChain player [] pos     = False
findChain player (x:xs) pos | x == player = pos /= 0
                            | x /= '.'    = findChain player xs (pos+1)
                            | otherwise   = False

test_getPlayer :: Bool
test_getPlayer = getPlayer "\n\n\n\n\n\n\n\nW" == 'W'

test_getBoard :: Bool
test_getBoard = getBoard "ABC\nDEF\nGHI\nJKL\nMNO\nPQR\nSTU\nVWX\nP" == ["ABC",
                                                                         "DEF",
                                                                         "GHI",
                                                                         "JKL",
                                                                         "MNO",
                                                                         "PQR",
                                                                         "STU",
                                                                         "VWX"]

test_cols ::  Bool
test_cols = cols ["ABC","DEF","GHI"] == ["ADG","BEH","CFI"] &&
            cols ["ABCD","EFGH","IJKL","MNOP"] == ["AEIM","BFJN","CGKO","DHLP"]

test_findChain :: Bool
test_findChain = findChain 'B' "WWB.." 0  == True  &&
                 findChain 'B' "B.." 0    == False &&
                 findChain 'B' ".WWB.." 0 == False &&
                 findChain 'B' "..." 0    == False &&
                 findChain 'B' "W.." 0    == False

test_findMovesInRow :: Bool
test_findMovesInRow = findMovesInRow 'W' "...BW..." 0 == [2] &&
                      findMovesInRow 'W' ".BW..BW." 0 == [0,4] &&
                      findMovesInRow 'W' "...WB..." 0 == []

test_findMoves :: Bool
test_findMoves = findMoves 'W' ["........",
                                "........",
                                "........",
                                "...BW...",
                                "...WB...",
                                "........",
                                "........",
                                "........"] == [(2,3),(5,4),(3,2),(4,5)]
test_all :: Bool
test_all = test_cols &&
           test_findChain &&
           test_findMovesInRow &&
           test_findMoves &&
           test_getPlayer &&
           test_getBoard