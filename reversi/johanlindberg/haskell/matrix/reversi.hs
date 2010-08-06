-- Reversi kata for langgeeks

import Test.HUnit

findAllMoves :: String -> IO ()
findAllMoves filename = do contents <- readFile filename
                           putStrLn (show (findMoves (getPlayer contents) (getBoard contents)))

getPlayer :: String -> Char
getPlayer contents = firstCharOf playerRow
                     where firstCharOf s = head (head s)
                           playerRow = drop 8 (lines contents)

getBoard :: String -> [String]
getBoard contents = take 8 (lines contents)

findMoves :: Char -> [String] -> [(Int,Int)]
findMoves player []    = []
findMoves player board = findMovesR player (rows board) 0 ++
                         findMovesC player (cols board) 0

findMovesR :: Char -> [String] -> Int -> [(Int,Int)]
findMovesR player [] n          = []
findMovesR player (row:board) n = zip moves (repeat n) ++ findMovesR player board (n+1)
                                 where moves = findMovesInRow player row 0 ++
                                               reverseIndex (findMovesInRow player (reverse row) 0)

findMovesC :: Char -> [String] -> Int -> [(Int,Int)] 
findMovesC player [] n          = []
findMovesC player (row:board) n = zip (repeat n) moves ++ findMovesC player board (n+1)
                                 where moves = findMovesInRow player row 0 ++
                                               reverseIndex (findMovesInRow player (reverse row) 0)

rows :: [String] -> [String]
rows board = board

cols :: [String] -> [String]
cols board = [[nth n row | row <- board] | n <- [0..sizeOf board]]
             where nth n row = head (snd (splitAt n row))
                   sizeOf board = ((length (head board))- 1)

reverseIndex :: [Int] -> [Int]
reverseIndex []    = []
reverseIndex moves = map (\x -> 7 - x) moves

findMovesInRow :: Char -> String -> Int -> [Int]
findMovesInRow player [] pos     = []
findMovesInRow player (x:xs) pos | x == '.'
                                   && findChain player xs 0 = [pos] ++ findMovesInRow player xs (pos+1)
                                 | otherwise                = findMovesInRow player xs (pos+1)

findChain :: Char -> String -> Int -> Bool
findChain player [] pos     = False
findChain player (x:xs) pos | x == player = pos /= 0
                            | x /= '.'    = findChain player xs (pos+1)
                            | otherwise   = False

-- Unit tests

tests = TestList [TestCase (assertEqual "getPlayer"
                            (getPlayer "\n\n\n\n\n\n\n\nW") 'W'),
                  TestCase (assertEqual "getBoard"
                            (getBoard "ABC\nDEF\nGHI\nJKL\nMNO\nPQR\nSTU\nVWX\nP")
                            ["ABC","DEF","GHI","JKL","MNO","PQR","STU","VWX"]),

                  TestCase (assertEqual "test_cols1"
                            (cols ["ABC","DEF","GHI"])
                            ["ADG","BEH","CFI"]),
                  TestCase (assertEqual "test_cols2"
                            (cols ["ABCD","EFGH","IJKL","MNOP"])
                            ["AEIM","BFJN","CGKO","DHLP"]),

                  TestCase (assertEqual "test_findChain1" (findChain 'B' "WWB.." 0) True),
                  TestCase (assertEqual "test_findChain2" (findChain 'B' "B.." 0) False),
                  TestCase (assertEqual "test_findChain3" (findChain 'B' ".WWB.." 0) False),
                  TestCase (assertEqual "test_findChain4" (findChain 'B' "..." 0) False),
                  TestCase (assertEqual "test_findChain5" (findChain 'B' "W.." 0) False),

                  TestCase (assertEqual "test_findMovesInRow1" (findMovesInRow 'W' "...BW..." 0) [2]),
                  TestCase (assertEqual "test_findMovesInRow2" (findMovesInRow 'W' ".BW..BW." 0) [0,4]),
                  TestCase (assertEqual "test_findMovesInRow3" (findMovesInRow 'W' "...WB..." 0) []),

                  TestCase (assertEqual "findMoves"
                            (findMoves 'W' ["........",
                                            "........",
                                            "........",
                                            "...BW...",
                                            "...WB...",
                                            "........",
                                            "........",
                                            "........"])
                            [(2,3),(5,4),(3,2),(4,5)]) ]
