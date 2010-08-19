-- Reversi kata for langgeeks

import Data.List (transpose)
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
findMoves player board = findMovesR player board 0 ++             -- board as rows
                         findMovesC player (transpose board) 0 ++ --  -"-  as columns
                         findMovesDL player (dl board) 0 ++       --  -"-  as slices diagonally from left
                         findMovesDR player (dr board) 0          --  -"-     -"-    diagonally from right 

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

findMovesDL :: Char -> [String] -> Int -> [(Int,Int)]
findMovesDL player [] n          = []
findMovesDL player (row:board) n = []

findMovesDR :: Char -> [String] -> Int -> [(Int,Int)]
findMovesDR player [] n          = []
findMovesDR player (row:board) n = []

makeSlice :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
makeSlice (x,y) (dx,dy) | x >= 0
                          && x < 8
                          && y >= 0
                          && y < 8  = [(x,y)] ++ makeSlice (x+dx,y+dy) (dx,dy)
                        | otherwise = []

getPosition :: [String] -> (Int,Int) -> Char
getPosition board (x,y) = (board !! y) !! x 

dlstart :: [(Int,Int)]
dlstart = [(0,5),(0,4),(0,3),(0,2),(0,1),(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)]

dl :: [String] -> [String]
dl board = [map (getPosition board) (makeSlice start (1,1)) | start <- dlstart]

drstart :: [(Int,Int)]
drstart = [(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(7,1),(7,2),(7,3),(7,4),(7,5)]

dr :: [String] -> [String]
dr board = [map (getPosition board) (makeSlice start (-1,1)) | start <- drstart]

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

                  TestCase (assertEqual "test_findChain1" (findChain 'B' "WWB.." 0) True),
                  TestCase (assertEqual "test_findChain2" (findChain 'B' "B.." 0) False),
                  TestCase (assertEqual "test_findChain3" (findChain 'B' ".WWB.." 0) False),
                  TestCase (assertEqual "test_findChain4" (findChain 'B' "..." 0) False),
                  TestCase (assertEqual "test_findChain5" (findChain 'B' "W.." 0) False),

                  TestCase (assertEqual "test_findMovesInRow1" (findMovesInRow 'W' "...BW..." 0) [2]),
                  TestCase (assertEqual "test_findMovesInRow2" (findMovesInRow 'W' ".BW..BW." 0) [0,4]),
                  TestCase (assertEqual "test_findMovesInRow3" (findMovesInRow 'W' "...WB..." 0) []),

                  TestCase (assertEqual "test_dl"
                            (dl ["FGHIJK78",
                                 "EFGHIJK8",
                                 "DEFGHIJK",
                                 "CDEFGHIJ",
                                 "BCDEFGHI",
                                 "ABCDEFGH",
                                 "1ABCDEFG",
                                 "12ABCDEF"])
                            ["AAA","BBBB","CCCCC","DDDDDD",
                             "EEEEEEE","FFFFFFFF","GGGGGGG",
                             "HHHHHH","IIIII","JJJJ","KKK"]),
                  TestCase (assertEqual "test_dr"
                            (dr ["12ABCDEF",
                                 "1ABCDEFG",
                                 "ABCDEFGH",
                                 "BCDEFGHI",
                                 "CDEFGHIJ",
                                 "DEFGHIJK",
                                 "EFGHIJK8",
                                 "FGHIJK78"])
                            ["AAA","BBBB","CCCCC","DDDDDD",
                             "EEEEEEE","FFFFFFFF","GGGGGGG",
                             "HHHHHH","IIIII","JJJJ","KKK"]),

                  TestCase (assertEqual "makeSlice"
                            (makeSlice (0,0) (1,1))
                            [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]),

                  TestCase (assertEqual "findMoves"
                            (findMoves 'W' ["........",
                                            "........",
                                            "........",
                                            "...BW...",
                                            "...WB...",
                                            "........",
                                            "........",
                                            "........"])
                            [(2,3),(5,4),(3,2),(4,5)]),
                  TestCase (assertEqual "findMoves"
                            (findMoves 'B' ["........",
                                            "........",
                                            "...W....",
                                            "...WW...",
                                            "...WB...",
                                            "........",
                                            "........",
                                            "........"])
                            [(2,4),(4,2),
                             (2,2)]) ]
