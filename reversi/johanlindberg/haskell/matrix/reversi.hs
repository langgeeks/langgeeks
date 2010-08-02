-- Reversi kata for langgeeks

import Data.Maybe

findMove :: Char -> [String] -> [Int]
findMove player board = catMaybes [findMoveInRow player row 0 | row <- board] ++
                        reverseIndex (catMaybes [findMoveInRow player (reverse row) 0 | row <- board])

reverseIndex :: [Int] -> [Int]
reverseIndex [] = []
reverseIndex moves = map (\x -> 7 - x) moves

findMoveInRow :: Char -> String -> Int -> Maybe Int
findMoveInRow player [] pos = Nothing
findMoveInRow player (x:xs) pos = if x == '.' && findChain player xs 0
                                  then Just pos
                                  else findMoveInRow player xs (pos+1)

findChain :: Char -> String -> Int -> Bool
findChain player [] pos     = False
findChain player (x:xs) pos = if x /= player
                              then if x /= '.'
                                   then findChain player xs (pos + 1)
                                   else False
                              else pos /= 0

test_findChain :: Bool
test_findChain = findChain 'B' "WWB.." 0  == True  &&
                 findChain 'B' "B.." 0    == False &&
                 findChain 'B' ".WWB.." 0 == False &&
                 findChain 'B' "..." 0    == False &&
                 findChain 'B' "W.." 0    == False

test_findMoveInRow :: Bool
test_findMoveInRow = findMoveInRow 'W' "...BW..." 0 == Just 2

test_findMove :: Bool
test_findMove = findMove 'W' ["...BW...","...WB..."] == [2,5]