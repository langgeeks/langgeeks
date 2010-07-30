-- Reversi kata for langgeeks

findMove :: Char -> [String] -> [Int]
findMove player board = [findMoveInRow player row 0 | row <- board] 

findMoveInRow :: Char -> String -> Int -> Int
findMoveInRow player [] pos = -1
findMoveInRow player (x:xs) pos = if x == '.' && findChain player xs 0 1
                                  then pos
                                  else findMoveInRow player xs (pos+1)

findChain :: Char -> String -> Int -> Int -> Bool
findChain player [] pos inc  = False
findChain player row pos inc = if (head row) /= player
                               then if (head row) /= '.'
                                    then findChain player (tail row) (pos + inc) inc
                                    else False
                               else pos /= 0

test_findChain :: Bool
test_findChain = findChain 'B' "WWB.." 0 1  == True  &&
                 findChain 'B' "B.." 0 1    == False &&
                 findChain 'B' ".WWB.." 0 1 == False &&
                 findChain 'B' "..." 0 1    == False &&
                 findChain 'B' "W.." 0 1    == False

test_findMoveInRow :: Bool
test_findMoveInRow = findMoveInRow 'W' "...BW..." 0 == 2

test_findMove :: Bool
test_findMove = findMove 'W' ["...BW...","...WB..."] == [2,5]