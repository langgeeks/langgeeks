-- Reversi kata for langgeeks

findMove :: Char -> [String] -> [Int]
findMove player board = [findMoveInRow player row 0 | row <- board] 

findMoveInRow :: Char -> String -> Int -> Int
findMoveInRow player [] pos = -1
findMoveInRow player (x:xs) pos = if x == player
                                  then pos
                                  else findMoveInRow player xs pos+1

test_findMoveInRow :: Bool
test_findMoveInRow = findMoveInRow 'B' "...BW..." 0 == 5

test_findMove :: Bool
test_findMove = findMove 'B' ["...BW..."] == [5]