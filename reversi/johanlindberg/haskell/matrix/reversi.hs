-- Reversi kata for langgeeks

findMove :: Char -> [String] -> Int 
findMove player board = 0

test_findMove :: Bool
test_findMove = findMove 'B' ["...BW..."] == 5