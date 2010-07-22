import System.Environment
import Data.List

type Player = Char
data Location = P | O | E deriving (Show,Eq)
-- P = player, O = other player, E = empty
type Line = [Location]

main = do
  args <- getArgs
  boardString <- readFile (head args)
  checkLines $ lines boardString
  
checkLines :: [String] -> IO [()]
checkLines lines = mapM putStrLn lines

stringToLine :: String -> Player -> Line
stringToLine line player = map (\a -> charToLocForPlayer a player) line

charToLocForPlayer :: Char -> Player -> Location
charToLocForPlayer c p = if c == p then P
                         else 
                           if c == '.' then
                             E else O

isMovePossible :: Line -> Bool
isMovePossible line = iMP line || iMP (reverse line)

iMP :: Line -> Bool
iMP line = 
  let trimmed = dropWhile (\a -> a == E) line in
  let grouped = group trimmed in
  let poe = concat $ map nub grouped in
  isInfixOf [P,O,E] poe

