import System.Environment
import Data.List

data Player = B | W
data Location = Black | White | Empty


main = do
  args <- getArgs
  boardString <- readFile (head args)
  checkLines $ lines boardString
  
checkLines :: [String] -> IO [()]
checkLines = mapM putStrLn



-- validMoveOnLine :: String -> String -> Bool
