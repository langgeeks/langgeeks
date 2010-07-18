import System.Environment

main = do
  args <- getArgs
  boardString <- readFile (head args)
  putStrLn boardString
