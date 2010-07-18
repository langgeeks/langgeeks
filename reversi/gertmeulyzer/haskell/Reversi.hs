import System.Environment

main = do
  args <- getArgs
  mapM putStrLn args
