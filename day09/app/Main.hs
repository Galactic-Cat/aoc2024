module Main (main) where
  import System.Environment (getArgs)

  import Parse (parse)
  import Solution (solve)

  main :: IO ()
  main = do
    args <- getArgs
