module Main (main) where
  import System.Environment (getArgs)

  import Parse (parse)
  import Solution (solve)

  main :: IO ()
  main = do
    args  <- getArgs
    input <- parse (head args)

    print $ "Part 1: " ++ show (solve input)
