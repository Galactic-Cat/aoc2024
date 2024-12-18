module Main (main) where
  import System.Environment (getArgs)

  import Parse (parse)
  import Solution (solve, solve2)

  main :: IO ()
  main = do
    args  <- getArgs
    input <- parse (head args)
    
    print $ "Part 1: " ++ show (solve input)
    print $ "Part 2: " ++ show (solve2 input)
