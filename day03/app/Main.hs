module Main (main) where
  import Parse (parse)
  import Solution (solve, solve2)
  import System.Environment (getArgs)

  main :: IO ()
  main =
    do
      args <- getArgs
      input <- parse (head args)

      print $ "Part 1: " ++ show (solve input)
      print $ "Part 2: " ++ show (solve2 input)
