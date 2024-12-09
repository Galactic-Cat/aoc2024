module Main (main) where
  import System.Environment (getArgs)

  import Parse (parse)
  import Parse2 (parse2)
  import Solution (solve)
  import Solution2 (solve2)

  main :: IO ()
  main = do
    args   <- getArgs
    input  <- parse (head args)
    input2 <- parse2 (head args)

    print $ "Part 1: " ++ show (solve input)
    print $ "Part 2: " ++ show (solve2 input2)