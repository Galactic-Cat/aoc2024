module Main (main) where
  import System.Environment (getArgs)
  import Lib (parse, distances, quicksort, similarity)

  main :: IO ()
  main = do
    args <- getArgs

    (left, right) <- if   null args
                     then error "Please provide an input file path"
                     else parse $ head args

    print $ "Part 1: " ++ show (sum (distances (quicksort left) (quicksort right)))
    print $ "Part 2: " ++ show (similarity (quicksort left) (quicksort right))
