{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib (parse, distances, quicksort, similarity) where
  import Data.List (partition)

  parse :: String -> IO ([Int], [Int])
  parse path = do
    contents <- readFile path

    let singles = map read . words $ contents

    return $ split singles [] []
    where
      split []     y z = (y, z)
      split (x:xs) y z
        | length y > length z = split xs y     (x:z)
        | otherwise           = split xs (x:y) z

  distances :: [Int] -> [Int] -> [Int]
  distances []     []     = []
  distances (l:ls) (r:rs) = abs (l - r) : distances ls rs

  quicksort :: [Int] -> [Int]
  quicksort []     = []
  quicksort [x]    = [x]
  quicksort (x:xs) = quicksort less ++ [x] ++ quicksort more
    where
      (less, more) = partition (< x) xs

  similarity :: [Int] -> [Int] -> Int
  similarity []     _  = 0
  similarity (l:ls) rs = l * occurances l rs + similarity ls rs
    where
      occurances _ []     = 0
      occurances x (y:ys)
        | x == y    = 1 + occurances x ys
        | otherwise = occurances x ys