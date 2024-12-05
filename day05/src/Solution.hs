module Solution (solve, solve2, Rule (Rule)) where
  import Prelude hiding (filter)
  import Data.List (elemIndex)

  data Rule = Rule Int Int
    deriving Show

  filter :: [[Int]] -> Rule -> [[Int]]
  filter []     _ = []
  filter (x:xs) r
    | validateRule x r = x : filter xs r
    | otherwise        = filter xs r

  filters :: [[Int]] -> [Rule] -> [[Int]]
  filters = foldl filter

  middle :: [a] -> a
  middle xs = xs !! (length xs `div` 2)

  solve :: ([Rule], [[Int]]) -> Int
  solve (rs, xs) = sum (map middle (filters xs rs))

  solve2 :: ([Rule], [[Int]]) -> Int
  solve2 _ = 3

  validateRule :: [Int] -> Rule -> Bool
  validateRule xs (Rule a b) =
    case (a `elemIndex` xs, b `elemIndex` xs) of
      (Just ia, Just ib) -> ia < ib
      _                  -> True