module Solution (solve, solve2) where
  import Prelude hiding (concat, filter)

  concat :: Int -> Int -> Int
  concat a b = read (show a ++ show b)

  filter :: [[Int]] -> [Int]
  filter []     = []
  filter (x:xs) =
    if   trySolve (head x) 0 (tail x)
    then head x : filter xs
    else filter xs

  filter2 :: [[Int]] -> [Int]
  filter2 []     = []
  filter2 (x:xs) =
    if   trySolve2 (head x) 0 (tail x)
    then head x : filter2 xs
    else filter2 xs

  trySolve :: Int -> Int -> [Int] -> Bool
  trySolve t a []     = t == a
  trySolve t 0 (x:xs) = trySolve t x       xs
  trySolve t a (x:xs) = trySolve t (a + x) xs || trySolve t (a * x) xs

  trySolve2 :: Int -> Int -> [Int] -> Bool
  trySolve2 t a []     = t == a
  trySolve2 t 0 (x:xs) = trySolve2 t x       xs
  trySolve2 t a (x:xs) = trySolve2 t (a + x) xs || trySolve2 t (a * x) xs || trySolve2 t (concat a x) xs

  solve :: [[Int]] -> Int
  solve = sum . filter

  solve2 :: [[Int]] -> Int
  solve2 = sum . filter2
