module Solution (solve, solve2) where
  solve :: [[Int]] -> Int
  solve = sum . map (boolToInt . isSafe)

  solve2 :: [[Int]] -> Int
  solve2 = sum . map (boolToInt . isSafeDampened)

  data Direction = Increase | Decrease

  boolToInt :: Bool -> Int
  boolToInt True  = 1
  boolToInt False = 0

  isMarginal :: Int -> Int -> Bool
  isMarginal x y = abs (x - y) <= 3

  isOrdered :: Int -> Int -> Direction -> Bool
  isOrdered x y Increase = x < y
  isOrdered x y Decrease = x > y

  isSafe :: [Int] -> Bool
  isSafe []  = True
  isSafe [_] = True
  isSafe xs  = isSafe' xs Increase || isSafe' xs Decrease

  isSafe' :: [Int] -> Direction -> Bool
  isSafe' []       _ = True
  isSafe' [_]      _ = True
  isSafe' (x:y:xs) d = isMarginal x y && isOrdered x y d && isSafe' (y:xs) d

  isSafeDampened :: [Int] -> Bool
  isSafeDampened []  = True
  isSafeDampened [_] = True
  isSafeDampened xs  = foldr ((||) . (\qs -> isSafe' qs Increase || isSafe' qs Decrease)) False (bruteforce xs)

  bruteforce :: [Int] -> [[Int]]
  bruteforce xs = bruteforce' xs (length xs)
    where
      bruteforce' :: [Int] -> Int -> [[Int]]
      bruteforce' zs 0 = [slice zs 0]
      bruteforce' zs i = slice zs i : bruteforce' zs (i - 1)
      slice :: [Int] -> Int -> [Int]
      slice []     _ = []
      slice (_:zs) 0 = zs
      slice (z:zs) i = z : slice zs (i - 1)