module Solution (solve, solve2) where
  import qualified Data.Map as Map (Map, empty, insert, lookup)

  blinkAll :: Map.Map (Int, Int) Int -> Int -> [Int] -> Int
  blinkAll _ _ []     = 0
  blinkAll m c (x:xs) =
    let (v, m') = blink m c x
    in  v + blinkAll m' c xs

  blink :: Map.Map (Int, Int) Int -> Int -> Int -> (Int, Map.Map (Int, Int) Int)
  blink m 0 _ = (1, m)
  blink m c 0 = blink m (c - 1) 1
  blink m c x =
    case Map.lookup (c, x) m of
      Just v  -> (v, m)
      Nothing ->
        if   even $ countDigits x
        then let (a, b) = splitNumber x in
             let (va, ma) = blink m  (c - 1) a in
             let (vb, mb) = blink ma (c - 1) b
             in  (va + vb, Map.insert (c, x) (va + vb) mb)
        else let (v', m') = blink m (c - 1) (x * 2024)
             in  (v', Map.insert (c, x) v' m')

  countDigits :: Int -> Int
  countDigits 0 = 1
  countDigits x = length $ show x

  solve :: [Int] -> Int
  solve = blinkAll Map.empty 25

  solve2 :: [Int] -> Int
  solve2 = blinkAll Map.empty 75

  splitNumber :: Int -> (Int, Int)
  splitNumber i =
    let (a, b) = splitAt (length (show i) `div` 2) (show i)
    in  (read a, read b)