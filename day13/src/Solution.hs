module Solution (Coordinate, Offset, solve, solve2) where
  type Coordinate = (Int, Int)
  type Offset = (Int, Int)
  type SystemInput = ((Rational, Rational), (Rational, Rational), (Rational, Rational))

  aCost :: Int
  aCost = 3

  bCost :: Int
  bCost = 1

  increasePrizes :: [(Offset, Offset, Coordinate)] -> [(Offset, Offset, Coordinate)]
  increasePrizes []             = []
  increasePrizes ((a, b, p):is) = (a, b, increaseTenQuadrillion p) : increasePrizes is
    where
      increaseTenQuadrillion (px, py) = (px + 10000000000000, py + 10000000000000)

  solve :: [(Offset, Offset, Coordinate)] -> Int
  solve []     = 0
  solve (i:is) =
    case system (toSystemInput i) of
      Just v  -> v + solve is
      Nothing -> solve is

  solve2 :: [(Offset, Offset, Coordinate)] -> Int
  solve2 = solve . increasePrizes

  system :: SystemInput -> Maybe Int
  system ((ax, ay), (bx, by), (px, py)) =
    let as = (bx * py - by * px) / (bx * ay - by * ax) in
    let bs = (ax * py - ay * px) / (ax * by - ay * bx) in
      if   isInt as && isInt bs
      then Just $ aCost * (round as :: Int) + bCost * (round bs :: Int)
      else Nothing
    where
      isInt x = x == fromInteger (round x)

  toRealPair :: (Int, Int) -> (Rational, Rational)
  toRealPair (a, b) = (fromIntegral a, fromIntegral b)

  toSystemInput :: (Offset, Offset, Coordinate) -> SystemInput
  toSystemInput (a, b, p) = (toRealPair a, toRealPair b, toRealPair p)
