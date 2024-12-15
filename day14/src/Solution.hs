module Solution (solve) where
  import Prelude hiding (truncate)

  type Coordinate = (Int, Int)
  type Velocity = (Int, Int)
  type Robot = (Coordinate, Velocity)

  countQuadrants :: [Maybe Int] -> [Int]
  countQuadrants []           = [0, 0, 0, 0]
  countQuadrants (Just x :xs) = fset x (+ 1) (countQuadrants xs)
  countQuadrants (Nothing:xs) = countQuadrants xs

  fset :: Int -> (a -> a) -> [a] -> [a]
  fset _ _ []     = []
  fset 0 f (x:xs) = f x : xs
  fset i f (x:xs) = x : fset (i - 1) f xs

  getQuadrant :: Robot -> Maybe Int
  getQuadrant ((x, y), _)
    | x == halfGridWidth || y == halfGridHeight = Nothing
    | otherwise                                 =
    case (x < halfGridWidth, y < halfGridHeight) of
      (True,  True)  -> Just 0
      (False, True)  -> Just 1
      (True,  False) -> Just 2
      (False, False) -> Just 3

  gridHeight :: Int
  gridHeight = 103

  gridWidth :: Int
  gridWidth = 101

  halfGridHeight :: Int
  halfGridHeight = gridHeight `div` 2

  halfGridWidth :: Int
  halfGridWidth = gridWidth `div` 2

  move :: Coordinate -> Velocity -> Coordinate
  move (x, y) (w, h) =
    let (nx, ny) = (x + w, y + h) in
      (truncate nx gridWidth, truncate ny gridHeight)
    where
      truncate a b
        | a <  0    = b + a
        | a >= b    = a - b
        | otherwise = a

  moveRobot :: Robot -> Int -> Robot
  moveRobot r      0 = r
  moveRobot (c, v) i = moveRobot (move c v, v) (i - 1)

  solve :: [Robot] -> Int
  solve = product . countQuadrants . map (getQuadrant . flip moveRobot 100)