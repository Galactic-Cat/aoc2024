module Solution (solve, solve2, Grid, Row, Tile (Tile)) where
  import Data.List (groupBy, nub)
  import Data.Maybe (isJust)
  
  type Coordinate = (Int, Int)
  type Grid = [Row]
  type Row = [Tile]
  newtype Tile = Tile Int
  type Trail = [Coordinate]

  assert :: Maybe a -> a
  assert (Just x) = x
  assert _        = error "Failed to assert"

  countPeaks :: [Trail] -> Int
  countPeaks = length . nub . map last

  getTile :: Grid -> Coordinate -> Maybe Tile
  getTile g (x, y) =
    if   x >= 0 && y >= 0 && x < length (head g) && y < length g
    then Just $ (g !! y) !! x
    else Nothing

  findTrails :: Grid -> [Trail]
  findTrails g = findTrails' g 0
    where
      findTrails' []     _ = []
      findTrails' (r:rs) y = findTrailsRow r (0, y) ++ findTrails' rs (y + 1)
        where
          findTrailsRow :: Row -> Coordinate -> [Trail]
          findTrailsRow []          _       = []
          findTrailsRow (Tile 0:ts) (x, y') = trackTrail g (x, y') ++ findTrailsRow ts (x + 1, y')
          findTrailsRow (Tile _:ts) (x, y') = findTrailsRow ts (x + 1, y')

  lookAround :: Grid -> Coordinate -> [Coordinate]
  lookAround g c@(x, y) =
    case getTile g c of
      Just (Tile z) -> map assert $ filter isJust [look z (x + 1, y), look z (x, y + 1), look z (x - 1, y), look z (x, y - 1)]
      Nothing       -> []
    where
      look :: Int -> Coordinate -> Maybe Coordinate
      look h c' =
        case getTile g c' of
          Just (Tile h')
            | h' == h + 1 -> Just c'
            | otherwise   -> Nothing
          Nothing        -> Nothing

  groupTrail :: Trail -> Trail -> Bool
  groupTrail []           _            = False
  groupTrail _            []           = False
  groupTrail ((ax, ay):_) ((bx, by):_) = ax == bx && ay == by

  solve :: Grid -> Int
  solve g = sum $ map countPeaks $ groupBy groupTrail (findTrails g)

  solve2 :: Grid -> Int
  solve2 = length . findTrails

  trackTrail :: Grid -> Coordinate -> [Trail]
  trackTrail g c =
    case getTile g c of
      Just (Tile 9) -> [[c]]
      Just (Tile _) -> lookAround g c >>= map (c :) . trackTrail g
      Nothing       -> []