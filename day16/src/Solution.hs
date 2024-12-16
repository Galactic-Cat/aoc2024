module Solution (Coordinate, Direction (East), Grid, Reindeer (Reindeer), solve, Tile (Floor, Goal, Wall)) where
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  
  type Coordinate = (Int, Int)
  data Direction = North | East | South | West
    deriving (Eq)
  type Grid = Map.Map Coordinate Tile
  data Reindeer = Reindeer Coordinate Direction
  data Tile = Floor | Goal | Wall

  best :: Ord a => Maybe a -> Maybe a -> Maybe a
  best Nothing     Nothing     = Nothing
  best a@(Just _)  Nothing     = a
  best Nothing     b@(Just _)  = b
  best a@(Just a') b@(Just b')
    | a' < b'   = a
    | otherwise = b

  fst3 :: (a, b, c) -> a
  fst3 (x, _, _) = x

  directions :: [Direction]
  directions = North : East : South : West : directions

  isGoal :: Grid -> Coordinate -> Bool
  isGoal g c =
    case Map.lookup c g of
      Just Goal -> True
      _         -> False

  move :: Coordinate -> Direction -> Coordinate
  move (x, y) North = (x, y - 1)
  move (x, y) East  = (x + 1, y)
  move (x, y) South = (x, y + 1)
  move (x, y) West  = (x - 1, y)

  navigate :: Grid -> Coordinate -> Set.Set Coordinate -> Direction -> [(Coordinate, Int, Direction)]
  navigate g c v d = checkTile (take 4 directions)
    where
      checkTile []      = []
      checkTile (d':ds) =
        let c' = move c d' in
          if   Set.member c' v
          then checkTile ds
          else case Map.lookup c' g of
            Just Wall -> checkTile ds
            Nothing   -> checkTile ds
            Just _    ->
              if   d' == d
              then (c',    1, d') : checkTile ds
              else (c', 1001, d') : checkTile ds

  search :: Grid -> Reindeer -> Set.Set Coordinate -> Int -> Int -> Maybe Int
  search g (Reindeer c d) v a m
    | a >= m     = Nothing
    | isGoal g c = Just a
    | otherwise  = search' (navigate g c v d)
    where
      search' :: [(Coordinate, Int, Direction)] -> Maybe Int
      search' []                = Nothing
      search' ((c', a', d'):xs) =
        let v' = Set.insert c' v in
        case search g (Reindeer c' d') v a

    

  solve :: IO ()
  solve = print "No solution"