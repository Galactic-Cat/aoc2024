module Solution (Coordinate, Direction (East), Grid, Reindeer (Reindeer), solve, Tile (Floor, Goal, Wall)) where
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  
  type Coordinate = (Int, Int)
  data Direction = North | East | South | West
    deriving (Eq)
  type Grid = Map.Map Coordinate Tile
  data Reindeer = Reindeer Coordinate Direction
  data Tile = Floor | Goal | Wall

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
    | otherwise  = map (\c' a' d' -> search g (Reindeer c' d') a')

  solve :: IO ()
  solve = print "No solution"