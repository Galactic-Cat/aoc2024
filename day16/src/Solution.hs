module Solution (Coordinate, Direction (East), Grid (Grid), Reindeer (Reindeer), solve, Tile (Floor, Goal)) where
  import qualified Data.Map as Map
  import qualified Data.Set as Set

  type Coordinate = (Int, Int)
  type Cost = Int
  data Direction = North | East | South | West
    deriving (Eq)
  data Grid = Grid (Map.Map Coordinate Tile) Int Int
  data Reindeer = Reindeer Coordinate Direction
  data Tile = Floor | Goal
  type Visited = Set.Set Coordinate

  directions :: [Direction]
  directions = [North, East, South, West]

  getOptions :: Grid -> Coordinate -> Direction -> Visited -> [(Coordinate, Direction, Cost)]
  getOptions g c d v = map getCost (filter checkTile neighbours)
    where
      getCost (c', d')
        | d' == d   = (c', d',    1)
        | otherwise = (c', d', 1001)

      checkTile (c', _) = case Map.lookup c' g of
        Just _  -> not (Set.member c' v)
        Nothing -> False

      neighbours = zip (map (move c) directions) directions

  move :: Coordinate -> Direction -> Coordinate
  move (x, y) North = (x, y - 1)
  move (x, y) East  = (x + 1, y)
  move (x, y) South = (x, y + 1)
  move (x, y) West  = (x - 1, y)

  solve :: (Grid, Coordinate, Coordinate) -> IO ()
  solve _ = print "No solution"