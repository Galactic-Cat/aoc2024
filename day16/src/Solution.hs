module Solution (Coordinate, Direction (East), Grid (Grid), Reindeer (Reindeer), solve, Tile (Floor, Goal)) where
  import qualified Data.Map as M
  import qualified Data.Set as S
  import qualified PriorityQueue as P
  
  type Coordinate = (Int, Int)
  type Cost = Int
  data Direction = North | East | South | West
    deriving (Eq)
  data Grid = Grid (M.Map Coordinate Tile) Int Int
  data Reindeer = Reindeer Coordinate Direction
  data Tile = Floor | Goal
  type Visited = S.Set Coordinate
  type Queue = P.PriorityQueue Int (Coordinate, Direction)

  directions :: [Direction]
  directions = [North, East, South, West]

  dijkstra :: Grid -> M.Map Coordinate (Cost, Coordinate) -> Visited -> Queue -> Maybe Coordinate
  dijkstra g h v q =
    let (c, d) = P.pop q in
      if   isGoal g c
      then Just c
      else let os = getOptions g c d in
        let (h', v', q') = foldr (\(ah, av, aq) -> dijkstraSingle g ah av aq) (h, v, q) os in
          dijkstra g h' v' q'

  getOptions :: Grid -> Coordinate -> Direction -> [(Coordinate, Direction, Cost)]
  getOptions (Grid g _ _) c d = map getCost (filter checkTile neighbours)
    where
      getCost (c', d')
        | d' == d   = (c', d',    1)
        | otherwise = (c', d', 1001)

      checkTile (c', _) = case M.lookup c' g of
        Just _  -> True
        Nothing -> False

      neighbours = zip (map (move c) directions) directions

  isGoal :: Grid -> Coordinate -> Bool
  isGoal (Grid g _ _) c = case M.lookup c g of
    Just Goal -> True
    _         -> False

  move :: Coordinate -> Direction -> Coordinate
  move (x, y) North = (x, y - 1)
  move (x, y) East  = (x + 1, y)
  move (x, y) South = (x, y + 1)
  move (x, y) West  = (x - 1, y)

  solve :: (Grid, Coordinate, Coordinate) -> IO ()
  solve _ = print "No solution"