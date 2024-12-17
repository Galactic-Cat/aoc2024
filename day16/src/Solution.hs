module Solution (Coordinate, Direction (East), Grid (Grid), Reindeer (Reindeer), solve, Tile (Floor, Goal)) where
  import qualified Data.Map as M
  import qualified Data.Set as S
  import qualified PriorityQueue as P

  type Coordinate = (Int, Int)
  type Cost = Int
  data Direction = North | East | South | West
    deriving (Eq, Ord)
  data Grid = Grid (M.Map Coordinate Tile) Int Int
  data Reindeer = Reindeer Coordinate Direction
  data Tile = Floor | Goal
  type Visited = S.Set (Coordinate, Direction)
  type Queue = P.PriorityQueue Int (Coordinate, Direction)
  type History = M.Map Coordinate (Cost, Coordinate, Direction)

  directions :: [Direction]
  directions = [North, East, South, West]

  dijkstra :: Grid -> History -> Visited -> Queue -> Maybe (Coordinate, History)
  dijkstra g h v q =
    let (cd, q') = P.pop q in
      case cd of
        Just (c, d) ->
          if   isGoal g c
          then Just (c, h)
          else
            let os = getOptions g c d in
            let (h', v', q'') = foldr (\x (ah, av, aq) -> dijkstraSingle ah av aq c x) (h, v, q') os in
              dijkstra g h' v' q''
        Nothing -> Nothing

  dijkstraSingle :: History -> Visited -> Queue -> Coordinate -> (Coordinate, Direction, Cost) -> (History, Visited, Queue)
  dijkstraSingle h v q c (c', d, p)
    | S.member (c', d) v = (h, v, q)
    | otherwise          =
      let nv = S.insert (c', d) v  in
      let nq = P.push np (c', d) q in
        (nh, nv, nq)
    where
      nh = case M.lookup c' h of
        Just (op, _, _) ->
          if   np < op
          then M.update (\_ -> Just (np, c, d)) c' h
          else h
        Nothing         -> M.insert c' (np, c, d) h
      np = case M.lookup c h of
        Just (op, _, _) -> op + p
        Nothing         -> p


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

  solve :: (Grid, Coordinate, Coordinate) -> Int
  solve (g, s, _) =
    case dijkstra g M.empty S.empty (P.singletonAsc 0 (s, East)) of
      Just (x, h) ->
        case M.lookup x h of
          Just (p, _, _) -> p
          Nothing        -> error $ "Goal coordinate " ++ show x ++ " not found in history"
      Nothing     -> error "Didn't find any path"

  -- dijkstra g h v q