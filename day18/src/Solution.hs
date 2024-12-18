module Solution (Coordinate, solve, solve2) where
  import Prelude hiding (min, max)
  import qualified Data.Map as M
  import qualified Data.Set as S

  import PriorityQueue as P

  type Coordinate = (Int, Int)
  type Cost = Int
  type History = M.Map Coordinate (Cost, Coordinate)
  type Queue = P.PriorityQueue Cost Coordinate
  type Space = S.Set Coordinate

  bisect :: [Coordinate] -> Int -> Int -> Int
  bisect cs min max
    | min + 1 == max = max
    | otherwise      =
      let bisection = min + ((max - min) `div` 2) in
      let s = rain (take bisection cs) (coordinates size size) in
      let h = dijkstra M.empty (P.singletonAsc 0 (0, 0)) s in
        case M.lookup (size, size) h of
          Just _ -> bisect cs bisection max
          _      -> bisect cs min       bisection

  coordinates :: Int -> Int -> Space
  coordinates w 0 = coordinatesLine w 0
  coordinates w h = S.union (coordinatesLine w h) (coordinates w (h - 1))

  coordinatesLine :: Int -> Int -> Space
  coordinatesLine 0 y = S.singleton (0, y)
  coordinatesLine x y = S.insert (x, y) (coordinatesLine (x - 1) y)

  dijkstra :: History -> Queue -> Space -> History
  dijkstra h q s =
    case P.pop q of
      (Nothing, _ ) -> h
      (Just c , q') ->
        let os = dijkstraOptions c s in
        let (h', q'', s') = foldl (dijkstraSingle c) (h, q', s) os in
          dijkstra h' q'' s'

  dijkstraOptions :: Coordinate -> Space -> [Coordinate]
  dijkstraOptions c s = filter (`S.member` s) (neighbours c)

  dijkstraSingle :: Coordinate -> (History, Queue, Space) -> Coordinate -> (History, Queue, Space)
  dijkstraSingle c (h, q, s) c' = case (M.lookup c h, M.lookup c' h) of
        (Nothing    , Nothing     ) -> (M.insert c' (1    , c) h, P.push 1       c' q, S.insert c' s)
        (Nothing    , Just _      ) -> error "Weird situation in dijkstraSingle"
        (Just (p, _), Nothing     ) -> (M.insert c' (p + 1, c) h, P.push (p + 1) c' q, S.insert c' s)
        (Just (p, _), Just (p', _)) ->
          if   p + 1 < p'
          then (M.update (\_ -> Just (p + 1, c)) c' h, P.push (p + 1) c' q, S.insert c' s)
          else (h                                    , q                  , S.insert c' s)

  neighbours :: Coordinate -> [Coordinate]
  neighbours (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

  rain :: [Coordinate] -> Space -> Space
  rain cs s = foldl (flip S.delete) s cs

  rainCount :: Int
  rainCount = 1024

  size :: Int
  size = 70

  solve :: [Coordinate] -> Int
  solve cs =
    let s = rain (take rainCount cs) (coordinates size size) in
    let h = dijkstra M.empty (P.singletonAsc 0 (0, 0)) s in
      case M.lookup (size, size) h of
        Just (p, _) -> p
        _           -> error "Solve found no way out"

  solve2 :: [Coordinate] -> Coordinate
  solve2 cs = 
    let b = bisect cs rainCount (length cs)
    in  cs !! (b - 1)