{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Solution (Coordinate, solve, solve2, Space) where
  import qualified Data.Map as M
  import qualified Data.Set as S

  import qualified PriorityQueue as P

  type Coordinate = (Int, Int)
  type Cost = Int
  type History = M.Map Coordinate (Cost, Coordinate)
  type Queue = P.PriorityQueue Cost Coordinate
  type Space = S.Set Coordinate

  assertList :: [Maybe a] -> [a]
  assertList []          = []
  assertList (Just x:xs) = x : assertList xs
  assertList (_     :xs) = assertList xs

  cheat :: (Coordinate, Cost) -> [(Coordinate, Cost)] -> Int -> [Int]
  cheat (c, p) s m =
    let s' = filter ((p <) . snd) s in
      assertList $ map cheat' s'
    where
      cheat' :: (Coordinate, Cost) -> Maybe Int
      cheat' (c', p') =
        let m' = manhattan c c' in
          if   (m' <= m) && (p + m' < p')
          then Just (p' - (p + m'))
          else Nothing

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

  manhattan :: Coordinate -> Coordinate -> Int
  manhattan (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

  neighbours :: Coordinate -> [Coordinate]
  neighbours (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

  solve :: (Space, Coordinate) -> Int
  solve (s, i) =
    let h  = dijkstra M.empty (P.singletonAsc 0 i) s in
    let hs = map (\(c, (p, _)) -> (c, p)) (M.toList h) in
    let cs = concatMap (\c -> cheat c hs 2) hs in
      length (filter (>= 100) cs)

  solve2 :: (Space, Coordinate) -> Int
  solve2 (s, i) =
    let h  = dijkstra M.empty (P.singletonAsc 0 i) s in
    let hs = map (\(c, (p, _)) -> (c, p)) (M.toList h) in
    let cs = concatMap (\c -> cheat c hs 20) hs in
      length (filter (>= 100) cs)